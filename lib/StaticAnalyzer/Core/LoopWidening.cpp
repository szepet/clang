//===--- LoopWidening.cpp - Widen loops -------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// This file contains functions which are used to widen loops. A loop may be
/// widened to approximate the exit state(s), without analyzing every
/// iteration. The widening is done by invalidating anything which might be
/// modified by the body of the loop.
///
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Core/PathSensitive/LoopWidening.h"
#include "clang/AST/AST.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExplodedGraph.h"
#include "llvm/ADT/SmallSet.h"

using namespace clang;
using namespace ento;
using namespace clang::ast_matchers;

/// Return the loops condition Stmt or NULL if LoopStmt is not a loop
static const Expr *getLoopCondition(const Stmt *LoopStmt) {
  switch (LoopStmt->getStmtClass()) {
  default:
    return nullptr;
  case Stmt::ForStmtClass:
    return cast<ForStmt>(LoopStmt)->getCond();
  case Stmt::WhileStmtClass:
    return cast<WhileStmt>(LoopStmt)->getCond();
  case Stmt::DoStmtClass:
    return cast<DoStmt>(LoopStmt)->getCond();
  }
}

static internal::Matcher<Stmt> callByRef(StringRef NodeName) {
  return callExpr(forEachArgumentWithParam(
      expr().bind(NodeName),
      parmVarDecl(hasType(references(qualType(unless(isConstQualified())))))));
}

static internal::Matcher<Stmt> cxxNonConstCall(StringRef NodeName) {
  return anyOf(cxxMemberCallExpr(on(expr().bind("changedExpr")),
                                 unless(callee(cxxMethodDecl(isConst())))),
               cxxOperatorCallExpr(
                   hasArgument(0, ignoringImpCasts(expr().bind(NodeName))),
                   unless(callee(cxxMethodDecl(isConst())))));
}
static internal::Matcher<Stmt> changedByAssignement(StringRef NodeName) {
  return binaryOperator(
      anyOf(hasOperatorName("="), hasOperatorName("+="), hasOperatorName("/="),
            hasOperatorName("*="), hasOperatorName("-="), hasOperatorName("%="),
            hasOperatorName("&="), hasOperatorName("|="), hasOperatorName("^="),
            hasOperatorName("<<="), hasOperatorName(">>=")),
      hasLHS(ignoringParenImpCasts(expr().bind(NodeName))));
}
static internal::Matcher<Stmt> changedByIncrement(StringRef NodeName) {
  return unaryOperator(
      anyOf(hasOperatorName("--"), hasOperatorName("++")),
      hasUnaryOperand(ignoringParenImpCasts(expr().bind(NodeName))));
}

static internal::Matcher<Stmt> changeVariable() {
  return anyOf(changedByIncrement("changedExpr"),
               changedByAssignement("changedExpr"), callByRef("changedExpr"),
               cxxNonConstCall("changedExpr"));
}

namespace clang {
namespace ento {

bool collectRegion(const Expr *E,
                   llvm::SmallSet<const MemRegion *, 16> &RegionsToInvalidate,
                   ProgramStateRef State, const LocationContext *LCtx) {
  const VarDecl *VD = nullptr;
  while (true) {
    E = E->IgnoreParenImpCasts();
    switch (E->getStmtClass()) {
    case Stmt::DeclRefExprClass:
      VD = dyn_cast<VarDecl>(cast<DeclRefExpr>(E)->getDecl());
      if (!VD || VD->getType()->isAnyPointerType() ||
          VD->getType()->isReferenceType())
        return false;
      RegionsToInvalidate.insert(State->getLValue(VD, LCtx).getAsRegion());
      return true;
    case Stmt::MemberExprClass:
      E = cast<MemberExpr>(E)->getBase();
      break;
    case Stmt::CXXDependentScopeMemberExprClass:
      E = cast<CXXDependentScopeMemberExpr>(E)->getBase();
      break;
    case Stmt::ArraySubscriptExprClass:
      E = cast<ArraySubscriptExpr>(E)->getBase();
      break;
    default:
      return false;
    }
  }
}

ProgramStateRef getConservativelyWidenedLoopState(ProgramStateRef State,
                                                  ASTContext &ASTCtx,
                                                  const LocationContext *LCtx,
                                                  unsigned BlockCount,
                                                  const Stmt *LoopStmt) {

  llvm::SmallSet<const MemRegion *, 16> RegionsToInvalidate;
  auto Matches = match(findAll(changeVariable()), *LoopStmt, ASTCtx);
  for (auto &Match : Matches) {
    auto E = Match.getNodeAs<Expr>("changedExpr");
    bool Success = collectRegion(E, RegionsToInvalidate, State, LCtx);
    if (!Success)
      return State;
  }
  llvm::SmallVector<const MemRegion *, 16> Regions;
  Regions.reserve(RegionsToInvalidate.size());
  for (auto E : RegionsToInvalidate)
    Regions.push_back(E);

  return State->invalidateRegions(llvm::makeArrayRef(Regions),
                                  getLoopCondition(LoopStmt), BlockCount, LCtx,
                                  true);
}

ProgramStateRef getWidenedLoopState(ProgramStateRef PrevState,
                                    const LocationContext *LCtx,
                                    unsigned BlockCount, const Stmt *LoopStmt) {

  assert(isa<ForStmt>(LoopStmt) || isa<WhileStmt>(LoopStmt) ||
         isa<DoStmt>(LoopStmt));

  // Invalidate values in the current state.
  // TODO Make this more conservative by only invalidating values that might
  //      be modified by the body of the loop.
  // TODO Nested loops are currently widened as a result of the invalidation
  //      being so inprecise. When the invalidation is improved, the handling
  //      of nested loops will also need to be improved.
  const StackFrameContext *STC = LCtx->getCurrentStackFrame();
  MemRegionManager &MRMgr = PrevState->getStateManager().getRegionManager();
  const MemRegion *Regions[] = {MRMgr.getStackLocalsRegion(STC),
                                MRMgr.getStackArgumentsRegion(STC),
                                MRMgr.getGlobalsRegion()};
  RegionAndSymbolInvalidationTraits ITraits;
  for (auto *Region : Regions) {
    ITraits.setTrait(Region,
                     RegionAndSymbolInvalidationTraits::TK_EntireMemSpace);
  }
  return PrevState->invalidateRegions(Regions, getLoopCondition(LoopStmt),
                                      BlockCount, LCtx, true, nullptr, nullptr,
                                      &ITraits);
}

} // end namespace ento
} // end namespace clang
