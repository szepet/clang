//===--- LoopUnrolling.cpp - Unroll loops -----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// This file contains functions which are used to decide if a loop worth to be
/// unrolled. Moreover, these functions manages the stack of loop which is
/// tracked by the ProgramState.
///
//===----------------------------------------------------------------------===//

#include "clang/Analysis/CFGStmtMap.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/AST/ParentMap.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/LoopUnrolling.h"
#include "llvm/ADT/Statistic.h"

using namespace clang;
using namespace ento;
using namespace clang::ast_matchers;

struct LoopState {
private:
  enum Kind { Normal, Unrolled } K;
  const Stmt *LoopStmt;
  const LocationContext *LCtx;
  LoopState(Kind InK, const Stmt *S, const LocationContext *L)
      : K(InK), LoopStmt(S), LCtx(L) {}

public:
  static LoopState getNormal(const Stmt *S, const LocationContext *L) {
    return LoopState(Normal, S, L);
  }
  static LoopState getUnrolled(const Stmt *S, const LocationContext *L) {
    return LoopState(Unrolled, S, L);
  }
  bool isUnrolled() const { return K == Unrolled; }
  const Stmt *getLoopStmt() const { return LoopStmt; }
  const LocationContext *getLocationContext() const { return LCtx; }
  bool operator==(const LoopState &X) const {
    return K == X.K && LoopStmt == X.LoopStmt;
  }
  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.AddInteger(K);
    ID.AddPointer(LoopStmt);
  }
};

// The tracked stack of loops. The stack indicates that which loops the
// simulated element contained by. The loops are marked depending if we decided
// to unroll them.
// TODO: The loop stack should not need to be in the program state since it is
// lexical in nature. Instead, the stack of loops should be tracked in the
// LocationContext.
REGISTER_LIST_WITH_PROGRAMSTATE(LoopStack, LoopState)

namespace clang {
namespace ento {

static void printLoopStack(ProgramStateRef State) {
  auto LS = State->get<LoopStack>();
  llvm::errs() << "LoopStack:\n";
  for (auto &E : LS) {
    llvm::errs() << E.getLoopStmt() << " " << E.isUnrolled() << " "
                 << E.getLocationContext() << "\n";
  }
  llvm::errs() << "\n";
  return;
}

static bool isLoopStmt(const Stmt *S) {
  return S && (isa<ForStmt>(S) || isa<WhileStmt>(S) || isa<DoStmt>(S));
}

ProgramStateRef processLoopEnd(const Stmt *LoopStmt, ProgramStateRef State) {
  auto LS = State->get<LoopStack>();
  assert(!LS.isEmpty() && "Loop not added to the stack.");
  assert(LoopStmt == LS.getHead().getLoopStmt() &&
         "Loop is not on top of the stack.");

  State = State->set<LoopStack>(LS.getTail());
  return State;
}

static internal::Matcher<Stmt> simpleCondition(StringRef BindName) {
  return binaryOperator(
      anyOf(hasOperatorName("<"), hasOperatorName(">"), hasOperatorName("<="),
            hasOperatorName(">="), hasOperatorName("!=")),
      hasEitherOperand(ignoringParenImpCasts(
          declRefExpr(to(varDecl(hasType(isInteger())).bind(BindName))))),
      hasEitherOperand(ignoringParenImpCasts(integerLiteral())));
}

static internal::Matcher<Stmt> changeIntBoundNode(StringRef NodeName) {
  return anyOf(hasDescendant(unaryOperator(
                   anyOf(hasOperatorName("--"), hasOperatorName("++")),
                   hasUnaryOperand(ignoringParenImpCasts(
                       declRefExpr(to(varDecl(equalsBoundNode(NodeName)))))))),
               hasDescendant(binaryOperator(
                   anyOf(hasOperatorName("="), hasOperatorName("+="),
                         hasOperatorName("/="), hasOperatorName("*="),
                         hasOperatorName("-=")),
                   hasLHS(ignoringParenImpCasts(
                       declRefExpr(to(varDecl(equalsBoundNode(NodeName)))))))));
}

static internal::Matcher<Stmt> callByRef(StringRef NodeName) {
  return hasDescendant(callExpr(forEachArgumentWithParam(
      declRefExpr(to(varDecl(equalsBoundNode(NodeName)))),
      parmVarDecl(hasType(references(qualType(unless(isConstQualified()))))))));
}

static internal::Matcher<Stmt> assignedToRef(StringRef NodeName) {
  return hasDescendant(varDecl(
      allOf(hasType(referenceType()),
            hasInitializer(
                anyOf(initListExpr(has(
                          declRefExpr(to(varDecl(equalsBoundNode(NodeName)))))),
                      declRefExpr(to(varDecl(equalsBoundNode(NodeName)))))))));
}

static internal::Matcher<Stmt> getAddrTo(StringRef NodeName) {
  return hasDescendant(unaryOperator(
      hasOperatorName("&"),
      hasUnaryOperand(declRefExpr(hasDeclaration(equalsBoundNode(NodeName))))));
}

static internal::Matcher<Stmt> hasSuspiciousStmt(StringRef NodeName) {
  return anyOf(hasDescendant(gotoStmt()), hasDescendant(switchStmt()),
               // Escaping and not known mutation of the loop counter is handled
               // by exclusion of assigning and address-of operators and
               // pass-by-ref function calls on the loop counter from the body.
               changeIntBoundNode(NodeName), callByRef(NodeName),
               getAddrTo(NodeName), assignedToRef(NodeName));
}

static internal::Matcher<Stmt> forLoopMatcher() {
  return forStmt(
             hasCondition(simpleCondition("initVarName")),
             // Initialization should match the form: 'int i = 6' or 'i = 42'.
             hasLoopInit(
                 anyOf(declStmt(hasSingleDecl(
                           varDecl(allOf(hasInitializer(integerLiteral()),
                                         equalsBoundNode("initVarName"))))),
                       binaryOperator(hasLHS(declRefExpr(to(varDecl(
                                          equalsBoundNode("initVarName"))))),
                                      hasRHS(integerLiteral())))),
             // Incrementation should be a simple increment or decrement
             // operator call.
             hasIncrement(unaryOperator(
                 anyOf(hasOperatorName("++"), hasOperatorName("--")),
                 hasUnaryOperand(declRefExpr(
                     to(varDecl(allOf(equalsBoundNode("initVarName"),
                                      hasType(isInteger())))))))),
             unless(hasBody(hasSuspiciousStmt("initVarName")))).bind("forLoop");
}

bool shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx) {

  if (!isLoopStmt(LoopStmt))
    return false;

  // TODO: Match the cases where the bound is not a concrete literal but an
  // integer with known value

  auto Matches = match(forLoopMatcher(), *LoopStmt, ASTCtx);
  return !Matches.empty();
}

ProgramStateRef updateLoopStack(const Stmt *LoopStmt, ASTContext &ASTCtx,
                                ExplodedNode* Pred) {
  auto State = Pred->getState();
  auto LCtx = Pred->getLocationContext();

  if (!isLoopStmt(LoopStmt))
    return State;

  auto LS = State->get<LoopStack>();
  if (!LS.isEmpty() && LoopStmt == LS.getHead().getLoopStmt() &&
      LCtx == LS.getHead().getLocationContext())
    return State;

  if (!shouldCompletelyUnroll(LoopStmt, ASTCtx)) {
    State = State->add<LoopStack>(LoopState::getNormal(LoopStmt, LCtx));
    return State;
  }

  State = State->add<LoopStack>(LoopState::getUnrolled(LoopStmt, LCtx));
  return State;
}

bool isUnrolledState(ProgramStateRef State) {
  auto LS = State->get<LoopStack>();
  if (LS.isEmpty() || !LS.getHead().isUnrolled())
    return false;
  return true;
}
}
}
