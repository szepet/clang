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
/// unrolled.
///
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Core/PathSensitive/LoopUnrolling.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugReporter.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CoreEngine.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramStateTrait.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SubEngine.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExprEngine.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/AST/ParentMap.h"
#include "clang/Analysis/CFGStmtMap.h"
#include "clang/Basic/Builtins.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "llvm/ADT/Statistic.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugType.h"
#include "clang/AST/StmtVisitor.h"
#include "PrettyStackTraceLocationContext.h"

using namespace clang;
using namespace ento;
using namespace clang::ast_matchers;

#define DEBUG_TYPE "LoopUnrolling"

STATISTIC(NumTimesLoopUnrolled,
          "The # of times a loop is got completely unrolled");

REGISTER_MAP_WITH_PROGRAMSTATE(UnrolledLoopBlocks, const Stmt *,
                               llvm::ImmutableSet<const CFGBlock *>)
namespace clang {
namespace ento {
class LoopVisitor : public ConstStmtVisitor<LoopVisitor> {
public:
  LoopVisitor(ProgramStateRef State, AnalysisManager &AMgr, CFGStmtMap *M,const Stmt* Term )
      : State(State), AMgr(AMgr), StmtToBlockMap(M), LoopStmt(Term) {}

  void VisitChildren(const Stmt *S) {
    for (const Stmt *Child : S->children())
      if (Child)
        Visit(Child);
  }

  void VisitStmt(const Stmt *S) {
    if (!S || (isa<ForStmt>(S) && !State->contains<UnrolledLoopBlocks>(S)))
      return;
    llvm::ImmutableSet<const CFGBlock *> BlockSet = *State->get<UnrolledLoopBlocks>(LoopStmt);
    if(StmtToBlockMap->getBlock(S))
    BlockSet = F.add(BlockSet, StmtToBlockMap->getBlock(S));
    if (auto CallExp = dyn_cast<CallExpr>(S)) {
      auto CalleeCFG = AMgr.getCFG(CallExp->getCalleeDecl());
      for (CFG::const_iterator BlockIt = CalleeCFG->begin();
           BlockIt != CalleeCFG->end(); BlockIt++) {
        if(*BlockIt)
        BlockSet = F.add(BlockSet, *BlockIt);
      }
    }
    State = State->set<UnrolledLoopBlocks>(LoopStmt, BlockSet);
    VisitChildren(S);
  }

  ProgramStateRef getState() { return State; }

private:
  ProgramStateRef State;
  AnalysisManager &AMgr;
  CFGStmtMap *StmtToBlockMap;
  const Stmt* LoopStmt;
  llvm::ImmutableSet<const CFGBlock *>::Factory F;
};

bool shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx) {
  auto LoopMatcher =
      forStmt(
          hasLoopInit(anyOf(
              declStmt(hasSingleDecl(varDecl(hasInitializer(integerLiteral()))
                                         .bind("initVarName"))),
              binaryOperator(
                  hasLHS(declRefExpr(to(varDecl().bind("initVarName")))),
                  hasRHS(integerLiteral())))),
          hasIncrement(unaryOperator(
              hasOperatorName("++"),
              hasUnaryOperand(declRefExpr(to(varDecl(allOf(
                  equalsBoundNode("initVarName"), hasType(isInteger())))))))),
          hasCondition(binaryOperator(
              anyOf(hasOperatorName("<"), hasOperatorName(">"),
                    hasOperatorName("<="), hasOperatorName(">=")),
              hasLHS(ignoringParenImpCasts(declRefExpr(to(varDecl(allOf(
                  equalsBoundNode("initVarName"), hasType(isInteger()))))))),
              hasRHS(integerLiteral().bind("bound")))),
          unless(hasBody(anyOf(
              hasDescendant(callExpr(forEachArgumentWithParam(
                  declRefExpr(hasDeclaration(equalsBoundNode("initVarName"))),
                  parmVarDecl(hasType(
                      references(qualType(unless(isConstQualified())))))))),
              hasDescendant(expr(unaryOperator(
                  hasOperatorName("&"),
                  hasUnaryOperand(declRefExpr(hasDeclaration(
                      equalsBoundNode("initVarName"))))))))))).bind("forLoop");

  if (dyn_cast_or_null<ForStmt>(LoopStmt)) {
    auto Matches = match(LoopMatcher, *LoopStmt, ASTCtx);
    if (Matches.empty())
      return false;
    return true;
  }
  return false;
}

bool isUnrolledLoopBlock(ProgramStateRef State, const CFGBlock *Block) {
  UnrolledLoopBlocksTy ULB = State->get<UnrolledLoopBlocks>();
  for (const UnrolledLoopBlocksTy::value_type E : ULB) {
    llvm::errs() << E.first << " ";
    E.first->dump();
    //for(auto asd : E.second)
    //  llvm::errs() << asd << "   Dude\n";
    //if (E.second.contains(Block))
    //  return true;
  }
  return false;
}

ProgramStateRef markBlocksAsUnrolled(ProgramStateRef State,
                                     AnalysisManager &AMgr,
                                     CFGStmtMap *StmtToBlockMap,
                                     const Stmt *Term) {

  if (!State->contains<UnrolledLoopBlocks>(Term)) {
    llvm::ImmutableSet<const CFGBlock *>::Factory F;
    auto newState = State->set<UnrolledLoopBlocks>(Term, F.getEmptySet());
    LoopVisitor LV(newState, AMgr, StmtToBlockMap, Term);
    LV.Visit(Term);

    int Cnt = 0;
    for (auto E : LV.getState()->get<UnrolledLoopBlocks>()) {
      (void) E;
      Cnt++;
    }
    NumTimesLoopUnrolled = Cnt;

    return LV.getState();
  }
  return State;
}

void stateTesting(ProgramStateRef State, std::string ErrorString){
  llvm::errs() << "STATE TEST " << ErrorString << "\n";
  UnrolledLoopBlocksTy ULB = State->get<UnrolledLoopBlocks>();
  for (const UnrolledLoopBlocksTy::value_type E : ULB) {
    for (auto Block : E.second)
      llvm::errs() << Block << "  " << ErrorString << "\n";
  }
}

}
}