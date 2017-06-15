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
#include <utility>
using namespace clang;
using namespace ento;
using namespace clang::ast_matchers;

#define DEBUG_TYPE "LoopUnrolling"

STATISTIC(NumTimesLoopUnrolled,
          "The # of times a loop has got completely unrolled");

REGISTER_SET_WITH_PROGRAMSTATE(UnrolledLoops, const Stmt *)
REGISTER_SET_WITH_PROGRAMSTATE(UnrolledLoopBlocks, const CFGBlock *)

namespace clang {
namespace ento {

static bool isLoopStmt(const Stmt *S) {
  // if(!S)
  //  return false;
  return S && (isa<ForStmt>(S) || isa<WhileStmt>(S) || isa<DoStmt>(S));
}

class LoopVisitor : public ConstStmtVisitor<LoopVisitor> {
public:
  LoopVisitor(ProgramStateRef St, AnalysisManager &AMgr, CFGStmtMap *M,
              const Stmt *Term)
      : State(St), AMgr(AMgr), StmtToBlockMap(M), LoopStmt(Term) {}

  void VisitChildren(const Stmt *S) {
    for (const Stmt *Child : S->children())
      if (Child)
        Visit(Child);
  }

  void VisitStmt(const Stmt *S) {
    if (!S || (isLoopStmt(S) && S != LoopStmt))
      return;

    if (StmtToBlockMap->getBlock(S))
      State = State->add<UnrolledLoopBlocks>(StmtToBlockMap->getBlock(S));
    if (auto CallExp = dyn_cast<CallExpr>(S)) {
      auto CalleeCFG = AMgr.getCFG(CallExp->getCalleeDecl());
      for (CFG::const_iterator BlockIt = CalleeCFG->begin();
           BlockIt != CalleeCFG->end(); BlockIt++) {
        if (*BlockIt)
          State = State->add<UnrolledLoopBlocks>(*BlockIt);
      }
    }
    VisitChildren(S);
  }

  ProgramStateRef getState() { return State; }

private:
  ProgramStateRef State;
  AnalysisManager &AMgr;
  CFGStmtMap *StmtToBlockMap;
  const Stmt *LoopStmt;
};

static internal::Matcher<Stmt> forLoopMatcher() {
  return forStmt(
             hasLoopInit(anyOf(
                 declStmt(
                     hasSingleDecl(varDecl(hasInitializer(integerLiteral()))
                                       .bind("initVarName"))),
                 binaryOperator(
                     hasLHS(declRefExpr(to(varDecl().bind("initVarName")))),
                     hasRHS(integerLiteral())))),
             hasIncrement(
                 unaryOperator(hasOperatorName("++"),
                               hasUnaryOperand(declRefExpr(to(
                                   varDecl(allOf(equalsBoundNode("initVarName"),
                                                 hasType(isInteger())))))))),
             hasCondition(binaryOperator(
                 anyOf(hasOperatorName("<"), hasOperatorName(">"),
                       hasOperatorName("<="), hasOperatorName(">=")),
                 hasLHS(ignoringParenImpCasts(declRefExpr(to(varDecl(allOf(
                     equalsBoundNode("initVarName"), hasType(isInteger()))))))),
                 hasRHS(integerLiteral().bind("bound")))),
             unless(hasBody(anyOf(
                 hasDescendant(callExpr(forEachArgumentWithParam(
                     declRefExpr(
                         hasDeclaration(equalsBoundNode("initVarName"))),
                     parmVarDecl(hasType(
                         references(qualType(unless(isConstQualified())))))))),
                 hasDescendant(expr(unaryOperator(
                     hasOperatorName("&"),
                     hasUnaryOperand(declRefExpr(hasDeclaration(equalsBoundNode(
                         "initVarName"))))))))))).bind("forLoop");
}

static internal::Matcher<Stmt> whileLoopMatcher() {
  return whileStmt(
             hasCondition(binaryOperator(
                 anyOf(hasOperatorName("<"), hasOperatorName(">"),
                       hasOperatorName("<="), hasOperatorName(">=")),
                 hasLHS(ignoringParenImpCasts(declRefExpr(
                     to(varDecl(hasType(isInteger())).bind("initVarName"))))),
                 hasRHS(integerLiteral().bind("bound")))),
             unless(hasBody(anyOf(
                 hasDescendant(callExpr(forEachArgumentWithParam(
                     declRefExpr(
                         hasDeclaration(equalsBoundNode("initVarName"))),
                     parmVarDecl(hasType(
                         references(qualType(unless(isConstQualified())))))))),
                 hasDescendant(expr(unaryOperator(
                     hasOperatorName("&"),
                     hasUnaryOperand(declRefExpr(hasDeclaration(equalsBoundNode(
                         "initVarName"))))))))))).bind("whileLoop");
}

bool shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx) {

  if (!isLoopStmt(LoopStmt))
    return false;

  MatchFinder FindLoop;
  auto Matches = match(whileLoopMatcher(), *LoopStmt, ASTCtx);
  if(!Matches.empty())
    return true;
  Matches = match(forLoopMatcher(), *LoopStmt, ASTCtx);
  return !Matches.empty();
}

bool isUnrolledLoopBlock(const CFGBlock *Block, ProgramStateRef State) {
  return State->contains<UnrolledLoopBlocks>(Block);
}

ProgramStateRef markBlocksAsUnrolled(const Stmt *Term, ProgramStateRef State,
                                     AnalysisManager &AMgr,
                                     CFGStmtMap *StmtToBlockMap) {
  if (State->contains<UnrolledLoops>(Term))
    return State;

  State = State->add<UnrolledLoops>(Term);
  NumTimesLoopUnrolled++;
  LoopVisitor LV(State, AMgr, StmtToBlockMap, Term);
  LV.Visit(Term);
  return LV.getState();
}

void stateTesting(ProgramStateRef State, std::string ErrorString) {
  llvm::errs() << "STATE TEST " << ErrorString << "\n";
  UnrolledLoopBlocksTy ULB = State->get<UnrolledLoopBlocks>();
  for (const UnrolledLoopBlocksTy::value_type &E : ULB) {
    llvm::errs() << E << "  " << ErrorString << "\n";
  }
}
}
}