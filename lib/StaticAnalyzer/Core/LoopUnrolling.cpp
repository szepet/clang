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
/// unrolled. Moreover contains function which mark the CFGBlocks which belongs
/// to the unrolled loop and store them in ProgramState.
///
//===----------------------------------------------------------------------===//

#include "clang/Analysis/CFGStmtMap.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/AST/ParentMap.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/LoopUnrolling.h"
#include "llvm/ADT/Statistic.h"

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
  return S && (isa<ForStmt>(S) || isa<WhileStmt>(S) || isa<DoStmt>(S));
}

static internal::Matcher<Stmt> simpleCondition(StringRef BindName) {
  return binaryOperator(
      anyOf(hasOperatorName("<"), hasOperatorName(">"), hasOperatorName("<="),
            hasOperatorName(">="), hasOperatorName("!=")),
      hasEitherOperand(expr(ignoringParenImpCasts(declRefExpr(to(
                                varDecl(hasType(isInteger())).bind(BindName)))))
                           .bind("CounterExp")),
      hasEitherOperand(
          expr(unless(equalsBoundNode("CounterExp"))).bind("BoundExp")));
}

static internal::Matcher<Stmt> changeIntBoundNode(StringRef NodeName) {
  return hasDescendant(binaryOperator(
      anyOf(hasOperatorName("="), hasOperatorName("+="), hasOperatorName("/="),
            hasOperatorName("*="), hasOperatorName("-=")),
      hasLHS(ignoringParenImpCasts(
          declRefExpr(to(varDecl(equalsBoundNode(NodeName))))))));
}

static internal::Matcher<Stmt> callByRef(StringRef NodeName) {
  return hasDescendant(callExpr(forEachArgumentWithParam(
      declRefExpr(hasDeclaration(equalsBoundNode(NodeName))),
      parmVarDecl(hasType(references(qualType(unless(isConstQualified()))))))));
}

static internal::Matcher<Stmt> getAddrTo(StringRef NodeName) {
  return hasDescendant(unaryOperator(
      hasOperatorName("&"),
      hasUnaryOperand(declRefExpr(hasDeclaration(equalsBoundNode(NodeName))))));
}

static internal::Matcher<Stmt> forLoopMatcher() {
  return forStmt(
             hasCondition(simpleCondition("initVarName")),
             // Initialization should match the form: 'int i = 6' or 'i = 7'.
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
             // Escaping and not known mutation of the loop counter is handled
             // by exclusion of assigning and address-of operators and
             // pass-by-ref function calls on the loop counter from the body.
             unless(hasBody(anyOf(changeIntBoundNode("initVarName"),
                                  callByRef("initVarName"),
                                  getAddrTo("initVarName"))))).bind("forLoop");
}

static internal::Matcher<Stmt> whileLoopMatcher() {
  return whileStmt(hasCondition(simpleCondition("initVarName")),
                   hasBody(hasDescendant(unaryOperator(
                       anyOf(hasOperatorName("++"), hasOperatorName("--")),
                       hasUnaryOperand(declRefExpr(
                           to(varDecl(allOf(equalsBoundNode("initVarName"),
                                            hasType(isInteger()))))))))),
                   unless(hasBody(anyOf(changeIntBoundNode("initVarName"),
                                        callByRef("initVarName"),
                                        getAddrTo("initVarName")))))
      .bind("whileLoop");
}

static internal::Matcher<Stmt> doWhileLoopMatcher() {
  return doStmt(hasCondition(simpleCondition("initVarName")),
                hasBody(hasDescendant(unaryOperator(
                    anyOf(hasOperatorName("++"), hasOperatorName("--")),
                    hasUnaryOperand(declRefExpr(
                        to(varDecl(allOf(equalsBoundNode("initVarName"),
                                         hasType(isInteger()))))))))),
                unless(hasBody(anyOf(
                    changeIntBoundNode("initVarName"), callByRef("initVarName"),
                    getAddrTo("initVarName"))))).bind("whileLoop");
}

static internal::Matcher<Stmt> loopMatcher() {
  return anyOf(doWhileLoopMatcher(), whileLoopMatcher(), forLoopMatcher());
}

bool shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx,
                            ExplodedNode *Pred, SValBuilder &SVB) {

  if (!isLoopStmt(LoopStmt))
    return false;

  // TODO: Check for possibilities of changing the bound expression.

  auto Matches = match(loopMatcher(), *LoopStmt, ASTCtx);
  if (Matches.empty())
    return false;

  const Expr *BoundExp = Matches[0].getNodeAs<Expr>("BoundExp");
  const Expr *CounterExp = Matches[0].getNodeAs<Expr>("CounterExp");
  auto State = Pred->getState();
  auto LCtx = Pred->getLocationContext();
  SVal BoundVal = State->getSVal(BoundExp, LCtx);
  SVal CounterVal = State->getSVal(CounterExp, LCtx);
  if (SVB.getKnownValue(State, BoundVal))
    llvm::errs() << "BoundVal: " << *SVB.getKnownValue(State, BoundVal) << "\n";
  if (SVB.getKnownValue(State, CounterVal))
    llvm::errs() << "CounterVal: " << *SVB.getKnownValue(State, CounterVal)
                 << "\n";

  if (!SVB.getKnownValue(State, BoundVal) ||
      !SVB.getKnownValue(State, CounterVal))
    return false;

  return true;
}

namespace {
class LoopVisitor : public ConstStmtVisitor<LoopVisitor> {
public:
  LoopVisitor(ProgramStateRef St, AnalysisManager &AMgr, CFGStmtMap *M)
      : State(St), AMgr(AMgr), StmtToBlockMap(M), Found(false) {}

  void VisitChildren(const Stmt *S) {
    for (const Stmt *Child : S->children())
      if (Child)
        Visit(Child);
  }

  void VisitStmt(const Stmt *S) {
    if (!S || (isLoopStmt(S) && S != LoopStmt) || Found)
      return;
    assert(StmtToBlockMap->getBlock(S));
    if (StmtToBlockMap->getBlock(S) == SearchedBlock) {
      Found = true;
      return;
    }

    // In case of function calls investigate their CFGBlocks as well.
    auto CallExp = dyn_cast<CallExpr>(S);
    if (CallExp && CallExp->getCalleeDecl() &&
        CallExp->getCalleeDecl()->getBody()) {
      auto CalleeCFG = AMgr.getCFG(CallExp->getCalleeDecl());
      for (auto &Block : *CalleeCFG) {
        if (Block == SearchedBlock) {
          Found = true;
          return;
        }
      }
    }
    VisitChildren(S);
  }

  bool isBlockOfLoop(const CFGBlock *B, const Stmt *Loop) {
    LoopStmt = Loop;
    SearchedBlock = B;
    Visit(LoopStmt);
    return Found;
  }

private:
  ProgramStateRef State;
  AnalysisManager &AMgr;
  CFGStmtMap *StmtToBlockMap;
  const Stmt *LoopStmt;
  bool Found;
  const CFGBlock *SearchedBlock;
};
}

bool isUnrolledLoopBlock(const CFGBlock *Block, ProgramStateRef State,
                         AnalysisManager &AMgr, CFGStmtMap *StmtToBlockMap) {

  for (auto Term : State->get<UnrolledLoops>()) {
    LoopVisitor LV(State, AMgr, StmtToBlockMap);
    if (LV.isBlockOfLoop(Block, Term))
      return true;
  }
  return false;
}

ProgramStateRef markLoopAsUnrolled(const Stmt *Term, ProgramStateRef State) {
  if (State->contains<UnrolledLoops>(Term))
    return State;

  State = State->add<UnrolledLoops>(Term);
  ++NumTimesLoopUnrolled;
  return State;
}
}
}
