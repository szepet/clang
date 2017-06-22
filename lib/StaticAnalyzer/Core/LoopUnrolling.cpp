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
  return binaryOperator(anyOf(hasOperatorName("<"), hasOperatorName(">"),
                              hasOperatorName("<="), hasOperatorName(">=")),
                        hasEitherOperand(expr(ignoringParenImpCasts(declRefExpr(
                            to(varDecl(hasType(isInteger())).bind(BindName))))).bind("counterRef")),
                        hasEitherOperand(expr(unless(equalsBoundNode("counterRef"))).bind("DUDE2")));
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

static internal::Matcher<Stmt> collectBoundVars(StringRef VarName) {
  return declRefExpr(to(varDecl().bind(VarName)));
}

const Stmt* shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx, ExplodedNode* Pred) {

  if (!isLoopStmt(LoopStmt))
    return nullptr;

  // TODO: In cases of while and do..while statements the value of initVarName
  // should be checked to be known
  // TODO: Match the cases where the bound is not a concrete literal but an
  // integer with known value


  auto Matches = match(whileLoopMatcher(), *LoopStmt, ASTCtx);
  if (!Matches.empty())
    return nullptr;

  Matches = match(doWhileLoopMatcher(), *LoopStmt, ASTCtx);
  if (!Matches.empty())
    return nullptr;

  Matches = match(forLoopMatcher(), *LoopStmt, ASTCtx);
  if(!Matches.empty()) {
    const Stmt* asd =  Matches[0].getNodeAs<Stmt>("DUDE2");
    Pred->getState()->getSVal(asd,Pred->getLocationContext()).dump();
    return asd;
  }
  return nullptr;
}

QualType shouldCompletelyUnroll2(const Stmt *LoopStmt, ASTContext &ASTCtx) {

  if (!isLoopStmt(LoopStmt))
    return QualType();

  // TODO: In cases of while and do..while statements the value of initVarName
  // should be checked to be known
  // TODO: Match the cases where the bound is not a concrete literal but an
  // integer with known value

  auto Matches = match(whileLoopMatcher(), *LoopStmt, ASTCtx);
  if (!Matches.empty())
    //return true;
    return QualType();

  Matches = match(doWhileLoopMatcher(), *LoopStmt, ASTCtx);
  if (!Matches.empty())
    //return true;
    return QualType();

  Matches = match(forLoopMatcher(), *LoopStmt, ASTCtx);
  if(!Matches.empty()) {
    return Matches[0].getNodeAs<VarDecl>("initVarName")->getType();
  }
  return QualType();
  //return !Matches.empty();
}


bool isUnrolledLoopBlock(const CFGBlock *Block, ProgramStateRef State) {
  return State->contains<UnrolledLoopBlocks>(Block);
}

namespace {
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
    assert(StmtToBlockMap->getBlock(S));
    State = State->add<UnrolledLoopBlocks>(StmtToBlockMap->getBlock(S));

    // In case of function calls mark their CFGBlocks as well.
    auto CallExp = dyn_cast<CallExpr>(S);
    if (CallExp && CallExp->getCalleeDecl() &&
        CallExp->getCalleeDecl()->getBody()) {
      auto CalleeCFG = AMgr.getCFG(CallExp->getCalleeDecl());
      for (CFG::const_iterator BlockIt = CalleeCFG->begin();
           BlockIt != CalleeCFG->end(); BlockIt++) {
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
}

ProgramStateRef markBlocksAsUnrolled(const Stmt *Term, ProgramStateRef State,
                                     AnalysisManager &AMgr,
                                     CFGStmtMap *StmtToBlockMap) {
  if (State->contains<UnrolledLoops>(Term))
    return State;

  State = State->add<UnrolledLoops>(Term);
  ++NumTimesLoopUnrolled;
  LoopVisitor LV(State, AMgr, StmtToBlockMap, Term);
  LV.Visit(Term);
  return LV.getState();
}

}
}
