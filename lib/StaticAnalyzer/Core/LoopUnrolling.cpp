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
  // if(!S)
  //  return false;
  return S && (isa<ForStmt>(S) || isa<WhileStmt>(S) || isa<DoStmt>(S));
}

static internal::Matcher<Stmt> simpleCondition(std::string BindName) {
  return binaryOperator(anyOf(hasOperatorName("<"), hasOperatorName(">"),
                              hasOperatorName("<="), hasOperatorName(">=")),
                        hasEitherOperand(ignoringParenImpCasts(declRefExpr(
                            to(varDecl(hasType(isInteger())).bind(BindName))))),
                        hasEitherOperand(integerLiteral()));
}

static internal::Matcher<Stmt> changeIntBoundNode(std::string NodeName) {
  return binaryOperator(
      anyOf(hasOperatorName("="), hasOperatorName("+="), hasOperatorName("/="),
            hasOperatorName("*="), hasOperatorName("-=")),
      hasLHS(ignoringParenImpCasts(
          declRefExpr(to(varDecl(equalsBoundNode(NodeName)))))));
}

static internal::Matcher<Stmt> callByRef(std::string NodeName) {
  return hasDescendant(callExpr(forEachArgumentWithParam(
      declRefExpr(hasDeclaration(equalsBoundNode(NodeName))),
      parmVarDecl(hasType(references(qualType(unless(isConstQualified()))))))));
}

static internal::Matcher<Stmt> getAddrTo(std::string NodeName) {
  return expr(unaryOperator(
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

bool shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx) {

  if (!isLoopStmt(LoopStmt))
    return false;

  // TODO: In cases of while and do..while statements the value of initVarName
  // should be checked to be known
  // TODO: Match the cases where the bound is not a concrete literal but an
  // integer with known value

  auto Matches = match(whileLoopMatcher(), *LoopStmt, ASTCtx);
  if (!Matches.empty())
    return true;

  Matches = match(doWhileLoopMatcher(), *LoopStmt, ASTCtx);
  if (!Matches.empty())
    return true;

  Matches = match(forLoopMatcher(), *LoopStmt, ASTCtx);
  return !Matches.empty();
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
    if (auto CallExp = dyn_cast<CallExpr>(S)) {
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