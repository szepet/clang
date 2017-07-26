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
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/LoopUnrolling.h"
#include "llvm/ADT/Statistic.h"

using namespace clang;
using namespace ento;
using namespace clang::ast_matchers;

#define DEBUG_TYPE "LoopUnrolling"

STATISTIC(NumTimesLoopUnrolled,
          "The # of times a loop has got completely unrolled");

REGISTER_LIST_WITH_PROGRAMSTATE(MaxBlockVisitStack, const llvm::APInt)
REGISTER_LIST_WITH_PROGRAMSTATE(UnrolledLoopStack, const Stmt *)

namespace clang {
namespace ento {

static bool isLoopStmt(const Stmt *S) {
  return S && (isa<ForStmt>(S) || isa<WhileStmt>(S) || isa<DoStmt>(S));
}

ProgramStateRef processLoopEnd(const Stmt *LoopStmt, ProgramStateRef State) {
    auto ULS = State->get<UnrolledLoopStack>();
      auto MBVS = State->get<MaxBlockVisitStack>();

  assert(LoopStmt == ULS.getHead());

  State = State->set<UnrolledLoopStack>(ULS.getTail());
  State = State->set<MaxBlockVisitStack>(MBVS.getTail());
  return State;
}

static internal::Matcher<Stmt> simpleCondition(StringRef BindName) {
  return binaryOperator(anyOf(hasOperatorName("<"), hasOperatorName(">"),
                              hasOperatorName("<="), hasOperatorName(">="),
                              hasOperatorName("!=")),
                        hasEitherOperand(ignoringParenImpCasts(declRefExpr(
                            to(varDecl(hasType(isInteger())).bind(BindName))))),
                        hasEitherOperand(ignoringParenImpCasts(
                            integerLiteral().bind("counterEndVal"))))
      .bind("condition");
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
             hasLoopInit(anyOf(
                 declStmt(hasSingleDecl(varDecl(allOf(
                     hasInitializer(integerLiteral().bind("counterStartVal")),
                     equalsBoundNode("initVarName"))))),
                 binaryOperator(
                     hasLHS(declRefExpr(
                         to(varDecl(equalsBoundNode("initVarName"))))),
                     hasRHS(integerLiteral().bind("counterStartVal"))))),
             // Incrementation should be a simple increment or decrement
             // operator call.
             hasIncrement(
                 unaryOperator(
                     anyOf(hasOperatorName("++"), hasOperatorName("--")),
                     hasUnaryOperand(declRefExpr(to(varDecl(
                         allOf(equalsBoundNode("initVarName"),
                               hasType(isInteger()))))))).bind("stepOperator")),
             unless(hasBody(hasSuspiciousStmt("initVarName")))).bind("forLoop");
}

ProgramStateRef setMaxBlockVisitOnPath(const Stmt *LoopStmt, ASTContext &ASTCtx,
                                       ProgramStateRef State,
                                       unsigned DefaultVal) {  
  if (!isLoopStmt(LoopStmt))
    return State;

  // TODO: Match the cases where the bound is not a concrete literal but an
  // integer with known value
  auto ULS = State->get<UnrolledLoopStack>();
  auto MBVS = State->get<MaxBlockVisitStack>();
  auto Matches = match(forLoopMatcher(), *LoopStmt, ASTCtx);

  if (!ULS.isEmpty() && LoopStmt == ULS.getHead())
    return State;

  if (Matches.empty()) {
    State = State->add<MaxBlockVisitStack>(llvm::APInt(8, 0));
    State = State->add<UnrolledLoopStack>(LoopStmt);
    return State;
  }

  llvm::APInt Start =
      Matches[0].getNodeAs<IntegerLiteral>("counterStartVal")->getValue();
  llvm::APInt End =
      Matches[0].getNodeAs<IntegerLiteral>("counterEndVal")->getValue();
  llvm::APInt Diff = (End - Start).abs();
  auto Cond = Matches[0].getNodeAs<BinaryOperator>("condition");
  if (Cond->getOpcode() == BO_GE || Cond->getOpcode() == BO_LE)
    Diff++;

  llvm::APInt OuterVisitNum = llvm::APInt(Diff.getBitWidth(), 1);
  //if (!MBVS.isEmpty() && MBVS.getHead().getLimitedValue() != DefaultVal)
  //  OuterVisitNum =
  //     llvm::APInt(Diff.getBitWidth(), MBVS.getHead().getLimitedValue());

  //Diff *= OuterVisitNum;
  State = State->add<MaxBlockVisitStack>(Diff.abs());
  State = State->add<UnrolledLoopStack>(LoopStmt);
  ++NumTimesLoopUnrolled;
  return State;
}

unsigned getMaxBlockVisitOnPath(ProgramStateRef State) {
  auto MBVS = State->get<MaxBlockVisitStack>();
  if (MBVS.isEmpty() or MBVS.getHead() == 0 )
    return 0;
  return 1;
}
}
}
