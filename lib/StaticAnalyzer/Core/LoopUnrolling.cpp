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

REGISTER_MAP_WITH_PROGRAMSTATE(UnrolledLoops, const Stmt *, const CFGStmtMap *)

namespace clang {
namespace ento {

static bool isLoopStmt(const Stmt *S) {
  return S && (isa<ForStmt>(S) || isa<WhileStmt>(S) || isa<DoStmt>(S));
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
  return hasDescendant(binaryOperator(
      anyOf(hasOperatorName("="), hasOperatorName("+="), hasOperatorName("/="),
            hasOperatorName("*="), hasOperatorName("-=")),
      hasLHS(ignoringParenImpCasts(
          declRefExpr(to(varDecl(equalsBoundNode(NodeName))))))));
}

static internal::Matcher<Stmt> callByRef(StringRef NodeName) {
  return hasDescendant(callExpr(forEachArgumentWithParam(
      declRefExpr(to(varDecl(equalsBoundNode(NodeName)))),
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

bool shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx) {

  if (!isLoopStmt(LoopStmt))
    return false;

  // TODO: In cases of while and do..while statements the value of initVarName
  // should be checked to be known
  // TODO: Match the cases where the bound is not a concrete literal but an
  // integer with known value

  auto Matches = match(loopMatcher(), *LoopStmt, ASTCtx);
  return !Matches.empty();
}

namespace {
class LoopBlockVisitor : public ConstStmtVisitor<LoopBlockVisitor> {
public:
  LoopBlockVisitor(llvm::SmallPtrSet<const CFGBlock *, 8> &BS)
      : BlockSet(BS), Found(false) {}

  void VisitChildren(const Stmt *S) {
    for (const Stmt *Child : S->children())
      if (Child)
        Visit(Child);
  }

  void VisitStmt(const Stmt *S) {
    if (!S || (isLoopStmt(S) && S != LoopStmt) || Found)
      return;

    BlockSet.insert(StmtToBlockMap->getBlock(S));

    VisitChildren(S);
  }

  void setBlocksOfLoop(const Stmt *Loop, const CFGStmtMap *M) {
    BlockSet.clear();
    StmtToBlockMap = M;
    LoopStmt = Loop;
    Visit(LoopStmt);
  }

private:
  llvm::SmallPtrSet<const CFGBlock *, 8> &BlockSet;
  bool Found;
  const CFGStmtMap *StmtToBlockMap;
  const Stmt *LoopStmt;
};
}

bool isUnrolledLoopBlock(const CFGBlock *Block, ExplodedNode *Prev) {
  const Stmt* Term = Block->getTerminator();
  auto State = Prev->getState();
  // In case of nested loops in an inlined function should not be unrolled only
  // if the inner loop is marked.
  if (Term && isLoopStmt(Term) && !State->contains<UnrolledLoops>(Term))
    return false;

  const CFGBlock *SearchedBlock;
  llvm::SmallPtrSet<const CFGBlock *, 8> BlockSet;
  LoopBlockVisitor LBV(BlockSet);
  // Check the CFGBlocks of every marked loop.
  for (auto& E : State->get<UnrolledLoops>()) {
    SearchedBlock = Block;
    const StackFrameContext *StackFrame = Prev->getStackFrame();
    LBV.setBlocksOfLoop(E.first, E.second);
    // In case of an inlined function call check if any of its callSiteBlock is
    // marked.
    while (SearchedBlock && BlockSet.find(SearchedBlock) == BlockSet.end()) {
      SearchedBlock = StackFrame->getCallSiteBlock();
      StackFrame = StackFrame->getParent()->getCurrentStackFrame();
    }

    if (SearchedBlock)
      return true;
  }

  return false;
}

ProgramStateRef markLoopAsUnrolled(const Stmt *Term, ProgramStateRef State,
                                   CFGStmtMap *StmtToBlockMap) {
  if (State->contains<UnrolledLoops>(Term))
    return State;

  State = State->set<UnrolledLoops>(Term, StmtToBlockMap);
  ++NumTimesLoopUnrolled;
  return State;
}
}
}
