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

REGISTER_MAP_WITH_PROGRAMSTATE(UnrolledLoopBlocks, const Stmt *,
                               llvm::ImmutableSet<const CFGBlock *>)

class LoopVisitor : public ConstStmtVisitor<LoopVisitor> {
public:
    LoopVisitor(ProgramStateRef State, AnalysisManager& AMgr, CFGStmtMap* M):
            State(State), AMgr(AMgr),StmtToBlockMap(M){}

    void VisitChildren(const Stmt* S){
      for (const Stmt *Child : S->children())
        if(Child)
          Visit(Child);
    }

    void VisitStmt(const Stmt* S) {
      if (!S ||
          (isa<ForStmt>(S) && !State->contains<UnrolledLoopBlocks>(S)))
        return;
      auto BlockSet = *State->get<UnrolledLoopBlocks>(S);
      BlockSet = F.add(BlockSet,StmtToBlockMap->getBlock(S));
      if (auto CallExp = dyn_cast<CallExpr>(S)) {
        auto CalleeCFG = AMgr.getCFG(CallExp->getCalleeDecl());
        for (CFG::const_iterator BlockIt = CalleeCFG->begin();
             BlockIt != CalleeCFG->end(); BlockIt++) {
             BlockSet = F.add(BlockSet,StmtToBlockMap->getBlock(S));
        }
      }
      VisitChildren(S);
    }
private:
    ProgramStateRef State;
    AnalysisManager& AMgr;
    CFGStmtMap* StmtToBlockMap;

    llvm::ImmutableSet<const CFGBlock *>::Factory F;
};

bool shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx,
                            ExplodedNode *Pred) {
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
              hasRHS(integerLiteral().bind(
                  "bound")))),
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
  }
  return true;
}

void processCFGBlockEntrance(const BlockEdge &L,
  NodeBuilderWithSinks &nodeBuilder,
  ExplodedNode *Pred) {
    //LoopVisitor v(AMgr,
    //              Pred->getLocationContext()->getAnalysisDeclContext()->getCFGStmtMap());
    PrettyStackTraceLocationContext CrashInfo(Pred->getLocationContext());
    const Stmt *Term = nodeBuilder.getContext().getBlock()->getTerminator();
    /*if (Term && isa<ForStmt>(Term) && shouldCompletelyUnroll(Term, AMgr.getASTContext(), Pred)) {
     if(UnrolledLoops.find(Term)==UnrolledLoops.end()) {
        UnrolledLoops.insert(Term);
        v.Visit(Term);
      }
      //NumTimesLoopUnrolled = UnrolledLoops.size();
      return;
    }*/

    //if(ExceptionBlocks.find(nodeBuilder.getContext().getBlock()) != ExceptionBlocks.end()){
    //  return;
   // }
}

