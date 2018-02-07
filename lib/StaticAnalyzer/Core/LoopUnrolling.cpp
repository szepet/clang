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

#include "clang/StaticAnalyzer/Core/PathSensitive/LoopUnrolling.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"

#include "llvm/ADT/Statistic.h"

#define DEBUG_TYPE "LoopUnrolling"

STATISTIC(NumLoopsUnrolled,
"The # of times a loop is decided to be unrolled");
STATISTIC(NumDiffLoopsUnrolled,
          "The # of different loops which was worthy to be unrolled");


using namespace clang;
using namespace ento;
using namespace clang::ast_matchers;
std::set<const Stmt*> UnrolledLoops;
static const int MAXIMUM_STEP_UNROLLED = 128;

struct LoopState {
private:
  enum Kind { Normal, Unrolled } K;
  unsigned MaxStep;
  LoopState(Kind InK, unsigned N) : K(InK), MaxStep(N) {}

public:
  static LoopState getNormal(unsigned N) { return LoopState(Normal, N); }
  static LoopState getUnrolled(unsigned N) { return LoopState(Unrolled, N); }
  bool isUnrolled() const { return K == Unrolled; }
  unsigned getMaxStep() const { return MaxStep; }
  bool operator==(const LoopState &X) const {
    return K == X.K && MaxStep == X.MaxStep;
  }
  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.AddInteger(K);
    ID.AddInteger(MaxStep);
  }
};

// The map of the currently simulated loops which are marked depending if we
// decided to unroll them.
REGISTER_MAP_WITH_PROGRAMSTATE(LoopMap, const LoopContext *, LoopState)

namespace clang {
namespace ento {

static bool isLoopStmt(const Stmt *S) {
  return S && (isa<ForStmt>(S) || isa<WhileStmt>(S) || isa<DoStmt>(S));
}

ProgramStateRef processLoopEnd(const LoopContext *LC, ProgramStateRef State) {
  return State->remove<LoopMap>(LC);
}

static internal::Matcher<Stmt> simpleCondition(StringRef BindName) {
  return binaryOperator(anyOf(hasOperatorName("<"), hasOperatorName(">"),
                              hasOperatorName("<="), hasOperatorName(">="),
                              hasOperatorName("!=")),
                        hasEitherOperand(ignoringParenImpCasts(declRefExpr(
                            to(varDecl(hasType(isInteger())).bind(BindName))))),
                        hasEitherOperand(ignoringParenImpCasts(
                            integerLiteral().bind("boundNum"))))
      .bind("conditionOperator");
}

static internal::Matcher<Stmt>
changeIntBoundNode(internal::Matcher<Decl> VarNodeMatcher) {
  return anyOf(
      unaryOperator(anyOf(hasOperatorName("--"), hasOperatorName("++")),
                    hasUnaryOperand(ignoringParenImpCasts(
                        declRefExpr(to(varDecl(VarNodeMatcher)))))),
      binaryOperator(anyOf(hasOperatorName("="), hasOperatorName("+="),
                           hasOperatorName("/="), hasOperatorName("*="),
                           hasOperatorName("-=")),
                     hasLHS(ignoringParenImpCasts(
                         declRefExpr(to(varDecl(VarNodeMatcher)))))));
}

static internal::Matcher<Stmt>
callByRef(internal::Matcher<Decl> VarNodeMatcher) {
  return callExpr(forEachArgumentWithParam(
      declRefExpr(to(varDecl(VarNodeMatcher))),
      parmVarDecl(hasType(references(qualType(unless(isConstQualified())))))));
}

static internal::Matcher<Stmt>
assignedToRef(internal::Matcher<Decl> VarNodeMatcher) {
  return declStmt(hasDescendant(varDecl(
      allOf(hasType(referenceType()),
            hasInitializer(anyOf(
                initListExpr(has(declRefExpr(to(varDecl(VarNodeMatcher))))),
                declRefExpr(to(varDecl(VarNodeMatcher)))))))));
}

static internal::Matcher<Stmt>
getAddrTo(internal::Matcher<Decl> VarNodeMatcher) {
  return unaryOperator(
      hasOperatorName("&"),
      hasUnaryOperand(declRefExpr(hasDeclaration(VarNodeMatcher))));
}

static internal::Matcher<Stmt> hasSuspiciousStmt(StringRef NodeName) {
  return hasDescendant(stmt(
      anyOf(gotoStmt(), switchStmt(), returnStmt(),
            // Escaping and not known mutation of the loop counter is handled
            // by exclusion of assigning and address-of operators and
            // pass-by-ref function calls on the loop counter from the body.
            changeIntBoundNode(equalsBoundNode(NodeName)),
            callByRef(equalsBoundNode(NodeName)),
            getAddrTo(equalsBoundNode(NodeName)),
            assignedToRef(equalsBoundNode(NodeName)))));
}

static internal::Matcher<Stmt> forLoopMatcher() {
  return forStmt(
             hasCondition(simpleCondition("initVarName")),
             // Initialization should match the form: 'int i = 6' or 'i = 42'.
             hasLoopInit(anyOf(
                 declStmt(hasSingleDecl(varDecl(
                     allOf(hasInitializer(integerLiteral().bind("initNum")),
                           equalsBoundNode("initVarName"))))),
                 binaryOperator(hasLHS(declRefExpr(to(
                                    varDecl(equalsBoundNode("initVarName"))))),
                                hasRHS(integerLiteral().bind("initNum"))))),
             // Incrementation should be a simple increment or decrement
             // operator call.
             hasIncrement(unaryOperator(
                 anyOf(hasOperatorName("++"), hasOperatorName("--")),
                 hasUnaryOperand(declRefExpr(
                     to(varDecl(allOf(equalsBoundNode("initVarName"),
                                      hasType(isInteger())))))))),
             unless(hasBody(hasSuspiciousStmt("initVarName"))))
      .bind("forLoop");
}

extern const internal::VariadicDynCastAllOfMatcher<Stmt, IndirectGotoStmt>
    indirectGotoStmt;

bool isPreciselyModelableLoop(const Stmt *LoopStmt, ASTContext &ASTCtx) {
  return match(stmt(hasDescendant(stmt(anyOf(indirectGotoStmt(), caseStmt(), switchStmt())))), *LoopStmt, ASTCtx)
      .empty();
}

static bool isPossiblyEscaped(const VarDecl *VD, ExplodedNode *N) {
  // Global variables assumed as escaped variables.
  if (VD->hasGlobalStorage())
    return true;

  while (!N->pred_empty()) {
    const Stmt *S = PathDiagnosticLocation::getStmt(N);
    if (!S) {
      N = N->getFirstPred();
      continue;
    }

    if (const DeclStmt *DS = dyn_cast<DeclStmt>(S)) {
      for (const Decl *D : DS->decls()) {
        // Once we reach the declaration of the VD we can return.
        if (D->getCanonicalDecl() == VD)
          return false;
      }
    }
    // Check the usage of the pass-by-ref function calls and adress-of operator
    // on VD and reference initialized by VD.
    ASTContext &ASTCtx =
        N->getLocationContext()->getAnalysisDeclContext()->getASTContext();
    auto Match =
        match(stmt(anyOf(callByRef(equalsNode(VD)), getAddrTo(equalsNode(VD)),
                         assignedToRef(equalsNode(VD)))),
              *S, ASTCtx);
    if (!Match.empty())
      return true;

    N = N->getFirstPred();
  }
  llvm_unreachable("Reached root without finding the declaration of VD");
}

bool shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx,
                            ExplodedNode *Pred, unsigned &maxStep) {

  if (!isLoopStmt(LoopStmt))
    return false;

  // TODO: Match the cases where the bound is not a concrete literal but an
  // integer with known value
  auto Matches = match(forLoopMatcher(), *LoopStmt, ASTCtx);
  if (Matches.empty())
    return false;

  auto CounterVar = Matches[0].getNodeAs<VarDecl>("initVarName");
  llvm::APInt BoundNum =
      Matches[0].getNodeAs<IntegerLiteral>("boundNum")->getValue();
  llvm::APInt InitNum =
      Matches[0].getNodeAs<IntegerLiteral>("initNum")->getValue();
  auto CondOp = Matches[0].getNodeAs<BinaryOperator>("conditionOperator");
  if (InitNum.getBitWidth() != BoundNum.getBitWidth()) {
    InitNum = InitNum.zextOrSelf(BoundNum.getBitWidth());
    BoundNum = BoundNum.zextOrSelf(InitNum.getBitWidth());
  }

  if (CondOp->getOpcode() == BO_GE || CondOp->getOpcode() == BO_LE)
    maxStep = (BoundNum - InitNum + 1).abs().getZExtValue();
  else
    maxStep = (BoundNum - InitNum).abs().getZExtValue();

  // Check if the counter of the loop is not escaped before.
  // In case it is defined in the init statement then it couldn't escape yet.
  if (auto ForLoop = dyn_cast<ForStmt>(LoopStmt))
    if (auto InitStmt = dyn_cast<DeclStmt>(ForLoop->getInit()))
      if (InitStmt->getSingleDecl() == CounterVar->getCanonicalDecl())
        return true;
  // Otherwise walk up on the ExplodedGraph and check for every possibility.
  return !isPossiblyEscaped(CounterVar->getCanonicalDecl(), Pred);
}

bool madeNewBranch(ExplodedNode *N, const Stmt *LoopStmt) {
  const Stmt *S = nullptr;
  while (!N->pred_empty()) {
    if (N->succ_size() > 1)
      return true;

    ProgramPoint P = N->getLocation();
    if (Optional<BlockEntrance> BE = P.getAs<BlockEntrance>())
      S = BE->getBlock()->getTerminator();
    else if (Optional<LoopEnter> LE = P.getAs<LoopEnter>())
      S = LE->getLoopStmt();

    if (S == LoopStmt)
      return false;

    N = N->getFirstPred();
  }
  llvm_unreachable("Reached root without encountering the previous step");
}

// updateLoopStates is called on every basic block, therefore it needs to be
// fast
ProgramStateRef updateLoopStates(const LoopContext *LoopCtx, ASTContext &ASTCtx,
                                 ExplodedNode *Pred, unsigned MaxVisitOnPath) {
  auto State = Pred->getState();
  auto LM = State->get<LoopMap>();
  auto LoopStmt = LoopCtx->getLoopStmt();

  assert((!LM.contains(LoopCtx) ||
          (LM.contains(LoopCtx) &&
           Pred->getLocation().getKind() == ProgramPoint::BlockEdgeKind)) &&
         "LoopEnter Block encountered for an already entered loop");

  if (LM.contains(LoopCtx) && LM.lookup(LoopCtx)->isUnrolled() &&
      madeNewBranch(Pred, LoopStmt)) {
    NumLoopsUnrolled--;
    UnrolledLoops.erase(LoopStmt);
    return State->set<LoopMap>(LoopCtx, LoopState::getNormal(MaxVisitOnPath));
  }

  if (LM.contains(LoopCtx))
    return State;

  unsigned MaxStep;
  if (!shouldCompletelyUnroll(LoopStmt, ASTCtx, Pred, MaxStep))
    return State->set<LoopMap>(LoopCtx, LoopState::getNormal(MaxVisitOnPath));

  const auto OuterLoopCtx =
      cast_or_null<LoopContext>(Pred->getLocationContext()->getCurrentLoop());
  unsigned OuterStep =
      (OuterLoopCtx ? LM.lookup(OuterLoopCtx)->getMaxStep() : 1);
  unsigned InnerMaxStep = MaxStep * OuterStep;
  if (InnerMaxStep > MAXIMUM_STEP_UNROLLED)
    return State->set<LoopMap>(LoopCtx, LoopState::getNormal(MaxVisitOnPath));
  else {
    NumLoopsUnrolled++;
    UnrolledLoops.insert(LoopStmt);
    NumDiffLoopsUnrolled = UnrolledLoops.size();
    return State->set<LoopMap>(LoopCtx, LoopState::getUnrolled(InnerMaxStep));
  }
}

bool isUnrolledLoopContext(const LoopContext *LC, ProgramStateRef State) {
  auto LM = State->get<LoopMap>();
  return LM.contains(LC) && LM.lookup(LC)->isUnrolled();
}

} // namespace ento
} // namespace clang
