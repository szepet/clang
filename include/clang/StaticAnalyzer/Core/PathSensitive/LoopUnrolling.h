//===--- LoopUnrolling.h - Unroll loops -------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// This header contains the declarations of functions which are used to decide
/// which loops should be completely unrolled and mark their corresponding
/// CFGBlocks. It is done by tracking a stack of loops in the ProgramState. This
/// way specific loops can be marked as completely unrolled. For considering a
/// loop to be completely unrolled it has to fulfill the following requirements:
/// - has escaped.
/// - known bound
/// - no goto or returnStmt
///
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_LOOPUNROLLING_H
#define LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_LOOPUNROLLING_H

#include "clang/Analysis/CFG.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExplodedGraph.h"
namespace clang {
namespace ento {
class AnalysisManager;

/// Returns if the given State indicates that is inside a completely unrolled
/// loop.
bool isUnrolledState(ProgramStateRef State);
/// Updates the stack of loops contained by the ProgramState.
ProgramStateRef updateLoopStack(const Stmt *LoopStmt, ASTContext &ASTCtx,
                                ProgramStateRef State,
                                const LocationContext *LCtx);
/// Updates the given ProgramState. In current implementation it removes the top
/// element of the stack of loops.
ProgramStateRef processLoopEnd(const Stmt *LoopStmt, ProgramStateRef State);

} // end namespace ento
} // end namespace clang

#endif
