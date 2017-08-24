//===--- LoopWidening.h - Widen loops ---------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// This header contains the declarations of functions which are used to widen
/// loops which do not otherwise exit. The widening is done by invalidating
/// anything which might be modified by the body of the loop.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_LOOPWIDENING_H
#define LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_LOOPWIDENING_H

#include "clang/Analysis/CFG.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExplodedGraph.h"

namespace clang {
namespace ento {

/// \brief Get the states that result from widening the loop.
///
/// Widen the loop by invalidating anything that might be modified
/// by the loop body in any iteration.
ProgramStateRef getWidenedLoopState(ProgramStateRef PrevState,
                                    const LocationContext *LCtx,
                                    unsigned BlockCount, const Stmt *LoopStmt);

ProgramStateRef getConservativelyWidenedLoopState(ProgramStateRef State,
                                                  ASTContext &ASTCtx,
                                                  const LocationContext *LCtx,
                                                  unsigned BlockCount,
                                                  const Stmt *LoopStmt);

bool isWidenedState(ProgramStateRef State);
const Stmt *isInsideOfALoop(ProgramStateRef State);

ProgramStateRef updateWidenLoopStack(const Stmt *LoopStmt, ASTContext &ASTCtx,
                                     ExplodedNode *Pred);

ProgramStateRef processWidenLoopEnd(const Stmt *LoopStmt,
                                    ProgramStateRef State);

} // end namespace ento
} // end namespace clang

#endif
