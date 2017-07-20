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
/// CFGBlocks.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_LOOPUNROLLING_H
#define LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_LOOPUNROLLING_H

#include "clang/Analysis/CFG.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExplodedGraph.h"

namespace clang {
namespace ento {
unsigned getMaxBlockVisitOnPath(ProgramStateRef State);
ProgramStateRef setMaxBlockVisitOnPath(const Stmt *LoopStmt, ASTContext &ASTCtx,
                                       ProgramStateRef State, unsigned DefaultVal);
ProgramStateRef processLoopEnd(const Stmt *LoopStmt, ProgramStateRef State);
} // end namespace ento
} // end namespace clang

#endif
