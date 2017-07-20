// RUN: %clang_cc1 -analyze -analyzer-checker=debug.DumpCFG -analyzer-config cfg-loopexit=true %s > %t 2>&1
// RUN: FileCheck --input-file=%t %s

// CHECK:       [B5 (ENTRY)]
// CHECK-NEXT:   Succs (1): B4

// CHECK:       [B1]
// CHECK-NEXT:   1: i
// CHECK-NEXT:   2:       [B1.1]++
// CHECK-NEXT:   Preds (1): B2
// CHECK-NEXT:   Succs (1): B3

// CHECK:       [B2]
// CHECK-NEXT:   1: i
// CHECK-NEXT:   2:       [B2.1]++
// CHECK-NEXT:   Preds (1): B3
// CHECK-NEXT:   Succs (1): B1

// CHECK:       [B3]
// CHECK-NEXT:   1: i
// CHECK-NEXT:   2:       [B3.1] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT:   3: 12
// CHECK-NEXT:   4:       [B3.2] < [B3.3]
// CHECK-NEXT:   T: for (...; [B3.4]; ...)
// CHECK-NEXT:   Preds (2): B1 B4
// CHECK-NEXT:   Succs (2): B2 B0

// CHECK:       [B4]
// CHECK-NEXT:   1: 0
// CHECK-NEXT:   2: int i = 0;
// CHECK-NEXT:   Preds (1): B5
// CHECK-NEXT:   Succs (1): B3

// CHECK:       [B0 (EXIT)]
// CHECK-NEXT:   1: ForStmt (LoopExit)
// CHECK-NEXT:   Preds (1): B3
void check_forloop1() {
  for (int i = 0; i < 12; i++) {
    i++;
  }
  
}

// CHECK:       [B3 (ENTRY)]
// CHECK-NEXT:   Succs (1): B2

// CHECK:       [B1]
// CHECK-NEXT:   Preds (1): B2
// CHECK-NEXT:   Succs (1): B2

// CHECK:       [B2]
// CHECK-NEXT:   T: for (; ; )
// CHECK-NEXT:   Preds (2): B1 B3
// CHECK-NEXT:   Succs (2): B1 NULL

// CHECK:       [B0 (EXIT)]
// CHECK-NEXT:   1: ForStmt (LoopExit)
void check_forloop2() {
  for (;;)
    ;
  
}

// CHECK:       [B4 (ENTRY)]
// CHECK-NEXT:   Succs (1): B3

// CHECK:       [B1]
// CHECK-NEXT:   Preds (1): B2
// CHECK-NEXT:   Succs (1): B3

// CHECK:       [B2]
// CHECK-NEXT:   1: int i;
// CHECK-NEXT:   Preds (1): B3
// CHECK-NEXT:   Succs (1): B1

// CHECK:       [B3]
// CHECK-NEXT:   1: true
// CHECK-NEXT:   T: while [B3.1]
// CHECK-NEXT:   Preds (2): B1 B4
// CHECK-NEXT:   Succs (2): B2 NULL

// CHECK:       [B0 (EXIT)]
// CHECK-NEXT:   1: WhileStmt (LoopExit)
void check_while1() {
  while (true) {
    int i;
  }
  
}

// CHECK:       [B4 (ENTRY)]
// CHECK-NEXT:   Succs (1): B3

// CHECK:       [B1]
// CHECK-NEXT:   Preds (1): B2
// CHECK-NEXT:   Succs (1): B2

// CHECK:       [B2]
// CHECK-NEXT:   1: l
// CHECK-NEXT:   2:       [B2.1] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT:   3: 42
// CHECK-NEXT:   4:       [B2.2] < [B2.3]
// CHECK-NEXT:   T: while [B2.4]
// CHECK-NEXT:   Preds (2): B1 B3
// CHECK-NEXT:   Succs (2): B1 B0

// CHECK:       [B3]
// CHECK-NEXT:   1: int l;
// CHECK-NEXT:   Preds (1): B4
// CHECK-NEXT:   Succs (1): B2

// CHECK:       [B0 (EXIT)]
// CHECK-NEXT:   1: WhileStmt (LoopExit)
// CHECK-NEXT:   Preds (1): B2

void check_while2() {
  int l;
  while (l < 42)
    ;
  
}

// CHECK:       [B3 (ENTRY)]
// CHECK-NEXT:   Succs (1): B1

// CHECK:       [B1]
// CHECK-NEXT:   1: false
// CHECK-NEXT:   T: do ... while [B1.1]
// CHECK-NEXT:   Preds (2): B2 B3
// CHECK-NEXT:   Succs (2): NULL B0

// CHECK:       [B2]
// CHECK-NEXT:   Succs (1): B1

// CHECK:       [B0 (EXIT)]
// CHECK-NEXT:   1: DoStmt (LoopExit)
// CHECK-NEXT:   Preds (1): B1

void check_dowhile1() {
  do {
  } while (false);
  
}

// CHECK:       [B5 (ENTRY)]
// CHECK-NEXT:   Succs (1): B4

// CHECK:       [B1]
// CHECK-NEXT:   1: j
// CHECK-NEXT:   2:       [B1.1] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT:   3: 20
// CHECK-NEXT:   4:       [B1.2] < [B1.3]
// CHECK-NEXT:   T: do ... while [B1.4]
// CHECK-NEXT:   Preds (1): B2
// CHECK-NEXT:   Succs (2): B3 B0

// CHECK:       [B2]
// CHECK-NEXT:   1: j
// CHECK-NEXT:   2: 2
// CHECK-NEXT:   3:       [B2.1] += [B2.2]
// CHECK-NEXT:   Preds (2): B3 B4
// CHECK-NEXT:   Succs (1): B1

// CHECK:       [B3]
// CHECK-NEXT:   Preds (1): B1
// CHECK-NEXT:   Succs (1): B2

// CHECK:       [B4]
// CHECK-NEXT:   1: 2
// CHECK-NEXT:   2: int j = 2;
// CHECK-NEXT:   Preds (1): B5
// CHECK-NEXT:   Succs (1): B2

// CHECK:       [B0 (EXIT)]
// CHECK-NEXT:    1: DoStmt (LoopExit)
// CHECK-NEXT:   Preds (1): B1
void check_dowhile2() {
  int j = 2;
  do {
    j += 2;
  } while (j < 20);
  
}
