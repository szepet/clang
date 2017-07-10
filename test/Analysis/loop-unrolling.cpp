// REQUIRES: asserts
// RUN: %clang_analyze_cc1 -analyzer-checker=core -analyzer-config unroll-loops=true -analyzer-stats -verify %s 2>&1 | FileCheck %s
//
int getNum();
void foo(int &);
// Testing for loops.
int simple_unroll1() {
  int a[9];
  int k = 42;
  for (int i = 0; i < 9; i++) {
    a[i] = 42;
  }
  int b = 22 / (k - 42); // expected-warning {{Division by zero}}
  return 0;
}

int simple_unroll2() {
  int a[9];
  int k = 42;
  int i;
  for (i = 0; i < 9; i++) {
    a[i] = 42;
  }
  int b = 22 / (k - 42); // expected-warning {{Division by zero}}
  return 0;
}

int simple_no_unroll1() {
  int a[9];
  int k = 42;
  for (int i = 0; i < 9; i++) {
    a[i] = 42;
    foo(i);
  }
  int b = 22 / (k - 42); // expected-warning {{Division by zero}}
  return 0;
}

int simple_no_unroll2() {
  int a[9];
  int k = 42;
  int i;
  for (i = 0; i < 9; i++) {
    a[i] = 42;
    i += getNum();
  }
  int b = 22 / (k - 42); // expected-warning {{Division by zero}}
  return 0;
}

int simple_no_unroll3() {
  int a[9];
  int k = 42;
  int i;
  for (i = 0; i < 9; i++) {
    a[i] = 42;
    (void)&i;
  }
  int b = 22 / (k - 42); // no-warning
  return 0;
}

int simple_no_unroll4() {
  int a[9];
  int k = 42;
  int i;
  for (i = 0; i < 9; i++) {
    a[i] = 42;
    int &j = i;
  }
  int b = 22 / (k - 42); // no-warning
  return 0;
}

int nested_outer_unrolled() {
  int a[9];
  int k = 42;
  int j = 0;
  for (int i = 0; i < 9; i++) {
    for (j = 0; j < getNum(); ++j) {
      a[j] = 22;
    }
    a[i] = 42;
  }
  int b = 22 / (k - 42); // no-warning
  return 0;
}

int nested_inner_unrolled() {
  int a[9];
  int k = 42;
  int j = 0;
  for (int i = 0; i < getNum(); i++) {
    for (j = 0; j < 8; ++j) {
      a[j] = 22;
    }
    a[i] = 42;
  }
  int b = 22 / (k - 42); // expected-warning {{Division by zero}}
  return 0;
}

int nested_both_unrolled() {
  int a[9];
  int k = 42;
  int j = 0;
  for (int i = 0; i < 7; i++) {
    for (j = 0; j < 6; ++j) {
      a[j] = 22;
    }
    a[i] = 42;
  }
  int b = 22 / (k - 42); // expected-warning {{Division by zero}}
  return 0;
}

int simple_known_bound_loop() {
  for (int i = 2; i < 12; i++) {
  }
  return 0;
}

int simple_unknown_bound_loop() {
  for (int i = 2; i < getNum(); i++) {
  }
  return 0;
}

int nested_inlined_unroll1() {
  int k;
  for (int i = 0; i < 9; i++) {
    k = simple_known_bound_loop(); // no reevaluation without inlining
  }
  int a = 22 / k; // expected-warning {{Division by zero}}
  return 0;
}

int nested_inlined_no_unroll1() {
  int k;
  for (int i = 0; i < 9; i++) {
    k = simple_unknown_bound_loop(); // reevaluation without inlining
  }
  int a = 22 / k; // no-warning
  return 0;
}

// CHECK: ... Statistics Collected ...
// CHECK: 5 ExprEngine       - The # of times we re-evaluated a call without inlining
// CHECK: 9 LoopUnrolling    - The # of times a loop has got completely unrolled
