// REQUIRES: asserts
// RUN: %clang_analyze_cc1 -analyzer-checker=core -analyzer-config unroll-loops=true -analyzer-stats -verify %s 2>&1 | FileCheck %s

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

// Testing while loops.
int simple_unroll3() {
  int a[9];
  int k = 42;
  int i = 0;
  while (i < 9) {
    a[i] = 42 * i;
    ++i;
  }
  int b = 22 / (k - 42); // expected-warning {{Division by zero}}
  return 0;
}

int simple_unroll4() {
  int a[22];
  int k = 42;
  int i = 21;
  while (i > 7) {
    a[i] = 42 * i;
    --i;
  }
  int b = 22 / (k - 42); // expected-warning {{Division by zero}}
  return 0;
}

int simple_no_unroll4() {
  int a[9];
  int k = 42;
  int i = 0;
  while (i < 9) {
    a[i] = 42 * i;
  }
  int b = 22 / (k - 42); // no-warning
  return 0;
}

// CHECK: ... Statistics Collected ...
// CHECK: 8 LoopUnrolling    - The # of times a loop has got completely unrolled
