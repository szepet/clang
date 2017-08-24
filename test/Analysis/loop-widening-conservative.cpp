// RUN: %clang_analyze_cc1 -analyzer-checker=core,unix.Malloc,debug.ExprInspection -analyzer-max-loop 4 -analyzer-config widen-loops-conservative=true,cfg-loopexit=true -verify -std=c++11 %s

void clang_analyzer_eval(int);
void clang_analyzer_warnIfReached();

typedef __typeof(sizeof(int)) size_t;
void *malloc(size_t);
void free(void *);

void loop_which_iterates_limit_times_not_widened() {
  int i;
  int x = 1;
  // Check loop isn't widened by checking x isn't invalidated
  for (i = 0; i < 1; ++i) {
  }
  clang_analyzer_eval(x == 1); // expected-warning {{TRUE}}
  for (i = 0; i < 2; ++i) {
  }
  clang_analyzer_eval(x == 1); // expected-warning {{TRUE}}
  for (i = 0; i < 3; ++i) {
  }
  clang_analyzer_eval(x == 1); // expected-warning {{TRUE}}
  for (i = 0; i < 4; ++i) {
  }
  clang_analyzer_eval(x == 1); // expected-warning {{TRUE}}
}

int a_global;

void loop_evaluated_before_widening() {
  int i;
  a_global = 1;
  for (i = 0; i < 10; ++i) {
    if (i == 2) {
      // True before widening then unknown after.
      clang_analyzer_eval(a_global == 1); // expected-warning{{TRUE}}
    }
  }
  clang_analyzer_warnIfReached(); // expected-warning{{REACHABLE}}
}

void warnings_after_loop() {
  int i;
  for (i = 0; i < 10; ++i) {
  }
  char *m = (char *)malloc(12);
} // expected-warning {{Potential leak of memory pointed to by 'm'}}

void for_loop_exits() {
  int i;
  for (i = 0; i < 10; ++i) {
  }
  clang_analyzer_warnIfReached(); // expected-warning{{REACHABLE}}
}

void invariably_infinite_loop() {
  int i = 0;
  for (;;) {
    ++i;
  }
  clang_analyzer_warnIfReached(); // no-warning
}

void invariably_infinite_break_loop() {
  int i = 0;
  while (1) {
    ++i;
    int x = 1;
    if (!x)
      break;
  }
  clang_analyzer_warnIfReached(); // no-warning
}

void reachable_break_loop() {
  int i = 0;
  while (1) {
    ++i;
    if (i == 100)
      break;
  }
  clang_analyzer_warnIfReached(); // expected-warning{{REACHABLE}}
}

void condition_constrained_true_in_loop() {
  int i = 0;
  while (i < 50) {
    clang_analyzer_eval(i < 50); // expected-warning {{TRUE}}
    ++i;
  }
  clang_analyzer_warnIfReached(); // expected-warning{{REACHABLE}}
}

void condition_constrained_false_after_loop() {
  int i = 0;
  while (i < 50) {
    ++i;
  }
  clang_analyzer_eval(i >= 50);   // expected-warning {{TRUE}}
  clang_analyzer_warnIfReached(); // expected-warning{{REACHABLE}}
}

void multiple_exit_test() {
  int x = 0;
  int i = 0;
  while (i < 50) {
    if (x) {
      i = 10;
      break;
    }
    ++i;
  }
  // Reachable by 'normal' exit.
  if (i == 50)
    clang_analyzer_warnIfReached(); // expected-warning{{REACHABLE}}
  // x is known to be 0, not reachable from break.
  if (i == 10)
    clang_analyzer_warnIfReached(); // no-warning
  // Not reachable.
  if (i < 10)
    clang_analyzer_warnIfReached(); // no-warning
  if (i > 10 && i < 50)
    clang_analyzer_warnIfReached(); // no-warning
}

void pointer_doesnt_leak_from_loop() {
  int *h_ptr = (int *)malloc(sizeof(int));
  for (int i = 0; i < 2; ++i) {
  }
  for (int i = 0; i < 10; ++i) {
  } // no-warning
  free(h_ptr);
}

int g_global;

void unknown_after_loop(int s_arg) {
  g_global = 0;
  s_arg = 1;
  int s_local = 2;
  int *h_ptr = (int *)malloc(sizeof(int));
  int change_var = 1;
  int nochange_var = 2;
  for (int i = 0; i < 10; ++i) {
    change_var *= nochange_var + nochange_var;
  }

  clang_analyzer_eval(change_var == 1);   // expected-warning {{UNKNOWN}}
  clang_analyzer_eval(nochange_var == 2); // expected-warning {{TRUE}}

  clang_analyzer_eval(g_global);     // expected-warning {{UNKNOWN}}
  clang_analyzer_eval(s_arg == 1);   // expected-warning {{TRUE}}
  clang_analyzer_eval(s_local == 2); // expected-warning {{TRUE}}
  clang_analyzer_eval(h_ptr == 0);   // expected-warning {{UNKNOWN}}
  free(h_ptr);
}

void nested_loop_outer_widen() {
  int i;
  int j;
  for (i = 0; i < 10; i++) {
    clang_analyzer_eval(i < 10); // expected-warning {{TRUE}}
    for (j = 0; j < 2; j++) {
      clang_analyzer_eval(j < 2); // expected-warning {{TRUE}}
    }
    clang_analyzer_eval(j >= 2); // expected-warning {{TRUE}}
  }
  clang_analyzer_eval(i >= 10); // expected-warning {{TRUE}}
}

void nested_loop_inner_widen() {
  int i = 0, j = 0;
  for (i = 0; i < 2; i++) {
    clang_analyzer_eval(i < 2); // expected-warning {{TRUE}}
    for (j = 0; j < 10; j++) {
      clang_analyzer_eval(j < 10); // expected-warning {{TRUE}}
    }
    clang_analyzer_eval(j >= 10); // expected-warning {{TRUE}}
  }
  clang_analyzer_eval(i >= 2); // expected-warning {{TRUE}}
}

void nested_loops() {
  int i = 0, j = 0;
  int x = 2;
  for (i = 0; i < 5; i++) {
    if (i == 3)
      break;
    for (j = 0; j < 10; j++) {
      clang_analyzer_eval(j < 10); // expected-warning {{TRUE}}
    }
    clang_analyzer_eval(j >= 10); // expected-warning {{TRUE}}
  }
  clang_analyzer_eval(i >= 5); // expected-warning {{TRUE}} // expected-warning {{FALSE}}
  clang_analyzer_eval(i == 3); // expected-warning {{TRUE}} // expected-warning {{FALSE}}

  // Precision loss of an inner loop variable resulted by the widening of the
  // outer loop.
  clang_analyzer_eval(j >= 10); // expected-warning {{UNKNOWN}}

  clang_analyzer_eval(x == 2); // expected-warning {{TRUE}}
}

void nested_loops_2() {
  int i = 0, j = 0, k = 0;
  int x = 2;
  for (i = 0; i < 5; i++) {
    if (i == 3)
      break;
    for (j = 0; j < 10; j++) {
      clang_analyzer_eval(j < 10); // expected-warning {{TRUE}}
      for (k = 0; k < 42; k++) {
        clang_analyzer_eval(k < 42); // expected-warning {{TRUE}}
      }
    }
    clang_analyzer_eval(k >= 42); // expected-warning {{UNKNOWN}}
    clang_analyzer_eval(j >= 10); // expected-warning {{TRUE}}
  }
  clang_analyzer_eval(i >= 5); // expected-warning {{TRUE}} // expected-warning {{FALSE}}
  clang_analyzer_eval(i == 3); // expected-warning {{TRUE}} // expected-warning {{FALSE}}

  // Precision loss of the inner loop variables resulted by the widening of the
  // outer loops.
  clang_analyzer_eval(j >= 10); // expected-warning {{UNKNOWN}}
  clang_analyzer_eval(k >= 42); // expected-warning {{UNKNOWN}}

  clang_analyzer_eval(x == 2); // expected-warning {{TRUE}}
}

struct A {
  A(int n) : num(n){};
  int num;
  void foo();
  void bar() const;
  bool operator==(const A &other) const;
  void operator=(const A &other);
  void assign(const A &other) { num = other.num; }
};

void check_memberexpr() {
  A a(2);
  for (int i = 0; i < 10; ++i) {
    a.num = i;
  }
  clang_analyzer_eval(a.num == 9); // expected-warning {{UNKNOWN}}
}

void non_const_method_call() {
  A a(42);
  for (int i = 0; i < 10; ++i) {
    a.foo();
  }
  clang_analyzer_eval(a.num == 42); // expected-warning {{UNKNOWN}}
}

void const_method_call() {
  A a(42);
  for (int i = 0; i < 10; ++i) {
    a.bar();
  }
  clang_analyzer_eval(a.num == 42); // expected-warning {{TRUE}}
}

void known_method_call() {
  A a(42);
  A b(10);
  for (int i = 0; i < 10; ++i) {
    a.assign(b);
  }
  // Loss of precision. It could be worth to simulate the last iteration after
  // widening as well.
  clang_analyzer_eval(a.num == 10); // expected-warning {{UNKNOWN}}
}

void non_const_operator_call() {
  A a(42);
  A b(10);
  for (int i = 0; i < 10; ++i) {
    a = b;
  }
  clang_analyzer_eval(a.num == 42); // expected-warning {{UNKNOWN}}
}

void const_operator_call() {
  A a(42);
  A b(10);
  for (int i = 0; i < 10; ++i) {
    if (a == b)
      continue;
  }
  clang_analyzer_eval(a.num == 42); // expected-warning {{TRUE}}
}

void passByRef(int &n);
void passByConstRef(const int &n);

void widen_escape() {
  int n = 2;
  for (int i = 0; i < 10; ++i) {
    passByRef(n);
  }
  clang_analyzer_eval(n == 2); // expected-warning {{UNKNOWN}}
}

void widen_const_escape() {
  int n = 2;
  for (int i = 0; i < 10; ++i) {
    passByConstRef(n);
  }
  clang_analyzer_eval(n == 2); // expected-warning {{TRUE}}
}

void widen_array() {
  int arr[10];
  for (int i = 0; i < 10; ++i) {
    arr[i] = i;
  }
  clang_analyzer_eval(arr[5] == 5); // expected-warning {{UNKNOWN}}
}

void no_widen_pointer() {
  int *r = nullptr;
  for (int i = 0; i < 10; ++i) {
    int *p;
    p = r;
  }
  clang_analyzer_warnIfReached(); // no-warning
}
