#include <stdio.h>

// Too simple - gets resolved to constant expression then inlined
[[clang::optnone]]
[[clang::noinline]]
int foo(int x) {
  return x + 1 - 2;
}

int main() {
  int x;
  scanf("%d", &x);
  for (int i = 0; i < x; i++) printf("\t%d\n", foo(i + 2));
  return foo(1);
}
