int fib(int arg0);

int main(void) {
  int n = 11;
  return fib(n);
}

int fib(int n) {
  if (n == 0 || n == 1) {
      return n;
  } else {
      return fib(n - 1) + fib(n - 2);
  }
}
