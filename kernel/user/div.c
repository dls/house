main () {
  int a = 10 / (fib(5) - fib(5));
}

int fib(int x) {
  if (x < 2) return x;
  else return fib(x-1) + fib(x-2);
}
