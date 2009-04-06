void exit(int c) {
  __asm__ ("mov %0,%%ebx" : : "r"(c));
  __asm__ ("clr %%eax" : :);
  __asm__ ("int $0x80");
}
