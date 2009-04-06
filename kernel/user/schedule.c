
#include "regs.h"

// FIX: need to return time_control 

int Schedule(int dest, int prio, int tc, int pc, int preempt) {
  int result, r_tc, ign0, ign1, ign2;

  asm volatile (
    "int $0x84"
    : _EAX (result), _ECX (ign0), _EDX (r_tc), _ESI (ign1), _EDI (ign2)
    : "0" (dest), "1" (prio), "2" (tc), "3" (pc), "4" (preempt)
    : "memory");

  return result;
}

int setPriority(int dest, int prio) {
  return Schedule(dest, prio,0,0,0);
}
