
#include "regs.h"

int ThreadControl(int dest, int pager, int sched, int ss, void* utcb) {
  int result, ign1, ign2, ign3, ign4;

  asm volatile (
    "int $0x90"
    : _EAX (result), _ECX (ign1), _EDX (ign2), _ESI (ign3), _EDI (ign4)
    : "0" (dest),  "1" (pager), "2" (sched), "3" (ss), "4" ((int)utcb)
    : "memory");

  return result;
}

int tc(int dest, int ss, void* entry) {
  int result, ign1, ign2, ign3;

  asm volatile (
    "int $0x90"
    : _EAX (result), _ECX (ign1), _ESI (ign3)
    : "0" (dest), "1" ((int)entry), "2" (ss)
    : "memory");

  return result;
}

int fork(int tid, void* entry) {
  int result, ign1;

  asm volatile (
    "int $0x90"
    : _EAX (result), _EDX (ign1)
    : "0" (tid), "1" ((int) entry)
    : "memory");

  return result;
}
