
#include "regs.h"

// FIX: need to put utcb addr in EDI

int Unmap(int control, int tag) {
  int mr0, ign0, ign1;

  asm volatile(
    "int $0x87" 
    : _EAX(ign0), _ESI(mr0) 
    : "0" (control), "1" (tag)
    : "memory");

  return mr0;
}

