
#include "regs.h"

int ThreadSwitch(int dest) {
  int ign0;

  asm volatile ("int $0x83" : _EAX (ign0) : "0" (dest) : "memory");
}

void yield() {
   ThreadSwitch(0);
}

void stop() {
   ThreadSwitch(1);
}

