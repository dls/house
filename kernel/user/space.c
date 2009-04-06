
#include "regs.h"

int SpaceControl(int ss, int control, int kip, int utcb, int redirect) {
  int result, r_control, ign0, ign1, ign2;

  asm volatile (
    "int $0x91"
    : _EAX(result), _ECX(r_control), _EDX(ign0), _ESI(ign1), _EDI(ign2)
    : "0"(ss), "1"(control), "2"(kip), "3"(utcb), "4"(redirect)
    : "memory");

  return result;
}

