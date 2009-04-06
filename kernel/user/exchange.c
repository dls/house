
#include "exchange.h"
#include "regs.h"

// FIX: need to pass "user defined handle" in EBX but GCC complains it is 
// out of general purpose registers

void ExchangeRegisters(int dest, int control, int sp, int ip, 
                       int flags, int udh, int pager, struct ExRegRet* x) {
  int result, r_control, r_sp, r_ip, r_flags, r_udh, r_pager;

  asm volatile (
    "pushl\t%%ebp\n\tint\t$0x81\n\tmovl\t%%ebp,%5\n\tpopl\t%%ebp"
    : _EAX(result), _ECX(r_control), _EDX(r_sp), _ESI(r_ip), _EDI(r_flags), "=r"(r_pager)
    : "0" (dest), "1" (control), "2" (sp), "3" (ip), "4" (flags), "5" (pager)
    : "memory");

  x->result  = result;
  x->control = r_control; 
  x->sp      = r_sp;
  x->ip      = r_ip;
  x->flags   = r_flags;
//  x->udb     = r_udh;
}  

