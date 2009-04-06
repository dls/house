#include "Rts.h"
#include "util.h"
#include "fault.h"

void dumpFaultContext(IA32_FaultContext *p) {
  kprintf("---------------------------------------------\n");
  kprintf("ss: %08x esp: %08x eflags: %08x\n", p->ss, p->esp,p->eflags);
  kprintf("cs: %08x eip: %08x error: %08x\n", p->cs, p->eip,p->error_code);
  kprintf("ds: %08x es: %08x\n", p->ds, p->es);
  kprintf("edi: %08x esi: %08x ebp: %08x esp: %08x\n", p->r.edi, p->r.esi, p->r.ebp, p->r.esp);
  kprintf("ebx: %08x edx: %08x ecx: %08x eax: %08x\n", p->r.ebx, p->r.edx, p->r.ecx, p->r.eax);
  kprintf("---------------------------------------------\n");
}

#define INTXX_DEBUG(c1,c2) { 0[(StgWord16*)0xB8000] = 0x4F00|(c1); 1[(StgWord16*)0xB8000] = 0x4F00|(c2); } while(0)

