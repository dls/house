#ifndef FAULT_H
#define FAULT_H

struct s_pusha {
  StgWord32 edi;
  StgWord32 esi;
  StgWord32 ebp;
  StgWord32 esp;
  StgWord32 ebx;
  StgWord32 edx;
  StgWord32 ecx;
  StgWord32 eax;
};

typedef struct s_IA32_FaultContext {
  struct s_pusha r;
  StgWord32 es;
  StgWord32 ds;
  StgWord32 _handler;   // junk if interrupt
  StgWord32 error_code; // irq# if interrupt
  StgWord32 eip;
  StgWord32 cs;
  StgWord32 eflags;
  StgWord32 esp; // only if faulted from lower privilege level
  StgWord32 ss;  // only if faulted from lower privilege level
  StgWord32 stack[];
} IA32_FaultContext;


void dumpFaultContext(IA32_FaultContext *p);

#endif
