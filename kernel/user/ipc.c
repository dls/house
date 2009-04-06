#include "ipc.h"
#include "regs.h"

// FIX: should pass UTCB pointer in EDI for both IPC and LIPC

void IPC(int to, int time, int from, int tag, struct IPCRet* x) {
  int a, b, c, d, ign1, ign2;
  
  // "mov\t%%esp,%%edi\n\tand\t$0xfff00000,%%eax\n\tpushl\t%%ebp\n\tint\t$0x86\n\tmovl\t%%ebp,%5\n\tpopl\t%%ebp"

  asm volatile (
    "pushl\t%%ebp\n\tint\t$0x85\n\tmovl\t%%ebp,%5\n\tpopl\t%%ebp"
    : _EAX(d), _ECX(ign1), _EDX(ign2), _ESI(a), _EBX(b), "=r"(c)
    : "0" (to), "1" (time), "2" (from), "3" (tag)
    : "memory");

  x->mr0  = a;
  x->mr1  = b;
  x->mr2  = c;
  x->from = d;
}

void send(int to, int time, int tag, struct IPCRet* x) {
  IPC(to, time, 0, tag, x);
}

void recv(int from, int time, struct IPCRet* x) {
  IPC(0, time, from, 0, x);
}

void sendEmpty(int to, int time) {
  int tag;
  int us[1];
  struct Msg m;
  struct IPCRet x;
  
  us[0] = 15;

  m.untyped = us;
  tag = setMsg(&m, 1, 0);
  send(to, time, tag, &x);
}

void recvEmpty(int from, int time) {
  struct IPCRet x;

  recv(from, time, &x);
}

void LIPC(int to, int time, int from, int tag, struct IPCRet* x) { 
  int a, b, c, d, ign1, ign2;
  
  asm volatile (
    "pushl\t%%ebp\n\tint\t$0x86\n\tmovl\t%%ebp,%5\n\tpopl\t%%ebp"
    : _EAX(d), _ECX(ign1), _EDX(ign2), _ESI(a), _EBX(b), "=r"(c)
    : "0" (to), "1" (time), "2" (from), "3" (tag)
    : "memory");

  x->mr0  = a;
  x->mr1  = b;
  x->mr2  = c;
  x->from = d;
}

static int* getLocal() {
  __asm__ ("mov %%esp,%%eax" : : : "eax" );
  __asm__ ("and $0xfff00000,%%eax" : : : "eax" );
}

int setMsg(struct Msg* m, int lenus, int lents) {
  int* local = getLocal();
  int i;
  int off = 65;
  int tag;

  for (i = 0; i < lenus; i++) { 
    local[off+i] = (*m).untyped[i];
  }

  tag = (lenus & 0x3f) | ((lents & 0x3f) << 6);
  return tag;
}

int makeFpage(int addr, int size, int read, int write, int exec) {
  int fpage = 0;
  int sizeMask = 0x000003F0;
  int permMask = 0x00000001;

  fpage |= addr << 10;
  fpage |= (size << 4) & sizeMask;
  fpage |= (read & permMask) << 2;
  fpage |= (write & permMask) << 1;
  fpage |= exec & permMask;

  printf("Fpage is: %d\n", fpage);

  return fpage;
}

void makeTyped(int fpage, int read, int write, int exec, int code, int base, struct Typed* t) {
  int permMask = 0x00000001;
  int codeMask = 0x0000000F;
  int newPerms = 0xFFFFFFF0;

  newPerms |= (read & permMask) << 2;
  newPerms |= (write & permMask) << 1;
  newPerms |= exec & permMask;

  t->miPlus1 = fpage & newPerms;
  t->mi = (base << 10) | (code & codeMask);
}
  


