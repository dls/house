// These define standard register contraints for gcc asm directives
#define _EAX "=a"
#define _EBX "=b"
#define _ECX "=c"
#define _EDX "=d"
#define _ESI "=S"
#define _EDI "=D"
// Unfortunately, there doesn't seem to be one for _EBP


void ThreadControl(int dest, int pager, int sched, int ss) { 
  int ign0,ign1,ign2,ign3;
  asm volatile (
        "int $0x90"
        : _EAX (ign0), _ECX (ign1), _EDX (ign2),  _ESI (ign3)
        : "0" (dest), "1" (pager), "2" (sched), "3" (ss)
        : "memory");
}

void tc(int dest, int ss, void* entry, int modnum) {
  int ign0,ign1,ign2,ign3;
  asm volatile (
        "int $0x90"
        : _EAX (ign0), _ECX (ign1), _EDX (ign2),  _ESI (ign3)
        : "0" (dest), "1" ((int)entry), "2" (modnum), "3" (ss)
        : "memory");
}


void fork(int tid, void* entry) {
  int ign0,ign1;
  asm volatile (
        "int $0x90"
        : _EAX (ign0), _EDX (ign1) 
        : "0" (tid), "1" ((int)entry)
        : "memory");

}


void IPC(int to, int time, int from, int tag, struct IPCRet* x) {
  int a, b, c, ign0, ign1,ign2;
  

  /* This is very ugly because gcc seems to ignore a 'clobber' annotation on %ebp,
     so we have to move it out of the way ourselves. (Since there is no
     free register available, we put it on the stack.)
     We could do a bit better with hand-crafted assembler. */
  asm volatile (
	"pushl\t%%ebp\n\tint\t$0x85\n\tmovl\t%%ebp,%5\n\tpopl\t%%ebp" 
        : _EAX(ign0), _ECX(ign1), _EDX (ign2), _ESI(a), _EBX(b), "=r"(c) 
	: "0" (to), "1" (time), "2" (from), "3" (tag) 
        : "memory");   
  x->mr0 = a;
  x->mr1 = b;
  x->mr2 = c;
}

