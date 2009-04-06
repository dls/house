#include "Rts.h"

////////////////////////////////////////////////////////////////////////
//
// IA32 Assembly bits
//

#define inb(dx) ({StgWord8 r; __asm__ __volatile__("inb %%dx, %%al":"=a"(r):"d"(dx)); r;})

#define outb(dx,al) {StgWord8 r; __asm__ __volatile__("outb %%al, %%dx;jmp 1f;1:"::"a"(al),"d"(dx));} while (0)
