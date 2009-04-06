#include "io.h"

// do we need this?
// void slow() { __asm__ __volatile__ ("jmp 1f; 1: jmp 1f; 1:" : : ); }
#define slow() 

Byte inb(Port port) { 
  Byte value;
  __asm__ __volatile__ ("inb %%dx, %%al" : "=a"(value) : "d"(port)); 
  slow();
  return value; 
}  

Word inw(Port port) { 
  Word value;          
  __asm__ __volatile__ ("inw %%dx, %%ax" : "=a"(value) : "d"(port)); 
  slow();
  return value; 
}  

Long inl(Port port) { 
  Long value;          
  __asm__ __volatile__ ("inl %%dx, %%eax" : "=a"(value) : "d"(port)); 
  slow();
  return value; 
}  

void outb (Port port, Byte value) { 
  __asm__ __volatile__ ("outb %%al, %%dx" : : "d"(port), "a"(value));
  slow();
} 

void outw (Port port, Word value) { 
  __asm__ __volatile__ ("outw %%ax, %%dx" : : "d"(port), "a"(value));
  slow();
} 

void outl (Port port, Long value) { 
  __asm__ __volatile__ ("outl %%eax, %%dx" : : "d"(port), "a"(value));
  slow();
} 

int kernelInterruptsAllowed = 0;

void enableInterrupts() {
  kernelInterruptsAllowed = 1;
  __asm__ __volatile__ ("sti");
}

void disableInterrupts() {
  __asm__ __volatile__ ("cli");
  kernelInterruptsAllowed = 0;
}
