#include "Rts.h"
#include "Schedule.h"
#include <string.h>
#include "start.h"
#include "config.h"
#include "util.h"
#include "ia32.h"
#include "video.h"
#include "multiboot.h"
#include "vbe.h"
#include "gfx.h"
#include "modules.h"
#include "segments.h"
#include "fault.h"
#include "userspace.h"
#include "mib.h"
#include "timer.h"

////////////////////////////////////////////////////////////////////////
//
// C Heap Management
//

void * sbrk(unsigned long increment) {
  static void * brk = (void *)C_HEAP_START;
  void * old_brk = brk;
  void * new_brk = old_brk + increment;
  if (new_brk < (void *) C_HEAP_START ||
      new_brk > ((void *) (C_HEAP_START + C_HEAP_SIZE))) {
    return (void *) -1;
  } else {
    brk = new_brk;
    return old_brk;
  }
}

////////////////////////////////////////////////////////////////////////
//
// IA32 Faults and Interrupts
//


#define USER_MODE(p) (USER_SEGMENT_DESC((p)->cs))

#define INTXX_DEBUG(c1,c2) { 0[(StgWord16*)0xB8000] = 0x4F00|(c1); 1[(StgWord16*)0xB8000] = 0x4F00|(c2); } while(0)

void do_fault(int fault,IA32_FaultContext* p) {
  if (USER_MODE(p)) 
    user_fault(fault,p);
  else {
    kprintf("!!!Fault %02x\n",fault);
    dumpFaultContext(p);
    INTXX_DEBUG('0' + fault/0x10, '0' + fault % 0x10);
    abort();
  }
}

void int00_handler(IA32_FaultContext* p) {
  // Divide Error
  do_fault(0x00,p);
}

void int02_handler(IA32_FaultContext* p) {
  // NMI
  do_fault(0x02,p);
}

void int03_handler(IA32_FaultContext* p) {
  // Breakpoint
  do_fault(0x03,p);
}

void int04_handler(IA32_FaultContext* p) {
  // Overflow
  do_fault(0x04,p);
}

void int05_handler(IA32_FaultContext* p) {
  // Bound Range Checking
  do_fault(0x05,p);
}

void int06_handler(IA32_FaultContext* p) {
  // Invalid Opcode
  do_fault(0x06,p);
}

void int07_handler(IA32_FaultContext* p) {
  // Device Not Available
  do_fault(0x07,p);
}

void int08_handler(IA32_FaultContext* p) {
  // Double Fault
  do_fault(0x08,p);
}

void int0A_handler(IA32_FaultContext* p) {
  // Invalid TSS
  do_fault(0x0A,p);
}

void int0B_handler(IA32_FaultContext* p) {
  // Segment Not Present
  do_fault(0x0B,p);
}

void int0C_handler(IA32_FaultContext* p) {
  // Stack-Segment Fault
  do_fault(0x0C,p);
}

void int0D_handler(IA32_FaultContext* p) {
  // General Protection Fault
  do_fault(0x0D,p);
}


void int0E_handler(IA32_FaultContext* p) {
  // Page Fault
  unsigned int cr2;
  __asm__ __volatile__ ("movl %%cr2,%0":"=r" (cr2));
  // kprintf("into page fault @ %08x address:%08x error code:%08x\n",p->eip,cr2,p->error_code);
  // dumpFaultContext(p);
  if (USER_MODE(p)) 
    user_pagefault(cr2,p);
  else {
    kprintf("!!! Bad page fault @ %08x address:%08x error code:%08x\n",p->eip,cr2,p->error_code);
    do_fault(0x0E,p);
  }
}

void int10_handler(IA32_FaultContext* p) {
  // FPU
  do_fault(0x10,p);
}

void int11_handler(IA32_FaultContext* p) {
  // Alignment Check
  do_fault(0x11,p);
}

void int12_handler(IA32_FaultContext* p) {
  // Machine Check
  do_fault(0x12,p);
}

void int13_handler(IA32_FaultContext* p) {
  // SIMD
  do_fault(0x13,p);
}

void int80_handler(IA32_FaultContext* p) {
  // kernel interface
  do_fault(0x80,p);
}  

void int81_handler(IA32_FaultContext* p) {
  // exchange registers
  do_fault(0x81,p);
}

void int82_handler(IA32_FaultContext* p) {
  // system clock
  do_fault(0x82,p);
}

void int83_handler(IA32_FaultContext* p) {
  // thread switch
  do_fault(0x83,p);
}
  
void int84_handler(IA32_FaultContext* p) {
  // scheduling
  do_fault(0x84,p);
}
  
void int85_handler(IA32_FaultContext* p) {
  // ipc
  do_fault(0x85,p);
}
  
void int86_handler(IA32_FaultContext* p) {
  // lipc
  do_fault(0x86,p);
}
  
void int87_handler(IA32_FaultContext* p) {
  // unmap
  do_fault(0x87,p);
}
  
void int90_handler(IA32_FaultContext* p) {
  // thread control
  do_fault(0x90,p);
}
  
void int91_handler(IA32_FaultContext* p) {
  // space control
  do_fault(0x91,p);
}

struct s_idt_init {
  StgWord8  vector;
  void*     handler;
  StgWord8  type;
} idt_init[] = {
  { 0x00, &int00_raw_handler, 0x8E },
  { 0x02, &int02_raw_handler, 0x8E },
  { 0x03, &int03_raw_handler, 0xEE },
  { 0x04, &int04_raw_handler, 0xEE },
  { 0x05, &int05_raw_handler, 0xEE },
  { 0x06, &int06_raw_handler, 0x8E },
  { 0x07, &int07_raw_handler, 0x8E },
  { 0x08, &int08_raw_handler, 0x8E },
  { 0x0A, &int0A_raw_handler, 0x8E },
  { 0x0B, &int0B_raw_handler, 0x8E },
  { 0x0C, &int0C_raw_handler, 0x8E },
  { 0x0D, &int0D_raw_handler, 0x8E },
  { 0x0E, &int0E_raw_handler, 0x8E }, 
  { 0x10, &int10_raw_handler, 0x8E },
  { 0x11, &int11_raw_handler, 0x8E },
  { 0x12, &int12_raw_handler, 0x8E },
  { 0x13, &int13_raw_handler, 0x8E },
  { 0x20, &irq0_raw_handler, 0x8E },
  { 0x21, &irq1_raw_handler, 0x8E },
  { 0x22, &irq2_raw_handler, 0x8E },
  { 0x23, &irq3_raw_handler, 0x8E },
  { 0x24, &irq4_raw_handler, 0x8E },
  { 0x25, &irq5_raw_handler, 0x8E },
  { 0x26, &irq6_raw_handler, 0x8E },
  { 0x27, &irq7_raw_handler, 0x8E },
  { 0x28, &irq8_raw_handler, 0x8E },
  { 0x29, &irq9_raw_handler, 0x8E },
  { 0x2A, &irqA_raw_handler, 0x8E },
  { 0x2B, &irqB_raw_handler, 0x8E },
  { 0x2C, &irqC_raw_handler, 0x8E },
  { 0x2D, &irqD_raw_handler, 0x8E },
  { 0x2E, &irqE_raw_handler, 0x8E },
  { 0x2F, &irqF_raw_handler, 0x8E },
  { 0x80, &int80_raw_handler, 0xEE }, 
  { 0x81, &int81_raw_handler, 0xEE },
  { 0x82, &int82_raw_handler, 0xEE },
  { 0x83, &int83_raw_handler, 0xEE },
  { 0x84, &int84_raw_handler, 0xEE },
  { 0x85, &int85_raw_handler, 0xEE },
  { 0x86, &int86_raw_handler, 0xEE },
  { 0x87, &int87_raw_handler, 0xEE },
  { 0x90, &int90_raw_handler, 0xEE },
  { 0x91, &int91_raw_handler, 0xEE },
  { 0x00, 0, 0 }
};

void setInterruptGate(void* base, StgWord8 n, void* offset, StgWord8 type) {
  StgWord32* p = base + n * 8;
  StgWord32 cs;
  __asm__ __volatile__ ("pushl %%cs ; popl %0":"=r"(cs));
  p[0] = (cs << 16) | ((unsigned)offset & 0xFFFF);
  p[1] = ((unsigned)offset & 0xFFFF0000) | ((unsigned)type << 8) | 0x00;
}

struct s_idtr {
  StgWord16 pad;
  StgWord16 limit;
  void* base;
};

void initIDT() {
  struct s_idtr idt;
  unsigned n = 0;
  __asm__ __volatile__ ("cli");
  idt.limit = 256 * 8;
  idt.base  = IDT_BASE;
  memset(idt.base, 0, idt.limit);
  while (0 != idt_init[n].handler) {
    setInterruptGate(idt.base, idt_init[n].vector, idt_init[n].handler,
                     idt_init[n].type);
    ++n;
  }
  // __asm__ __volatile__ ("lidt 0(%0); sti": : "r" ((void *) &idt.limit));
  __asm__ __volatile__ ("lidt 0(%0)": : "r" ((void *) &idt.limit));
}

////////////////////////////////////////////////////////////////////////
//
// IRQs
//

StgStablePtr irqHandlerThreads[16] =
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
int irqHandlerInstalled[16] = // booleans
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };


void initPIC(StgWord8 pic1, StgWord8 pic2)
{
  // configure PICs and mask all IRQs
  /* ICW 1 */
  outb(0x20, 0x11);   outb(0xA0, 0x11);
  /* ICW 2 */
  outb(0x21, pic1);   outb(0xA1, pic2);
  /* ICW 3 */
  outb(0x21, 4);      outb(0xA1, 2);
  /* ICW 4 */
  outb(0x21, 0x05);   outb(0xA1, 0x01);
  /* OCW 1 */
  outb(0x21, 0xFF);   outb(0xA1, 0xFF);
}

void onIRQ0(IA32_FaultContext* p);

// new stuff:
#define N_PENDING_HANDLERS 16
extern StgPtr *pending_handler_buf;
extern StgPtr *next_pending_handler;
void setIRQTable(int irq,StgStablePtr sp) {
  irqHandlerInstalled[irq] = 1;
  irqHandlerThreads[irq] = sp;
}

void irq_handler(IA32_FaultContext* p) {
  unsigned n = (unsigned)(p->error_code);
  if (0 == n) 
    onIRQ0(p);
  else {
    if (n < 16) {
      // kprintf("Received interrupt %u (%X) \n", n, irqHandlerThreads[n]);
      if (USER_MODE(p)) 
	// return interrupt immediately 
	user_fault(0x20+n,p);
      else if (irqHandlerInstalled[n]) {
	// treat like a signal, queueing handler for execution when GHC RTS finds convenient
	StgStablePtr sp = irqHandlerThreads[n];
	*next_pending_handler++ = deRefStablePtr(sp);
	if (next_pending_handler == &pending_handler_buf[N_PENDING_HANDLERS]) {
	  kprintf("Too many pending signal (interrupt) handlers\n");
	  abort();
	}
	context_switch = 1;
#if 0   // EOI will be done after handler actually executes
	if (n >= 8) {
	  outb(0xA0, 0x20);
	}
	outb(0x20, 0x20);
#endif
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////
//
// Timer 0
//

typedef void (*TickProc)(int);
static volatile TickProc timer0_proc;
unsigned long timer0_ticks;
unsigned ms_per_tick;

void onIRQ0(IA32_FaultContext* p) {
  0[(short*)0xB8000] = 0x4F00 | ('0' + timer0_ticks % 10);
  ++timer0_ticks;
  if (0 != timer0_proc)
    timer0_proc(0);
  if (USER_MODE(p)) 
    user_timer(p);
  else
    outb(0x20, 0x20); /* EOI */
}

/* Called by Timer.c initTimer() */
int
initTicker(unsigned ms, TickProc handle_tick) {
  ms_per_tick = ms;
  timer0_proc=handle_tick;
  /* set timer 0 frequency */
  StgWord16 ticks = 119319 * ms / 100;
  outb(0x40, (StgWord8) ticks);
  outb(0x40, (StgWord8) (ticks >> 8));
  /* enable IRQ 0 */
  outb(0x21, 0xfe & inb(0x21));
  return 0;
}

int
exitTicker()
{
  /* disable IRQ 0 */
  outb(0x21, 0x01 | inb(0x21));
  return 0;
}

unsigned int
getourtimeofday(void) {
  return timer0_ticks;
}

////////////////////////////////////////////////////////////////////////
//
// Entry point from assembly
//

extern void MBlock_init(void * heap, void * limit);
extern int main(int argc, char *argv[]);

multiboot_info_t *multibootinfo;

void c_start(unsigned long magic, multiboot_info_t *mbi) {
  int argc = 3;
  /* Warning: these are ignored by the STANDALONE version of the GHC RTS */
  char* argv[] = { "hOp", "+RTS", "-Dg", 0 };

  initPIC(0x20, 0x28);
  initIDT();
#ifndef BOCHS_VIDEO
  init_video();
#endif
#if 1
  init_tss();
  init_paging();
#endif
  MBlock_init((void*)HASKELL_HEAP_START, (void*)USER_BASE);
  if(magic != MULTIBOOT_BOOTLOADER_MAGIC) {
    kprintf ("Invalid magic number: 0x%x\n", magic);
  }
  else {
    multibootinfo=mbi;
    if(mbi->flags & (1<<11)) {
      /* Assume we got a color graphics mode with a linear frame buffer. */
      /* linear frame buffer is at mib->phys_base_ptr, size in bytes is
	 mib->x_resolution*((mib->bits_per_pixel+7)/8)*mib->y_resolution
	 (Assuming VBE 2.0 compatibility)
	 Or, supposedly, we can look at the cib
      */
      vbe_modeinfoblock_t *vbe_mib = (vbe_modeinfoblock_t *) mbi->vbe_mode_info;
      vbe_controlinfoblock_t *vbe_cib = (vbe_controlinfoblock_t *) mbi->vbe_control_info;
      int vbe_size = vbe_cib->totalmemory << 16;
      if (vbe_size > MAX_VBE_SIZE)
	kprintf ("VBE frame buffer size too large: 0x%x\n", vbe_size);
      else {
	remap_kernel_range(V_VBE_BASE,vbe_mib->phys_base_ptr,vbe_size);
	vbe_mib->phys_base_ptr = V_VBE_BASE;
	init_gfx(vbe_mib,vbe_cib);
	set_color(0,0,0x4000);
	fill_rectangle(0,0,gfx_width(),gfx_height());
      }
    }
    /* we don't do anything with this info at the moment; should use it to size memory */
    if(mbi->flags & (1<<6)) {
      kprintf("Memory map:\n");
      memory_map_t *m = ((memory_map_t *)mbi->mmap_addr);
      int length = (int)mbi->mmap_length;
      while (length > 0) {
	kprintf("addr_lo: %08x addr_hi: %08x len_lo: %08x len_hi: %08x typ: %d\n",
	        m->base_addr_low, m->base_addr_high, m->length_low, m->length_high, m->type);
	length -= (m->size + sizeof(m->size));
	m = (memory_map_t *) (((unsigned) m) + m->size + sizeof(m->size));
      }
    }
    if(mbi->flags & (1<<3)) 
      if (init_modules(mbi->mods_count,(module_t *)mbi->mods_addr) >= PAGE_TABLE_AREA_BASE) {
	kprintf ("Too many modules loaded");
	abort();
      }
  }
  main(argc, argv);
  kprintf("Exited.\n");
  exit(0);
}

////////////////////////////////////////////////////////////////////////
//
// Misc.
//

int select(int n, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout) {
  return 0;
}
