#include "Rts.h"
#include <string.h>
#include "ia32.h"
#include "config.h"
#include "util.h"
#include "segments.h"
#include "fault.h"
#include "userspace.h"

const unsigned int *min_user_addr = (unsigned int *) USER_BASE;
const unsigned int *max_user_addr = (unsigned int *) (MEMORY_SIZE - 1);

////////////////////////////////////////////////////////////////////////
// 
// User Fault Context
//

tks_struct task_kernel_stack __attribute__ ((aligned(PAGE_SIZE)));

IA32_FaultContext *task_fault_context = &task_kernel_stack.fault_context;


////////////////////////////////////////////////////////////////////////
// 
// TSS Management
//

static void
fill_descriptor(x86_desc *desc, unsigned base, unsigned limit,
		unsigned char access, unsigned char sizebits)
{
	desc->limit_low = limit & 0xffff;
	desc->base_low = base & 0xffff;
	desc->base_med = (base >> 16) & 0xff;
	desc->access = access;
	desc->limit_high = limit >> 16;
	desc->granularity = sizebits;
	desc->base_high = base >> 24;
}

x86_tss tss;

extern x86_desc gdt[];  // defined in start.S

static void dump_gdt() {
  kprintf("gdt:%08x\n",gdt);
  // assume gdtr is correctly set
  int i;
  unsigned int *g = (unsigned int *)(gdt);
  for (i = 0; i < GDT_SIZE/(sizeof(x86_desc)); i++) 
    kprintf("%d:%08x %08x\n",i,g[i*2],g[i*2+1]);
}

void init_tss() {
  fill_descriptor(&gdt[SEGMENT_NUMBER(TSS)],(unsigned) (&tss),sizeof(tss) - 1, 0x89,0);  
  tss.ss0 = KERNEL_DS; 
  tss.esp0 = ((int) &task_kernel_stack) + PAGE_SIZE;  
  tss.io_bit_map_offset = sizeof(tss);
  unsigned short tssd = TSS;
  dump_gdt();
  __asm__ __volatile__("ltr %0" : : "r" (tssd));
}

// todo: maybe use fill_descriptor to initialize USER_DS and USER_CS too.


////////////////////////////////////////////////////////////////////////
// 
// Paging setup
//
// Map the entire physical memory space for the kernel.
// Page directory is immediately followed by page tables.
//
void init_paging() {
  int i,j;
  unsigned int const page_directory_base = PAGE_TABLE_AREA_BASE;
  unsigned int const page_tables_base = PAGE_TABLE_AREA_BASE + PAGE_SIZE;
  int const number_of_page_tables = (MEMORY_SIZE+MAX_VBE_SIZE)/(PAGE_SIZE * PAGE_TABLE_ENTRIES);
  if ((number_of_page_tables + 1) * PAGE_SIZE > PAGE_TABLE_AREA_SIZE)
    kprintf("Not enough page table space!");
  // set up page directory
  unsigned int *p = (unsigned int *) page_directory_base;
  unsigned int q = page_tables_base;
  for (i = 0; i < number_of_page_tables; i++) {
    *p++ = q | 0x3;
    q+= PAGE_SIZE;
  }
  for (; i < PAGE_TABLE_ENTRIES; i++) 
    *p++ = 0;
  // set up all the page tables
  // note: tables associated with the VBE frame buffer will be altered later
  p = (unsigned int *) page_tables_base;
  q = 0; 
  for (i = 0; i < number_of_page_tables; i ++) {
    for (j = 0; j < PAGE_TABLE_ENTRIES; j++) {
      *p++ = q | 0x3;
      q+= PAGE_SIZE;
    }
  }
  // start paging!
  __asm__ __volatile__("movl %0,%%cr3": : "r" (page_directory_base));
  __asm__ __volatile__("movl %%cr0,%%eax; orl $0x80000000,%%eax; movl %%eax,%%cr0" : : : "eax");
  __asm__ __volatile__("ljmp %0,$1f\n1:\tnop": : "i" (KERNEL_CS));
}
  
void remap_kernel_range(unsigned int vaddr, unsigned int paddr, unsigned int size) {
  // slow but (?) sure
  unsigned int *pdir = (unsigned int *)PAGE_TABLE_AREA_BASE;
  while (size > 0) {
    int pdir_entry = (vaddr >> 22) & 0x3ff;
    int *ptable = (int *) (pdir[pdir_entry] & 0xfffff000);
    int ptable_entry = (vaddr >> 12) & 0x3ff;
    ptable[ptable_entry] = (paddr & 0xfffff000) | 0x3;
    vaddr += PAGE_SIZE;
    paddr += PAGE_SIZE;
    size -= PAGE_SIZE;
  }
}

// Initialize a pagedir.  Initial contents are just a copy of the kernel's entries.
void init_page_dir(unsigned int *pdir) {
  int i;
  for (i = 0; i < PAGE_TABLE_ENTRIES; i++) 
    pdir[i] = ((unsigned int *)PAGE_TABLE_AREA_BASE)[i];
}


////////////////////////////////////////////////////////////////////////
//
// User Process execution
//
//

void * volatile current_return_ksp;  // where to reset the kernel sp when returning to Haskell

unsigned int volatile last_fault_addr; // address of last fault; 0 if last return didn't cause fault

unsigned char execute(unsigned int *pdir) {
  
  // WARNING!: Don't make any function calls (even for debugging) within this routine!!

  __asm__ __volatile__ ("cli");  // just to be on the safe side, since things may get messy hereafter

  // force all callee-save registers onto the stack 
  __asm__  __volatile__ ("" ::: "ebx","esi","edi");

  // remember ksp for return
  __asm__ __volatile__("movl %%esp,%0": "=m"(current_return_ksp));

  // reset pdir if necessary 
  {
    void *existing_pdir = 0;
    __asm__ __volatile__("movl %%cr3,%0" : "=&r"(existing_pdir));
    if (pdir != existing_pdir) {
      // reset pdir
      __asm__ __volatile__("movl %0,%%eax; movl %%eax,%%cr3": : "m"(pdir) : "%eax");
      __asm__ __volatile__("ljmp %0,$1f\n1:\tnop" : : "i" (KERNEL_CS));
    }
    last_fault_addr = 0;  
  }

  // fill in the standard fields
  task_fault_context->ss = USER_DS;
  task_fault_context->cs = USER_CS;
  task_fault_context->ds = USER_DS;
  task_fault_context->es = USER_DS;

  // restore the world from the fault context and "return"
  __asm__ __volatile__("movl %0,%%esp" : : "m" (task_fault_context));
  __asm__ __volatile__ ("popal; popl %%es; popl %%ds; addl $8, %%esp; iretl"::);

  // return is elsewhere
  return 0;  // never executed
}

extern int kernelInterruptsAllowed;

void return_from_execute(void *ksp,int code) {
  if (kernelInterruptsAllowed) 
    __asm__ __volatile__ ("sti");
  // At this point, we've calculated the return code 
  __asm__ __volatile__ ("movl %0,%%eax" : : "r"(code) : "eax");
  // hack hack: this needs to match return sequence for execute; hopefully we've guranteed this
  __asm__ __volatile__ ("movl %0,%%esp; movl (%%esp),%%ebx; movl 4(%%esp),%%esi; movl 8(%%esp),%%edi; addl $12,%%esp; popl %%ebp; ret" : : "m"(ksp));
}  

void user_fault(int fault,IA32_FaultContext* p) {
  if (p != task_fault_context) {
    kprintf("user_fault oops %X %X\n",p,task_fault_context);
    abort();
  }
  return_from_execute(current_return_ksp,fault);
}

void user_timer(IA32_FaultContext* p) {
  if (p != task_fault_context) {
    kprintf("user_fault oops %X %X\n",p,task_fault_context);
    abort();
  }
  void *ksp = current_return_ksp;
  outb(0x20, 0x20);  // ideally, would be kernel's responsibility, but that doesn't seem to work,
                     // perhaps because we do a gc (and hence a thread switch) immediately upon
                     // returning to Haskell-land?
  // __asm__ __volatile__ ("sti");  // (HAS ALREADY HAPPENED) re-enable interrupts 
  return_from_execute(ksp,0x20);
}

void user_pagefault(unsigned int addr,IA32_FaultContext* p) {
  // kprintf("pagefault %X %X %X %X\n",addr,current_pblock,current_pblock->ksp,p);
  if (p != task_fault_context) {
    kprintf("user_fault oops %X %X\n",p,task_fault_context);
    abort();
  }
  last_fault_addr = addr;
  return_from_execute(current_return_ksp,0x0E);
}

// get current pdir
unsigned int *current_pdir() {
  unsigned int *pdir = 0;
  __asm__ __volatile__("movl %%cr3,%0" : : "r"(pdir));
  return pdir;
}

// invalidate an entry in the current pdir
void invalidate_page(unsigned int addr) {
    __asm__ __volatile__("invlpg (%0)": : "r"(addr));
}  
