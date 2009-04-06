// nested includes needed because FFI only allows one .h to be named (I think).
#include "config.h" 
#include "fault.h" 
#include "segments.h" 

extern const unsigned int *min_user_addr;
extern const unsigned int *max_user_addr;

typedef struct {
  unsigned char filler[PAGE_SIZE - sizeof(IA32_FaultContext)];  
  IA32_FaultContext fault_context;
} tks_struct;
extern tks_struct task_kernel_stack __attribute__ ((aligned(PAGE_SIZE)));
extern IA32_FaultContext *task_fault_context;
void init_tss();
void init_paging();
void remap_kernel_range(unsigned int vaddr, unsigned int paddr, unsigned int size);
void init_page_dir(unsigned int *pdir);

extern void * volatile current_return_ksp;  // where to reset the kernel sp when returning to Haskell
extern unsigned int volatile last_fault_addr; // address of last fault; 0 if last return didn't cause fault

unsigned char execute(unsigned int *pdir);

void user_fault(int fault,IA32_FaultContext* p);
void user_pagefault(unsigned int addr,IA32_FaultContext* p);
void user_timer(IA32_FaultContext* p);

unsigned int *current_pdir();

void invalidate_page(unsigned int addr);

