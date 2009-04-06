#include "config.h"	

	/* Multiboot header -- this must be in the first 8k  */
	/* of the image file.                                */

#define GRUB_MAGIC      0x1BADB002      /* Magic number */
#define GRUB_FLAGS      0x00000003      /* Flags        */

	.text        
	.align	4, 0xFF
	.long   GRUB_MAGIC
	.long   GRUB_FLAGS
	.long   0-GRUB_MAGIC-GRUB_FLAGS /* Checksum */

	/* The startup code */
        .extern c_start
        .global start
start:
	movl	%eax,%edx          /* Save multiboot magic value */
        leal    gdt_ptr, %eax
        lgdtl   0(%eax)
        ljmp    $KERNEL_CS, $1f
1:      movl    $KERNEL_DS, %eax
        movl    %eax, %ss
        movl    %eax, %ds
        movl    %eax, %es
        xorl    %eax, %eax
        movl    %eax, %fs
        movl    %eax, %gs
        movl    $C_STACK_BASE, %edi          /* Memory initialization start address     */
        movl    $(C_STACK_SIZE/4), %ecx      /* Number of 32 bits words to fill         */
        movl    $0xCCCCCCCC, %eax            /* Fill with 0xCC (breakpoint instruction) */
        repnz stosl
        movl    $(C_STACK_BASE+C_STACK_SIZE), %eax    
        movl    %eax, %esp
	
	pushl   %ebx	           /* Push the pointer to the Multiboot information structure. */
	pushl   %edx		   /* Push the magic value. */
        call     c_start
halt:	hlt
	jmp halt

	
	/* The GDT. */
        .align  8
        .word   0
gdt_ptr:
        .word   (GDT_SIZE-1)      /* limit of table */
        .long   gdt               /* base of table */
	.global gdt
gdt:
        .quad 0x0000000000000000  /* segment 0:	 empty */
	.quad 0x00cf9a000000ffff  /* segment 1:	 base= 0, G=1, limit = 4GB, S = 1 (user), DPL=0 */
				  /* 		 type = a (code,execute/read) */
			          /*             D/B = 1 (32bits), AVL = 0    */
	.quad 0x00cf92000000ffff  /* segment 2:	 same, but type = 2 (data,read/write) */
	.quad 0x00cffa000000ffff  /* segment 3:	 same as 1, but DPL=3 */
	.quad 0x00cff2000000ffff  /* segment 4:	 same as 2, but DPL=3 */
	.quad 0x0000000000000000  /* segment 5:	 TSS: filled in later */
	

#define INT_RAW_HANDLER(x) \
        .align 4;                               \
        .global int##x##_raw_handler;           \
        .extern int##x##_handler;               \
int##x##_raw_handler:;                          \
        pushl $0xDEADC0DE;                      \
        pushl $int##x##_handler;                \
        jmp fault_raw_handler

#define INT_RAW_HANDLER_EC(x) \
        .align 4;                               \
        .global int##x##_raw_handler;           \
        .extern int##x##_handler;               \
int##x##_raw_handler:;                          \
        pushl $int##x##_handler;                \
        jmp fault_raw_handler

#define IRQ_RAW_HANDLER(n) \
        .align 4;                               \
        .global irq##n##_raw_handler;           \
irq##n##_raw_handler:;                          \
        pushl $0x##n;                           \
        jmp irq_raw_handler

INT_RAW_HANDLER(00)
INT_RAW_HANDLER(02)
INT_RAW_HANDLER(03)
INT_RAW_HANDLER(04)
INT_RAW_HANDLER(05)
INT_RAW_HANDLER(06)
INT_RAW_HANDLER(07)
INT_RAW_HANDLER_EC(08)
INT_RAW_HANDLER_EC(0A)
INT_RAW_HANDLER_EC(0B)
INT_RAW_HANDLER_EC(0C)
INT_RAW_HANDLER_EC(0D)
INT_RAW_HANDLER_EC(0E)
INT_RAW_HANDLER(10)
INT_RAW_HANDLER_EC(11)
INT_RAW_HANDLER(12)
INT_RAW_HANDLER(13)
IRQ_RAW_HANDLER(0)
IRQ_RAW_HANDLER(1)
IRQ_RAW_HANDLER(2)
IRQ_RAW_HANDLER(3)
IRQ_RAW_HANDLER(4)
IRQ_RAW_HANDLER(5)
IRQ_RAW_HANDLER(6)
IRQ_RAW_HANDLER(7)
IRQ_RAW_HANDLER(8)
IRQ_RAW_HANDLER(9)
IRQ_RAW_HANDLER(A)
IRQ_RAW_HANDLER(B)
IRQ_RAW_HANDLER(C)
IRQ_RAW_HANDLER(D)
IRQ_RAW_HANDLER(E)
IRQ_RAW_HANDLER(F)
INT_RAW_HANDLER(80)
INT_RAW_HANDLER(81)
INT_RAW_HANDLER(82)
INT_RAW_HANDLER(83)
INT_RAW_HANDLER(84)
INT_RAW_HANDLER(85)
INT_RAW_HANDLER(86)
INT_RAW_HANDLER(87)
INT_RAW_HANDLER(90)
INT_RAW_HANDLER(91)

fault_raw_handler:
	cld
	pushl %ds
	pushl %es
        pushal
        pushl %esp
	movl $KERNEL_DS,%eax
	movl %eax,%ds
	movl %eax,%es
        call *44(%esp)
        addl $4, %esp
        popal
	popl %es
	popl %ds
        addl $8, %esp
        iretl

irq_raw_handler:
	cld
	subl $4,%esp
	pushl %ds
	pushl %es
        pushal
	pushl %esp		
	movl $KERNEL_DS,%eax
	movl %eax,%ds
	movl %eax,%es
        call irq_handler
        addl $4, %esp
        popal
	popl %es
	popl %ds
        addl $8, %esp
        iretl
