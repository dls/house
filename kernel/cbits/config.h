/* Global configuration parameters; shared by assembler and C */


/* Segment Descriptors */

#define KERNEL_CS	0x08	/* Kernel code segment */
#define	KERNEL_DS	0x10	/* Kernel data segment */
#define USER_CS		0x1B	/* User code segment */
#define	USER_DS		0x23	/* User data segment */
#define	TSS		0x28	/* Trap segment */

#define GDT_SIZE	(6*8)	

#define USER_BITS       0x03    /* One of these bits is set for user mode segment descriptors */

/* Memory Layout */

#define PAGE_SIZE 0x1000            /* in bytes */

#define IDT_BASE 0x00000000
#define C_STACK_BASE 0x90000   
#define C_STACK_SIZE (0x10000 - PAGE_SIZE)  /* top page below 0xA0000 is supposedly missing on some machines */
#define KERNEL_CODE_BASE 0x00100000 /* 1M */
#define PAGE_TABLE_AREA_BASE 0x00800000 /* 8M */
#define PAGE_TABLE_AREA_SIZE 0x80000  /* !!!! was 0x40000 */
#define PAGE_TABLE_ENTRIES (PAGE_SIZE/4)  /* entries per table */
#define C_HEAP_START (PAGE_TABLE_AREA_BASE + PAGE_TABLE_AREA_SIZE)
#define C_HEAP_SIZE  0x00400000
#define HASKELL_HEAP_START (C_HEAP_START + C_HEAP_SIZE)
#define USER_BASE 0x08000000     /* 128M */  /* !!!! was 0x04000000  64M */
#define MEMORY_SIZE  0x0e000000  /* (128+96)M */ /* !!!! was 0x08000000  12M */

#define V_VBE_BASE MEMORY_SIZE      /* currently assume these are contiguous */
#define MAX_VBE_SIZE 0x02000000     /* 32M */
#define V_USER_BASE 0x10000000
