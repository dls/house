#define MULTIBOOT_BOOTLOADER_MAGIC      0x2BADB002

extern void intxx_raw_handler();
extern void int00_raw_handler();
extern void int02_raw_handler();
extern void int03_raw_handler();
extern void int04_raw_handler();
extern void int05_raw_handler();
extern void int06_raw_handler();
extern void int07_raw_handler();
extern void int08_raw_handler();
extern void int09_raw_handler();
extern void int0A_raw_handler();
extern void int0B_raw_handler();
extern void int0C_raw_handler();
extern void int0D_raw_handler();
extern void int0E_raw_handler();
extern void int10_raw_handler();
extern void int11_raw_handler();
extern void int12_raw_handler();
extern void int13_raw_handler();
extern void irq0_raw_handler();
extern void irq1_raw_handler();
extern void irq2_raw_handler();
extern void irq3_raw_handler();
extern void irq4_raw_handler();
extern void irq5_raw_handler();
extern void irq6_raw_handler();
extern void irq7_raw_handler();
extern void irq8_raw_handler();
extern void irq9_raw_handler();
extern void irqA_raw_handler();
extern void irqB_raw_handler();
extern void irqC_raw_handler();
extern void irqD_raw_handler();
extern void irqE_raw_handler();
extern void irqF_raw_handler();
extern void int80_raw_handler();
extern void int81_raw_handler();
extern void int82_raw_handler();
extern void int83_raw_handler();
extern void int84_raw_handler();
extern void int85_raw_handler();
extern void int86_raw_handler();
extern void int87_raw_handler();
extern void int90_raw_handler();
extern void int91_raw_handler();

void c_print(char const * str);

void setIRQTable(int irq,StgStablePtr sp);

// void *memset(void *d, int c, size_t n);
