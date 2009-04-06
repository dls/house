#ifndef MULTIBOOT_HEADER_MAGIC
#include "multiboot.h"
#endif

extern unsigned long init_modules(unsigned mods_count,module_t *mods_addr);
extern unsigned module_count(void);
extern char *module_name(unsigned n);
extern void *module_start(unsigned n);
extern void *module_end(unsigned n);
