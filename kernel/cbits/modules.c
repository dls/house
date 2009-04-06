#include "multiboot.h"
#include "modules.h"

#if 0
/* This is now obsolete... /TH */

static struct {
  unsigned count;
  module_t *mods;
} modules;

unsigned module_count(void)        { return modules.count; }
char *   module_name (unsigned n)
{
  return n<modules.count ? (char *)modules.mods[n].string : "";
}

void *   module_start(unsigned n)
{
  return n<modules.count ? (void *)modules.mods[n].mod_start : 0;
}

void *   module_end  (unsigned n)
{
  return n<modules.count ? (void *)modules.mods[n].mod_end : 0;
}
#endif

unsigned long init_modules(unsigned mods_count,module_t * mods_addr)
{
  int i;
  unsigned long mend = 0;

#if 0
  modules.count=mods_count;
  modules.mods=mods_addr;
#endif

  for (i = 0; i < mods_count; i++)
    mend = mods_addr[i].mod_end > mend ? mods_addr[i].mod_end : mend;
  return mend;
}
