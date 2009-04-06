/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "OSMem.h"

lnat
getPageSize(void)
{
    return 0x1000; /* PAGE_SIZE from House cbits */
}

void
setExecutable(void *p, lnat len, rtsBool exec)
{
    barf("This shouldn't ever happen in House");
}

