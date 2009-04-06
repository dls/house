/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsFlags.h"
#include "SchedAPI.h"
#include "Schedule.h"

#ifndef house_HOST_OS
#error Attempt to build House when not building House
#endif

#include "RtsSignals.h"

#define N_PENDING_HANDLERS 16

StgPtr pending_handler_buf[N_PENDING_HANDLERS];
StgPtr *next_pending_handler = pending_handler_buf;

void
blockUserSignals(void)
{
    __asm__("cli");
}

void
unblockUserSignals(void)
{
    extern int kernelInterruptsAllowed;
    if (kernelInterruptsAllowed)
        __asm__("sti");
}

void
startSignalHandlers(Capability *cap)
{
  blockUserSignals();
  
  while (next_pending_handler != pending_handler_buf) {

    next_pending_handler--;

    scheduleThread (cap,
	createIOThread(cap,
		       RtsFlags.GcFlags.initialStkSize, 
		       (StgClosure *) *next_pending_handler));
  }

  unblockUserSignals();
}

/* Useless functions to avoid unnecessary ifdefs */

void
freeSignalHandlers(void)
{
}

void
initUserSignals(void)
{
}

void
initDefaultHandlers(void)
{
}

void awaitUserSignals(void)
{
}

rtsBool anyUserHandlers(void)
{
    return rtsFalse;
}

void
markSignalHandlers(evac_fn evac STG_UNUSED)
{
}

