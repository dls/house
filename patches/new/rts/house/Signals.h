/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HOUSE_SIGNALS_H
#define HOUSE_SIGNALS_H

extern rtsBool anyUserHandlers(void);

extern StgPtr pending_handler_buf[];
extern StgPtr *next_pending_handler;
#define signals_pending() (next_pending_handler != pending_handler_buf)
void startSignalHandlers(Capability *cap);

#endif /* HOUSE_SIGNALS_H */

