module Kernel.Timer(timeIt,readTimer) where
import H.AdHocMem(Ptr,peek)

foreign import ccall unsafe "../cbits/timer.h & timer0_ticks" ticks :: Ptr Int
foreign import ccall unsafe "../cbits/timer.h & ms_per_tick" ms_per_tick :: Ptr Int

{-# NOINLINE readTimer #-}
readTimer = do t <- peek ticks
	       f <- peek ms_per_tick
	       return (f*t)

{-# NOINLINE timeIt #-}
timeIt m = do t0 <- peek ticks
	      result <- m
	      t1 <- peek ticks
	      f <- peek ms_per_tick
	      return (f*(t1-t0),result)
