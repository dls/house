module Kernel.Interrupts
    ( registerIRQHandler
     , callIRQHandler      
    ) where

import H.Monad(H)
import H.Mutable(HArray,newArray,readArray,writeArray)
import H.Unsafe(unsafePerformH)
import H.Interrupts(IRQ,enableIRQ,eoiIRQ,installHandler)


{-# NOINLINE irqTable #-}
{- Keep a Haskell ghost copy of irqTable to handle user mode interrupts -}
irqTable :: HArray IRQ (Maybe (H ()))
irqTable = unsafePerformH (newArray (minBound,maxBound) Nothing)

registerIRQHandler irq handler =
    do writeArray irqTable irq (Just wrapper)
       installHandler irq wrapper
       enableIRQ irq
    where
      wrapper = 
         do handler
            eoiIRQ irq

callIRQHandler irq = 
      do mHandler <- readArray irqTable irq
         case mHandler of
           Just handler -> handler
           Nothing -> return ()	      

