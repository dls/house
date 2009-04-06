-- | Interrupts (section 3.4 in the paper)
module H.Interrupts(IRQ(..),enableIRQ,disableIRQ,eoiIRQ,
		    enableInterrupts,disableInterrupts,
	            installHandler,
		    {- registerIRQHandler, callIRQHandler -} ) where

import Foreign.StablePtr(StablePtr,newStablePtr)

import Data.Word
import Data.Bits
import Data.Ix
import H.Monad(H,liftIO)
-- import H.Mutable(HArray,newArray,readArray,writeArray)
-- import H.Unsafe(unsafePerformH)
import H.IOPorts


------------------------INTERFACE--------------------------

data IRQ = IRQ0 | IRQ1 | IRQ2 | IRQ3 | IRQ4 | IRQ5 | IRQ6 | IRQ7 |
           IRQ8 | IRQ9 | IRQ10 | IRQ11 | IRQ12 | IRQ13 | IRQ14 | IRQ15
           deriving (Show,Ord,Eq,Bounded,Enum,Ix)

-- | Enable an individual IRQ
enableIRQ :: IRQ -> H()
-- | Disable an individual IRQ
disableIRQ :: IRQ -> H()

{-| After an interrupt from a given IRQ, further interrupts from that
IRQ are disabled until the processor issues an \"end-of-interrupt\"
acknowledgement with the @eoiIRQ@ operation. -}
eoiIRQ :: IRQ -> H()

-- | Enable interrupts globally
enableInterrupts :: H()
-- | Disable interrupts globally
disableInterrupts :: H()

installHandler :: IRQ -> H() -> H()


---------PRIVATE IMPLEMENTATION FOLLOWS----------------------

mIRQp0, mIRQp1, sIRQp0, sIRQp1 :: Port
mIRQp0 = 0x20
mIRQp1 = 0x21
sIRQp0 = 0xA0
sIRQp1 = 0xA1

enableIRQ irq | w < 8 =
                 do oldmp1 <- inB mIRQp1
	     	    outB mIRQp1 (clearBit oldmp1 (fromIntegral w))
              | w < 16 =
                 do oldmp1 <- inB mIRQp1
                    outB mIRQp1 (clearBit oldmp1 2)
                    oldsp1 <- inB sIRQp1
                    outB sIRQp1 (clearBit oldsp1 (fromIntegral (w - 8)))
	      | otherwise = return ()
      where w = fromEnum irq


disableIRQ irq | w < 8 = 
	          do oldmp1 <- inB mIRQp1
                     outB mIRQp1 (setBit oldmp1 (fromIntegral w))
               | w < 16 =
	          do oldsp1 <- inB sIRQp1
                     outB sIRQp1 (setBit oldsp1 (fromIntegral (w - 8)))
   	       | otherwise = return ()
      where w = fromEnum irq

eoiIRQ irq | w < 8 = outB mIRQp0 0x20
           | w < 16 =
               do outB sIRQp0 0x20
                  outB mIRQp0 0x20
           | otherwise = return ()
      where w = fromEnum irq


foreign import ccall unsafe "io.h enableInterrupts" enableInterruptsIO :: IO ()
foreign import ccall unsafe "io.h disableInterrupts" disableInterruptsIO :: IO ()

enableInterrupts = liftIO $ enableInterruptsIO
disableInterrupts = liftIO $ disableInterruptsIO


foreign import ccall unsafe "start.h setIRQTable" setIRQTableIO :: Int -> StablePtr (H()) -> IO ()
installHandler irq h = liftIO $
	               do sptr <-  newStablePtr h
                          setIRQTableIO (fromEnum irq) sptr


