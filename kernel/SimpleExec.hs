{- This is just to provide an example for the paper.  It may well not run. -}

module SimpleExec where

import Data.Word(Word8, Word32)
import Data.Bits
import Control.Monad
import Numeric(showHex)
import H.Monad(H)
import H.PhysicalMemory(PAddr, PhysPage, setPAddr, allocPhysPage, pageSize)
import H.VirtualMemory(VAddr, PageMap, PageInfo(..), allocPageMap, setPage)
import H.UserMode(Context(..), Interrupt(..), execContext)
import H.IOPorts
import H.Interrupts

data UProc = UProc
	     {entry      :: VAddr,
	      startCode  :: VAddr,
	      endCode    :: VAddr,
	      startData  :: VAddr,
	      endData    :: VAddr,
	      startBss   :: VAddr,
	      brk        :: VAddr,
	      startStack :: VAddr,
	      endStack   :: VAddr,
	      codeBytes  :: [Word8],
	      dataBytes  :: [Word8],
	      pmap       :: PageMap,
	      ticks	 :: Int
	     } 

fixPage :: UProc -> VAddr -> H Bool
fixPage uproc vaddr | vaddr >= (startCode uproc) && vaddr < (endCode uproc)
  = let codeOffset = fromIntegral((pageFloor vaddr) - (startCode uproc))
    in do setupPage uproc vaddr (drop codeOffset (codeBytes uproc)) False
          return True
fixPage uproc vaddr | vaddr >= (startData uproc) && vaddr < (endData uproc)
  = let dataOffset = fromIntegral((pageFloor vaddr) - (startData uproc)) 
    in do setupPage uproc vaddr (drop dataOffset (dataBytes uproc)) True
          return True
fixPage uproc vaddr | (vaddr >= (startBss uproc) && vaddr < (brk uproc))
                      || (vaddr >= (startStack uproc) && vaddr < (endStack uproc))
  = do setupPage uproc vaddr (repeat 0) True
       return True
fixPage uproc vaddr | otherwise = return False

setupPage :: UProc -> VAddr -> [Word8] -> Bool -> H ()
setupPage uproc vbase src writable
 = do Just page <- allocPhysPage
      let pi = PageInfo {physPage=page,writable=writable, dirty=False, accessed=False}
      setPage (pmap uproc) vbase (Just pi)
      zipWithM_ (curry setPAddr page) [minBound..maxBound] src

pageFloor :: VAddr -> VAddr
pageFloor vaddr = vaddr `div` (fromIntegral pageSize) * (fromIntegral pageSize)

extendBrk :: UProc -> Word32 -> Maybe UProc
extendBrk uproc incr
  = let newBrk = brk uproc + incr
    in if newBrk >= (brk uproc) && newBrk <= (startStack uproc)
       then Just uproc{brk=newBrk}
       else Nothing

runUProc :: UProc -> Int -> H String
runUProc uproc msecs
  = do initTimer 10
       Just pmap <- allocPageMap
       let context = zeroContext{eip=entry uproc, esp=endStack uproc}
       exec uproc{pmap=pmap,ticks=msecs `div` 10} context

initTimer :: Int -> H ()
initTimer msPerTick = 
  do outB timerPort (ticks .&. 0xff)    -- set timer interrupt frequency
     outB timerPort (ticks `shiftR` 8)
     enableIRQ IRQ0 
  where ticks = fromIntegral (1193190 * msPerTick `div` 1000)
        
timerPort :: Port
timerPort = 0x40

zeroContext :: Context
zeroContext = Context {edi=0,esi=0,ebp=0,esp=0,ebx=0,
                       edx=0,ecx=0,eax=0,eip=0,eflags=0}

exec :: UProc -> Context -> H String
exec uproc context
  = do (interrupt,context') <- execContext (pmap uproc) context
       case interrupt of
         ProgrammedException(0x80) -> 
           {- system call -}
           let callnum = eax context'
               arg = ebx context'
           in case callnum of
                0 -> {- exit -}
                     return ("Successful completion with result " ++ show arg)
	        1 -> {- brk -} 
                     case extendBrk uproc arg
                     of Nothing -> 
			   exec uproc context'{eax=(-1)}
                        Just uproc' -> 
                           exec uproc' context'{eax=brk uproc'}
                _ -> exec uproc context'{eax=(-1)}

         PageFault _ faultAddr -> 
           do fixOK <- fixPage uproc faultAddr 
	      if fixOK 
                then
                  exec uproc context'
                else
                  do return ("Fatal page fault at " ++ (showHex faultAddr ""))

         ExternalInterrupt IRQ0 -> 
           {- timer interrupt -}
           do eoiIRQ IRQ0
              let ticks' = ticks uproc - 1
              if ticks' > 0 
                then      
                  exec uproc{ticks=ticks'} context'
                else 
                  return ("Time exhausted")

         _ -> 
	   return ("Unexpected Fault or Interrupt: " ++ (show interrupt))

