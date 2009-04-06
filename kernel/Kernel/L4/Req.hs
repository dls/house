

module Kernel.L4.Req where

import Kernel.L4.ThreadId
import Kernel.L4.SubTyping
import Data.Word
import Data.Bits
import Kernel.UserSpace

makeReq :: Word8 -> Context -> Maybe Req
makeReq i c
  = case i of  
      0x80 -> Nothing -- will be KernelInterface
      0x81 -> Nothing -- will be ExchangeRegisters
      0x82 -> Nothing -- will be SystemClock
      0x83 -> return (inj (ThreadSwitch (toThreadId (eax c))))
      0x84 -> return (inj (SetPriority (fromIntegral (ecx c)))) -- not real
      0x85 -> let dst  = toThreadId (eax c)
                  time = toTimeout (ecx c) 
              in if dst == Nilthread
                   then return (inj (Recv (toThreadId (edx c)) time))
                   else return (inj (Send dst time))
      0x86 -> Nothing -- will be LIPC
      0x87 -> let ctrl = eax c
              in return (inj (Unmap (testBit ctrl 6) (fromIntegral (ctrl .&. 63))))
      0x90 -> return (inj (Fork (toThreadId (eax c)) (fromIntegral (edx c))))
{- Commented out as a quick hack to use Fork instead of ThreadControl
      0x90 -> return (inj (ThreadControl (toThreadId (eax c)) (toThreadId (esi c))
                             (toThreadId (ecx c)) (toThreadId (edx c))))
-}
      0x91 -> Nothing -- will be SpaceControl 
      0x92 -> Nothing -- will be ProcessorControl
      0x93 -> Nothing -- will be MemoryControl
      _    -> Nothing

-- need to incorporate anythread/anylocal into thread id type

toThreadId :: Word32 -> ThreadId
toThreadId 0 = Nilthread
toThreadId x = (ThreadId . fromIntegral) x

toTimeout :: Word32 -> Timeout
toTimeout 0 = Zero
toTimeout _ = Infinite

 {-
data Interrupt = I_ProgrammedException Word8
data Context = Context {
     edi :: Word32,
     esi :: Word32,
     ebp :: Word32,
     esp :: Word32,
     ebx :: Word32,
     edx :: Word32,
     ecx :: Word32,
     eax :: Word32,
     eip :: Word32,
     eflags :: Word32
}
 -}

type Req = Either ThreadSwitch
               (Either Fork -- ThreadControl
                 (Either Unmap
                   (Either IPC
                     (Either Prio ()))))

data ThreadSwitch  = ThreadSwitch ThreadId 
 deriving Show

-- still missing UTCBlocation ptr (how is a void* ptr represented in Haskell?)

data Fork          = Fork ThreadId Word32
 deriving Show

data ThreadControl = ThreadControl ThreadId ThreadId ThreadId ThreadId

data Unmap   = Unmap Bool Int
 deriving Show

data IPC     = Send ThreadId Timeout | Recv ThreadId Timeout
 deriving Show

newtype  Dst = Dest ThreadId
data     Src = From ThreadId | AnyLocal | Any

data Timeout = Zero | Infinite
  deriving (Eq, Show)

data Prio    = SetPriority Int 
             | GetPriority 
 deriving Show

data Debug   = Debug String 

