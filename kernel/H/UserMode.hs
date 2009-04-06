{-# OPTIONS -fvia-C #-}
-- | User-space Execution (section 3.3 in the paper)
module H.UserMode(Context(..),execContext,
		  Interrupt(..),ErrorCode,PageFaultErrorCode(..)
		 ) where
import Data.Word(Word8,Word32)
import Data.Bits
import H.Monad(H,liftIO)
import H.VirtualMemory
import H.Interrupts(IRQ)
import H.Utils
import H.Unsafe(unsafePerformH)
import H.Concurrency(QSem,newQSem,withQSem)
import H.AdHocMem(Ptr,peek,peekElemOff,pokeElemOff)
import H.Mutable

-------------------------------- INTERFACE -------------------------------------


{-| @Context@ describes the (non-memory) state of the processor, including
the values of the program-accessible registers and control flags.
This is the definition for the IA32 architecture. -}
data Context  -- the user-visible r/w part of the IA32's context.
    = Context { edi, esi, ebp, esp, ebx, edx, ecx, eax, eip, eflags :: Word32 } 
    deriving Show

{- | @execContext@ is the operator for executing code in a user address space.

Invoking @execContext@ installs the specified page map and context
into the appropriate hardware registers and puts the processor into user mode. 
User code then executes (starting at the @eip@ recorded
in the 'Context') and can access the physical addresses visible to it
through its page map.
When user-mode execution is interrupted the processor records the
new current context and returns to kernel mode; @execContext@ then returns
with that 'Context' and the nature of the 'Interrupt'.
-}
execContext :: PageMap -> Context -> H(Interrupt,Context)

data Interrupt -- both "Interrupts" and "Exceptions" in IA32 terminology
    = DivideError
    | NMIInterrupt
    | Breakpoint
    | Overflow
    | BoundRangeExceeded
    | InvalidOpcode
    | DeviceNotAvailable
    | DoubleFault ErrorCode
    | InvalidTSS ErrorCode
    | SegmentNotPresent ErrorCode
    | StackSegmentFault ErrorCode
    | GeneralProtection ErrorCode
    | PageFault PageFaultErrorCode VAddr 
    | FPUError
    | AlignmentCheck ErrorCode
    | MachineCheck
    | SIMDException
    | ExternalInterrupt IRQ
    | ProgrammedException Word8 -- otherwise unclassifiable interrupts
    deriving Show

type ErrorCode = Word32      -- uninterpreted for now

data PageFaultErrorCode = PageFaultErrorCode {
       userMode :: Bool,  -- true if fault originated in user mode
       write :: Bool,     -- true if fault caused by a write
       protection :: Bool -- true if fault casued by page-level protection violation; 
                          -- false if caused by page not present
     }  -- Show

------------PRIVATE IMPLEMENTATION FOLLOWS ----------------------------------------------------
  
instance Show PageFaultErrorCode   where show = showPFECode

showPFECode :: PageFaultErrorCode -> String

showPFECode code = 
     (if userMode code then "U" else "S") ++ 
     (if write code then "W" else "R") ++ 
     (if protection code then "P" else "")

type FaultContext = Ptr Word32

-- Projection from a struct requires -fvia-C:
foreign import ccall unsafe "userspace.h & task_kernel_stack.fault_context" theFC :: FaultContext

foreign import ccall unsafe "userspace.h execute" execute :: Ptr Word32 -> IO Word8 -- returns interrupt vector #; reads and side-effects theFaultContext
	

-- offsets within faultContext (see fault.h)
data FCO 
  = EDI | ESI | EBP | BogusESP | EBX | EDX | ECX | EAX | ES | DS | Handler
  | Code | EIP | CS | Eflags | ESP | SS
  deriving Enum

pokeFC field value = pokeElemOff theFC (fromEnum field) value
peekFC field = peekElemOff theFC (fromEnum field)

-- To prevent multiple Haskell threads using the faultContext at the same time,
-- we protect it (and currentPageMap) with a semaphore.

fcSem :: QSem
fcSem = unsafePerformH $ newQSem 1

currentPageMap :: Ref PageMap  -- this is just here to keep the PageMap alive
currentPageMap = unsafePerformH $ newRef undefined

-- masks and bits within eflags
mUserEflags = 0xCD5
bInterrupt = 9

execContext pm context =
    withQSem fcSem $
    do pokeFC EDI (edi context)
       pokeFC ESI (esi context)
       pokeFC EBP (ebp context)
       pokeFC EBX (ebx context)
       pokeFC EDX (edx context)
       pokeFC ECX (ecx context)
       pokeFC EAX (eax context)
       pokeFC EIP (eip context)
       pokeFC ESP (esp context)
       pokeFC Eflags (setBit' bInterrupt ((eflags context) .&. mUserEflags))
       writeRef currentPageMap pm
       vector <- liftIO (execute (fromPageMap pm))
       code <- peekFC Code 
       faultAddr <- peek lastFaultAddr
       edi <- peekFC EDI 
       esi <- peekFC ESI
       ebp <- peekFC EBP
       ebx <- peekFC EBX 
       edx <- peekFC EDX 
       ecx <- peekFC ECX
       eax <- peekFC EAX
       eip <- peekFC EIP
       esp <- peekFC ESP 
       eflags <- peekFC Eflags 
       let interrupt = vectorToInterrupt vector code faultAddr
       let context = Context{edi=edi,esi=esi,ebp=ebp,ebx=ebx,edx=edx,
			     ecx=ecx,eax=eax,eip=eip,esp=esp,eflags=eflags}
       return (interrupt,context)      
     
foreign import ccall unsafe "userspace.h & last_fault_addr" lastFaultAddr :: Ptr Word32

--  bits within pfe code
bUserMode = 2
bWrite = 1
bProtection = 0

vectorToInterrupt :: Word8 -> Word32 -> Word32 -> Interrupt
vectorToInterrupt vector code faultAddr =
  case vector of
    0x00 -> DivideError
    0x02 -> NMIInterrupt
    0x03 -> Breakpoint
    0x04 -> Overflow
    0x05 -> BoundRangeExceeded
    0x06 -> InvalidOpcode
    0x07 -> DeviceNotAvailable
    0x08 -> DoubleFault code
    0x0A -> InvalidTSS code
    0x0B -> SegmentNotPresent code
    0x0C -> StackSegmentFault code
    0x0D -> GeneralProtection code
    0x0E -> let pfeCode = PageFaultErrorCode {userMode = testBit' bUserMode code,
                                              write = testBit' bWrite code,
                                              protection = testBit' bProtection code}
	    in PageFault pfeCode faultAddr
    0x10 -> FPUError
    0x11 -> AlignmentCheck code
    0x12 -> MachineCheck
    0x13 -> SIMDException
    _ | vector >= 0x20 && vector <= 0x2f -> ExternalInterrupt (toEnum (fromIntegral code))
    _ -> ProgrammedException vector
