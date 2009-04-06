module Kernel.UserProc(UProc,buildUProc,execUProc) where

{-P:
import Prelude hiding (putStr)
-}
-- import Kernel.Debug(putStr,putStrLn)
import Numeric(showHex)
import Data.Bits((.&.))
import Data.Char(chr)
import H.Monad(H)
import H.UserMode(Context(..),Interrupt(..),execContext)
import H.Interrupts(IRQ(..),eoiIRQ)
import Kernel.Interrupts(callIRQHandler)
import Data.Word(Word32)
import H.Monad(H)
import H.VirtualMemory(VAddr,PageMap,PageInfo(..),allocPageMap,freePageMap,setPage)
import H.PhysicalMemory(PhysPage,pageSize,allocPhysPage,freePhysPage,setPAddr)
import Kernel.AOut


data UProc = UProc
	     {aout        :: AOut,
	      myPutStr    :: String -> H (),
	      brk	  :: VAddr,
	      maxBrk      :: VAddr,
              stackRegion :: VRegion,
	      pmap        :: PageMap,
	      ownedPages  :: [PhysPage],
	      context	  :: Context
	     }  

instance Show UProc where
 showsPrec _ uproc s = s ++ "UProc{aout=" ++ show (aout uproc) 
                         ++ ",brk=" ++ show (brk uproc)
		         ++ ",maxBrk=" ++ show (maxBrk uproc)
                         ++ ",stackRegion=" ++ show (stackRegion uproc)
		         ++ ",context=" ++ show (context uproc)
                         ++ "}"	   

maxStackPages :: Int
maxStackPages = 16

maxHeapPages :: Int
maxHeapPages = 128   -- 512KB 

buildUProc :: (String -> H()) -> Image -> H UProc
buildUProc putStr aoutImage =
     do aout  <- buildAOut aoutImage
        let brk = pageCeiling (snd $ bssRegion aout)
        let endStack = snd userRegion
        let startStack = endStack - fromIntegral (maxStackPages*pageSize)
	Just pmap  <- allocPageMap  -- die here if out of space
        return UProc
	       {aout        = aout,
                myPutStr    = putStr,
	        brk         = brk,
		maxBrk      = brk + fromIntegral (maxHeapPages*pageSize),
	        stackRegion = (startStack,endStack),
	        pmap        = pmap,
	        ownedPages  = [],
		context     = zeroContext{eip = entry aout,esp = endStack-4}}

zeroContext :: Context
zeroContext  = Context {edi=0,esi=0,ebp=0,esp=0,ebx=0,
                        edx=0,ecx=0,eax=0,eip=0,eflags=0}


execUProc :: UProc -> H (Either UProc String)
execUProc uproc 
  = do (interrupt,context') <- execContext (pmap uproc) (context uproc)
       let uproc' = uproc{context=context'}
           exitWith msg = 
              do freeUProc uproc'
                 return $ Right msg
       -- putStrLn("Back to kernel: " ++ (show interrupt))
       -- putStrLn(show uproc')
       case interrupt of
         ProgrammedException(0x80) -> -- system call
           do r <- doSyscall uproc'				   
              case r of
                Left uproc'' -> execUProc uproc''
                Right msg -> exitWith msg

         PageFault _ faultAddr -> 
           do r <- fixPage uproc' faultAddr
              case r of
                Left uproc'' -> execUProc uproc''
                Right msg -> exitWith msg

         ExternalInterrupt IRQ0 -> -- timeslice exhausted
           do -- eoiIRQ IRQ0 
              -- this has already been done in C code, 
	      -- because it doesn't seem to be executed
	      -- in a timely fashion if it is put here.
	      return $ Left uproc'

         ExternalInterrupt irq -> -- other IO interrupts
           do callIRQHandler irq
	      return $ Left uproc'

         _ -> -- any other code means we're done, like it or not
	   exitWith ("Unexpected Interrupt: " ++ (show interrupt))


doSyscall :: UProc -> H (Either UProc String)
doSyscall uproc@UProc{context=Context{eax=callnum,ebx=arg}} =
   do -- putStrLn("Sys call " ++ (show callnum) ++ " " ++ (show arg))
      case callnum of
       0 -> {- exit -}
            return $ 
               Right ("Success with " ++ show (fromIntegral (arg .&. 0xff)))
       1 -> {- putChar -}
            do myPutStr uproc [chr (fromIntegral (arg .&. 0xff))]
               return $ uproc `withResult` 0
       2 -> {- sbrk -}
            case extendBrk uproc arg of
              Nothing                   -> return $ uproc `withResult` (-1)
              Just (uproc',oldBrk)      -> return $ uproc' `withResult` oldBrk
       _ -> return $ uproc `withResult` (-1) 
   where withResult uproc@UProc{context=context} v = 
            Left (uproc{context=context{eax=v}})


freeUProc :: UProc -> H ()
freeUProc uproc  
  = do freePageMap (pmap uproc)
       mapM_ freePhysPage (ownedPages uproc)

extendBrk :: UProc -> Word32 -> Maybe (UProc,Word32)
extendBrk uproc incr
  = let oldBrk = brk uproc
        newBrk = (fromIntegral (((fromIntegral oldBrk)::Int) 
                                + ((fromIntegral incr)::Int)))::Word32
    in if newBrk `inVRegion` (oldBrk,maxBrk uproc) then
          Just (uproc{brk=newBrk},oldBrk)
       else
          Nothing


fixPage :: UProc -> VAddr -> H (Either UProc String)
fixPage uproc vaddr | vaddr `inVRegion` (codeRegion $ aout uproc)
  = setupPage uproc vaddr False
              (codeBytes (aout uproc) .
                     (+fromIntegral ((pageFloor vaddr) - 
                                     (fst $ codeRegion $ aout uproc))))
fixPage uproc vaddr | vaddr `inVRegion` (dataRegion $ aout uproc)
  = setupPage uproc vaddr True
               (dataBytes (aout uproc) .
                     (+fromIntegral ((pageFloor vaddr) - 
                                     (fst $ dataRegion $ aout uproc))))
fixPage uproc vaddr | vaddr `inVRegion` (fst $ bssRegion $ aout uproc,brk uproc)
  = setupPage uproc vaddr True zeroSrc
fixPage uproc vaddr | vaddr `inVRegion` (stackRegion uproc)
  = setupPage uproc vaddr True zeroSrc
fixPage _     vaddr | otherwise 
  = return $ Right ("Invalid page fault at " ++ (showHex vaddr ""))

setupPage :: UProc -> VAddr -> Bool -> Image -> H (Either UProc String)
setupPage uproc vaddr writable src
 = do pagem <- allocPhysPage
      case pagem of
        Just page -> 
          do let pi = PageInfo {physPage=page,writable=writable, 
                                dirty=False, accessed=False}
             setPage (pmap uproc) vaddr (Just pi)
	     copyPage page src 
	     return $ Left uproc{ownedPages=page:(ownedPages uproc)}
        Nothing -> return $ 
                     Right ("Out of pages when servicing fault at " ++ 
                            (showHex vaddr ""))

copyPage :: PhysPage -> Image -> H ()
copyPage physPage src =
   sequence_ [setPAddr (physPage,off) =<< src (fromIntegral off) 
              | off <- [minBound..]] 

zeroSrc :: Image 
zeroSrc _ = return 0

-- round down to nearest whole number multiple of page size
pageFloor :: VAddr -> VAddr
pageFloor vaddr = vaddr `div` (fromIntegral pageSize) * (fromIntegral pageSize)

-- round up to nearest whole number multiple of page size
pageCeiling :: VAddr -> VAddr 
pageCeiling vaddr = ((vaddr - 1) `div` (fromIntegral pageSize) + 1) 
                    * (fromIntegral pageSize)

