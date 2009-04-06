-- A version of Hovel that can compile (though not run) on HModel.
-- Some modules from the Kernel/ directory have been copied in,
-- to avoid problems with imports from them...
-----------------------------------------------------------------------
module HovelM where

import HModel.H

-- import Kernel.AOut(Image)
import Data.Word(Word8, Word32)
import Data.Bits
import Kernel.Bits(catBits)

-- import Kernel.UserProc(UProc,buildUProc,execUProc)
import Numeric(showHex)
import Data.Bits((.&.))
import Data.Char(chr)
import Data.Word(Word32)

-----------------------------------------------------------------------
       
-- For verification purpoes, we only need to consider the behavior of exec.
-- Note that this does not depend on the actual images provided, so
-- there's no need to prove anything about the aout format processing.

exec :: Image -> [Image] -> H ()
exec tracedImage otherImages =
   do tracedProc <- buildUProc trace tracedImage
      otherProcs <- mapM (buildUProc (\s -> return ())) otherImages
      schedule $ tracedProc:otherProcs    

-- A very simple round-robin scheduler.
schedule :: [UProc] -> H ()
schedule [] = schedule []
schedule (uproc:uprocs) = 
  do r <- execUProc uproc
     case r of
       Left uproc' -> schedule $ uprocs ++ [uproc']
       Right msg -> schedule uprocs

-----------------------------------------------------------------

-- from Kernel.AOut:

-- full static info about a.out file
data AOut = AOut 
	     {entry      :: VAddr, 
	      codeRegion :: VRegion,
	      dataRegion :: VRegion,
	      bssRegion  :: VRegion,
	      codeBytes  :: Image,  -- return n'th code byte
	      dataBytes  :: Image   -- return n'th data byte
	     } 

instance Show AOut where
  showsPrec _ aout s = s ++ 
               "Aout{entry=" ++ show (entry aout) ++ 
	        ",codeRegion=" ++ show (codeRegion aout) ++
	        ",dataRegion=" ++ show (dataRegion aout) ++ 
	        ",bssRegion=" ++ show (bssRegion aout) ++
               "}"

type Image = Offset -> H Word8  -- abstract accessor into image 
type Offset = Word32      -- zero-based offset into the executable (a.out) image
type VRegion = (VAddr, VAddr)   
                  -- by convention, interval is closed on left but open on right
      
{- a.out header offsets -}
headerLen     = 0x20
headerInfo    = 0x00
headerTextLen = 0x04 -- includes header AND text
headerDataLen = 0x08
headerBssLen  = 0x0C
headerEntry   = 0x14

userRegion :: VRegion
userRegion = (0x10000000,0x40000000)

imageWord32 image offset =
   (catBits . reverse) `fmap` mapM image [offset..offset+3]
   -- little endian

buildAOut :: Image -> H AOut
buildAOut aoutImage =
     do entry   <- imageWord32 aoutImage headerEntry 
	lenText <- imageWord32 aoutImage headerTextLen 
        lenData <- imageWord32 aoutImage headerDataLen
	lenBss  <- imageWord32 aoutImage headerBssLen
	let endCode = fst userRegion + lenText - fromIntegral headerLen
                      -- a.out QMAGIC weirdness
	let endData = endCode + lenData
        let endBss = endData + lenBss
        return AOut
	       {entry      = entry,
	        codeRegion = (fst userRegion,endCode),
                dataRegion = (endCode,endData),
                bssRegion  = (endData,endBss),
		codeBytes  = aoutImage . (fromIntegral headerLen+),
                dataBytes  = aoutImage . (lenText+)}

inVRegion :: VAddr -> VRegion -> Bool
inVRegion x (s,e) = x >= s && x < e

---------------------------------------------------------------------

-- from Kernel/UserProc.hs:

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
		context     = zeroContext{eip = entry aout,esp = endStack}}

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



-- just ignore non-timer interrupts for now
callIRQHandler irq = return ()


{-
The following won't work in the model, of course.

---------------------------------------------------------------------
-- The main program is just to test the framework; it 
-- loads and runs aout images from grub modules,
-- and it displays output from all processes as it is generated.

main :: IO ()
main = runH
     $ do initVideo
          putStr "Welcome to Hovel!\n\n"
          helloImage <- loadImage "hello"  
	  divImage <- loadImage "div"
	  loopImage <- loadImage "loop"
  	  exec helloImage [loopImage,helloImage,divImage]
          putStr "Terminating!\n"
          let halt = putStr "" >> halt in halt

-- getting images from grub modules
loadImage :: String -> H Image
loadImage file =
   do findex <- pickModule putStrLn file
      case findex of
        Nothing -> do putStrLn ("No module: " ++ file)
 	              error "Fatal Error"
        Just i  -> do reg <- moduleRegion i
                      return $ peekByteOff reg


-}


