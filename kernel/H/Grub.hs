-- | Access to information provided by Grub in accordance with the
-- multiboot specification, see
-- <http://www.gnu.org/software/grub/manual/multiboot/html_node/Boot-information-format.html>
module H.Grub(H,moduleName,moduleCount,moduleRegion,
	      MemInfo(..),memInfo,
              -- * Deprecated functions
	      moduleStart,moduleEnd) where
import Foreign.C(CString,peekCString)

import Control.Monad(ap)
import Data.Bits
import Data.Word(Word8)
import H.Monad(liftIO)
import H.MemRegion(MemRegion,createRegion)
import H.AdHocMem

------------------------ INTERFACE ---------------------------------------------

--data ModuleInfo = Mod { name::String, region::MemRegion }
--moduleInfo   :: Int -> H ModuleInfo

moduleCount  :: H Int
moduleName   :: Int -> H String
moduleRegion :: Int -> H MemRegion

data MemInfo = Mem { lower :: Int, -- ^ size in KB of memory starting at 0, max 640KB
		     upper :: Int -- ^ size in KB of memory starting at 1MB
		   } deriving Show
-- | @memInfo@ returns the size of the physical memory installed
memInfo :: H (Maybe MemInfo)

{-# DEPRECATED moduleStart, moduleEnd "Use moduleRegion instead" #-}
moduleStart  :: Int -> H (Ptr Word8)
moduleEnd    :: Int -> H (Ptr Word8)

------------------------ IMPLEMENTATION ----------------------------------------


moduleName n = liftIO . peekCString . snd =<< moduleInfo n

moduleCount = fst `fmap` modules

modules :: H (Int,Ptr Word32)
modules = do mib <- getMultibootInfo
             flags <- getMultibootFlags
	     if testBit flags 3
		then (,) `fmap` peekByteOff mib 20 `ap` peekByteOff mib 24
		else return (0,nullPtr)

moduleInfo :: Int -> H ((Ptr Word8,Ptr Word8),CString)
moduleInfo n = do (count,addr) <- modules
		  if n<count
		     then do let o = 16*n
			     start <- peekByteOff addr o
			     end   <- peekByteOff addr (o+4)
			     name  <- peekByteOff addr (o+8)
			     return ((start,end),name)
		     else return ((nullPtr,nullPtr),nullPtr) -- or fail...

moduleStart n = (fst . fst) `fmap` moduleInfo n
moduleEnd n = (snd . fst) `fmap` moduleInfo n

moduleRegion n =
  do ((base,end),_) <- moduleInfo n
     let size = end `minusPtr` base
     return $ createRegion base (toEnum size)


memInfo = do mib <- getMultibootInfo
             flags <- getMultibootFlags
	     if testBit flags 0
		then do lower <- peekByteOff mib 4
			upper <- peekByteOff mib 8
			return $ Just Mem{lower=lower,upper=upper}
		else return Nothing

-- | Like System.getArgs...
getMultibootInfo :: H (Ptr Word8)
getMultibootInfo = peek mibPtr

getMultibootFlags :: H Word32
getMultibootFlags = do mib <- getMultibootInfo
		       if mib==nullPtr
			  then return 0
			  else peekByteOff mib 0

--foreign import ccall unsafe "modules.h" module_count :: IO Int
--foreign import ccall unsafe "modules.h" module_name :: Int -> IO CString
--foreign import ccall unsafe "modules.h" module_start :: Int -> IO (Ptr Word8)
--foreign import ccall unsafe "modules.h" module_end :: Int -> IO (Ptr Word8)

foreign import ccall unsafe "mib.h & multibootinfo" mibPtr :: Ptr (Ptr Word8)
