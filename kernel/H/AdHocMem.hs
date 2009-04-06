-- | Ad-hoc memory access, not necessarily safe!
module H.AdHocMem(module H.AdHocMem,H,
		  IO.Storable,Ptr,
		  nullPtr,plusPtr,minusPtr,alignPtr,advancePtr,castPtr,
		  Word32)
 where
import Data.Word(Word32)
import Control.Monad(zipWithM_)
import Foreign.Ptr(Ptr,nullPtr,plusPtr,minusPtr,alignPtr,castPtr)
import Foreign.Marshal(advancePtr)
import qualified Foreign.Marshal as IO(moveBytes,copyArray,withArray,allocaArray)
import qualified Foreign.Storable as IO
import qualified Foreign.Marshal.Alloc as IO(mallocBytes,free)
import H.Monad(H,liftIO,runH)
import Data.Array.IArray
import Data.Array.IO(IOUArray,newArray_,writeArray,freeze,unsafeFreeze)
--import Data.Ix
-- For SPECIALIZE pragma:
import Data.Array.Unboxed(UArray)
import Data.Word(Word8,Word16,Word32)

mallocBytes n = liftIO $ IO.mallocBytes n
free p = liftIO $ IO.free p

absolutePtr :: Word32 -> Ptr a
absolutePtr n = nullPtr `plusPtr` fromIntegral n

poke p x = liftIO $ IO.poke p x
peek p   = liftIO $ IO.peek p

pokeByteOff p o x = liftIO $ IO.pokeByteOff p o x
peekByteOff p o   = liftIO $ IO.peekByteOff p o

pokeElemOff p o x = liftIO $ IO.pokeElemOff p o x
peekElemOff p o   = liftIO $ IO.peekElemOff p o

moveBytes dst src n = liftIO $ IO.moveBytes dst src n
copyArray dst src n = liftIO $ IO.copyArray dst src n
withArray xs h      = liftIO $ IO.withArray xs (runH . h)
allocaArray i h     = liftIO $ IO.allocaArray i (runH . h)

pokeArray p a =
--    zipWithM_ (pokeElemOff p) [0..] (elems a) -- slow
    mapM_ (uncurry (pokeElemOff p . index b)) (assocs a) --avoids bounds checks?
--     sequence_ [pokeElemOff p (index b i) (a!i)|i<-range b]
  where b = bounds a

peekArray p n =
  do let b = (0,n-1)
     ma <- liftIO $ newArray_ b
     let t = id :: IOUArray Int a -> IOUArray Int a
     sequence_ [liftIO . writeArray (t ma) i
                  =<< peekByteOff p (fromIntegral i)| i<-range b]
     --liftIO $ freeze ma
     liftIO $ unsafeFreeze ma -- avoid extra copying

type PokeArray d = Ptr d  -> UArray Int d  -> H ()
{-# SPECIALIZE pokeArray :: PokeArray Word8 #-}
{-# SPECIALIZE pokeArray :: PokeArray Word16 #-}
{-# SPECIALIZE pokeArray :: PokeArray Word32 #-}

type PeekArray d = Ptr d -> Int -> H (UArray Int d)
{-# SPECIALIZE peekArray :: PeekArray Word8 #-}
{-# SPECIALIZE peekArray :: PeekArray Word16 #-}
{-# SPECIALIZE peekArray :: PeekArray Word32 #-}
