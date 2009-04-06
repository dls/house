-- | Utilities for memory regions.
module Kernel.MemRegion 
  ( module Kernel.MemRegion, module H.MemRegion
  ) where

import H.Monad
import H.MemRegion

import Data.Array.IArray(range)
import Data.Array.IO(IOUArray,newArray_,writeArray,freeze)
import Data.Array.Unboxed(UArray)

import Data.Word



regionBytes :: MemRegion -> H (UArray Int Word8)
regionBytes r =
  do let b = (0,fromIntegral (regionSize r)-1)
     ma <- liftIO $ newArray_ b
     sequence_ [liftIO . writeArray (ma::IOUArray Int Word8) i
                  =<< peekByteOff r (fromIntegral i)| i<-range b]
     liftIO $ freeze ma




