module H.Utils where
import Data.Bits
import Data.Word(Word32)
import H.AdHocMem(Ptr,absolutePtr,nullPtr,plusPtr,minusPtr,alignPtr)

setBit' b = flip setBit b
clearBit' b = flip clearBit b
testBit' b = flip testBit b
condBit True = setBit'
condBit False = clearBit'

ptrToWord32 :: Ptr a -> Word32
ptrToWord32 p =
  fromIntegral (p `minusPtr` nullPtr)  -- hack assuming nullPtr = 0

ptrFromWord32 :: Word32 -> Ptr a
ptrFromWord32 = absolutePtr -- hack assuming nullPtr = 0

validPtr :: (Ptr a,Ptr a) -> Ptr a -> Bool
validPtr (minAddr,maxAddr) p =  p `minusPtr` minAddr >= 0 && maxAddr `minusPtr` p >= 0

alignedPtr :: Int -> Ptr a -> Bool
alignedPtr s p = p `alignPtr` s == p
