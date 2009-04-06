-- From InternetLib by Thomas Hallgren,
-- http://www.cs.chalmers.se/~hallgren/InternetLib/
module BitStream(bitstream,btake,BitStream) where
import Data.Array.Unboxed
import Data.Bits
import Data.Word(Word8,Word32)

--import Trace(trace)
trace s x = x

type Code = Int
type Cache = Word32
type ByteArray = UArray Int Word8
data BitStream = B {cache:: !Cache,
		    cnt,byte,end:: !Int,buf:: !ByteArray, bufs::[ByteArray]}

bitstream :: [ByteArray] -> BitStream
bitstream (b:bs) = B 0 0 lo hi b bs
  where (lo,hi) = bounds b

btake :: Code -> Int -> BitStream -> (Code,BitStream)
btake eos n bs = btake' eos n (fillcache n bs)

fillcache n bs@B{cnt=cnt}
  | n<=cnt = bs
  | cnt==0 = get4 bs
  | cnt<=8 = get3 cnt bs
  | cnt<=16 = get2 cnt bs
  | cnt<=24 = get1 cnt bs
  | otherwise = bs

get1 pos s@(B cache cnt byte end b bs) =
    if byte<=end
    then B (extend cache (b!byte) pos) (pos+8) (byte+1) end b bs
    else case bs of
           [] -> s
           b2:bs2 -> get1 pos (B cache cnt lo hi b2 bs2)
	     where (lo,hi) = bounds b2

get2 pos bs = get1 (pos+ 8) (get1 pos bs)
get3 pos bs = get1 (pos+16) (get2 pos bs)
get4     bs = get1 24       (get3 0   bs)

btake' :: Code -> Int -> BitStream -> (Code,BitStream)
btake' eos n bs@(B cache cnt byte end buf bufs) =
  if n<=cnt
  then (fromIntegral (cache .&. ((1 `shiftL` n)-1)),
	B (cache `shiftR` n) (cnt-n) byte end buf bufs)
  else (eos,bs)

extend :: Cache -> Word8 -> Int -> Cache
extend cache b pos = cache .|. fromIntegral b `shiftL` pos

