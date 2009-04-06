module Util.FontDecode(decodeFont) where
import Util.Font
import Data.Array
--import Foreign
import H.MemRegion(H,MemRegion,Offset,peekByteOff)
import Data.Word(Word8)

data P = P !MemRegion !Offset

decodeFont p = decode (P p 0) $ \ fs _ -> return (fontsFromList fs)

fontsFromList fonts = [(i::(String,Int),fontFromList f)|(i,f)<-fonts]::Fonts
fontFromList ((w,(a,d)),(b,es)) = Fixed w a d (listArray b es)

class Decode a where
  decode :: P->(a->P->H c) -> H c

instance Decode Int where decode = decodeEnum
instance Decode Char where decode = decodeEnum

instance (Decode a,Decode b) => Decode (a,b) where
  decode p0 k = decode p0 $ \ a p1 ->
	        decode p1 $ \ b p2 ->
		k (a,b) p2

instance Decode a => Decode [a] where
  decode p0 k = decode p0 $ \ (hi,lo) p1 ->
	        let n = 256*hi+lo::Int
		in decodeList n p1 k

decodeEnum p k = do b <- peek p
		    k (convEnum b) (step p)
  where
    convEnum x = toEnum (fromEnum x)

decodeList 0 p0 k = k [] p0
decodeList n p0 k = decode p0 $ \ x p1 ->
		    decodeList (n-1) p1 $ \ xs p2 ->
		    k (x:xs) p2


step (P m o) = P m (succ o)
peek (P m o) = peekByteOff m o :: H Word8
