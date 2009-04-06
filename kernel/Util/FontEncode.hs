module Util.FontEncode where
import Util.Font
import Data.Array
import Foreign

encodeFonts = flip encode [] . fontsAsList

fontsAsList fonts = [(i,fontAsList f)|(i,f)<-fonts::Fonts]
fontAsList (Fixed w a d sh) = ((w,(a,d)),(bounds sh,elems sh))

class Encode a where
  encode :: a -> ShowS

instance Encode Int where
  encode x | inRange (0,255) x = (toEnum x:)

instance (Encode a,Encode b) => Encode (a,b) where
  encode (x,y) = encode x . encode y

instance Encode a => Encode [a] where
  encode xs = encode (long (length xs)) . foldr ((.).encode) id xs

instance Encode Char where
  --encode c = (c:) -- should work
  encode = encode . fromEnum -- incldues range check for 8 bit chars

long n = (n `div` 256,n `mod` 256)
