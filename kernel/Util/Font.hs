module Util.Font where
import Data.Array

type Fonts = [((String,Int),Font)] -- ((name,height),font)

data Font = Fixed {width,ascent,descent::Int,
		   shape::Array Char [(Int,Int)]}
--        | Prop (Array Char (Int,[(Int,Int)]))
           deriving (Read,Show)

textSize Fixed{width=w,ascent=a,descent=d} s = (w*length s,(a,d))

double (Fixed w a d s) = Fixed (2*w) (2*a) (2*d) (fmap (concatMap dbl) s)
  where
    dbl (x,y) = [(x2,y2),(x2+1,y2),(x2,y2+1),(x2+1,y2+1)]
      where x2=2*x; y2=2*y
