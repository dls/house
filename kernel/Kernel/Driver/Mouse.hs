module Kernel.Driver.Mouse where
import Prelude hiding (Either(..))
import Data.Bits
import Data.Word
import Data.Int(Int8)
import H.Concurrency

data Button = Left | Right | Middle
            deriving Show
data Event  = Up !Button !Int !Int | Down !Button !Int !Int | MoveTo !Int !Int
            deriving Show

launchMouseDecoder (width,height) from =
  do to <- newChan
     forkH  $ writeList2Chan to .
              decode False False False x0 y0 {-. tail-}=<< getChanContents from
                    -- hack: skip enabling ack.  
     return to
  where
    x0 = width `div` 2
    y0 = height `div` 2

    limit m = min m . max 0

    decode :: Bool -> Bool -> Bool -> Int -> Int -> [Word8] -> [Event]
    decode _ _ _ _ _ []           = [] 
    decode l r m x y (s:dx:dy:ws) =
	mv ++ btn Left l l' ++ btn Right r r' ++ btn Middle m m' 
	++ decode l' r' m' x' y' ws
      where
	-- use sign bit
	l'  = s `testBit` 0
	r'  = s `testBit` 1
	m'  = s `testBit` 2
	dx', dy'   :: Int8
	dx'         = fromIntegral dx 
	dy'         = fromIntegral dy
	x'          = limit width (x + fromIntegral dx')
	y'          = limit height (y - fromIntegral dy')
	btn b a a'  = [if a' then Down b x' y' else Up b x' y' | a /= a']
	mv          = [MoveTo x' y' | dx' /= 0 || dy' /= 0]