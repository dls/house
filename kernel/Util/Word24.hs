module Util.Word24(Word24,toWord24,fromWord24) where
import qualified Foreign.Storable as IO
import Data.Word(Word8,Word32)
import Data.Bits
import Foreign.Storable(Storable(..))

newtype Word24 = Word24 Word32 -- 8 uppermost bits to be ignored

toWord24 = Word24
fromWord24 (Word24 w) = w

instance Storable Word24 where
  sizeOf _ = 3
  alignment _ = 1
  peek p = do let c = fromIntegral :: Word8->Word32
	      b0 <- c `fmap` IO.peekByteOff p 0
	      b1 <- c `fmap` IO.peekByteOff p 1
	      b2 <- c `fmap` IO.peekByteOff p 2
	      return $ Word24 $ b0 .|. b1 `shiftL` 8 .|. b2 `shiftL` 16
  poke p (Word24 w) = 
          do let c = fromIntegral :: Word32->Word8
             IO.pokeByteOff p 0 (c w)
	     IO.pokeByteOff p 1 (c (w `shiftR` 8))
	     IO.pokeByteOff p 2 (c (w `shiftR` 16))
