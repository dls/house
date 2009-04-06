module Kernel.Driver.Intel8255x.Util where
import Foreign.Storable(Storable(..))
import Net.Ethernet as Eth(Addr(..))
import Net.Packet(OutPacket(..),Chunk,Word8)
import Net.Utils(arraySize)
import H.AdHocMem(H,Ptr,pokeArray,plusPtr)

instance Storable Eth.Addr where
  sizeOf _ = 6
  alignment _ = 1
  peek p = do a <- peekByteOff p 0
	      b <- peekByteOff p 1
	      c <- peekByteOff p 2
	      d <- peekByteOff p 3
	      e <- peekByteOff p 4
	      f <- peekByteOff p 5
	      return (Eth.Addr a b c d e f)
	      
  poke p (Eth.Addr a b c d e f) =
    do pokeByteOff p 0 a
       pokeByteOff p 1 b
       pokeByteOff p 2 c
       pokeByteOff p 3 d
       pokeByteOff p 4 e
       pokeByteOff p 5 f

--instance Storable OutPacket -- No, because OutPacket values have varying size!
pokeOutPacket p = pokeChunks p . chunks
  where
    pokeChunks :: Ptr Word8 -> [Chunk] -> H ()
    pokeChunks _ [] = return ()
    pokeChunks p (c:cs) = do pokeArray p c
			     pokeChunks (p `plusPtr` arraySize c) cs
