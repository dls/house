module Net.TCP where

-- Transmission Control Protocol
-- See http://www.networksorcery.com/enp/protocol/tcp.htm 
--     http://www.networksorcery.com/enp/rfc/rfc793.txt

import Kernel.Bits
import Net.PacketParsing
import Net.PortNumber(Port(..))
import Net.Utils(Container(..))

data Packet content = Packet
                    { sourcePort, destPort :: !Port,
		      seqNr, ackNr :: !Word32,
		      dataOffset :: !Word8, -- 4 bits
		      -- 3 reserved bits
		      ecn :: !ECN, -- 3 bits
		      controlBits :: !ControlBits, -- 6 bits
		      window :: !Word16,
		      checksum :: !Word16,
		      urgentPointer :: !Word16,
		      options :: ![Option], -- 0-44 bytes
		      -- padding
		      content :: !content }
		    deriving (Show)

template = Packet (Port 0) (Port 0) 0 0 5  minBound minBound 1400 0 0 [] ()

instance Functor   Packet where fmap f p = p{content=f (content p)}
instance Container Packet where contents = content

data ECN = ECN { n,c,e:: !Bool } -- Explicit Congestion Notification, RFC 3168
	 deriving (Show,Bounded)


data ControlBits = CB { urg,ack,psh,rst,syn,fin:: !Bool }
		   deriving (Eq,Bounded,Show)

type Option = Word8 -- not implemented yet

instance Parse content => Parse (Packet content) where
  parse = do (sp,dp,sqn,ackn) <- parse
	     hl <- bits 4
	     skip 3
	     let olen = (hl-5)*4
             Packet sp dp sqn ackn hl
		    # parse
		   <# parse
		   <# parse
		   <# parse
		   <# parse
		   <# bytes olen
		   <# parse
    where
      skip :: Int -> PacketParser Word32
      skip = bits

instance Parse ControlBits where
  parse = CB # parse <# parse <# parse <# parse <# parse <# parse

instance Parse ECN where
  parse = ECN # parse <# parse <# parse

--------------------------------------------------------------------------------

instance Unparse content => Unparse (Packet content) where
  unparse (Packet sp dp snr anr hl {-3-} ecn cb w s u o c) =
      unparse (sp,dp,snr,anr) .
      unparse ((ext hl << 12 .|. ecnB ecn << 6 .|. cbB cb)::Word16) .
      unparse (w,s,u,o,c)
    where
      (<<) = shiftL
      ext = fromIntegral
      ecnB (ECN n c e) = toBits [n,c,e]
      cbB (CB u a p r s f) = toBits [u,a,p,r,s,f]

toBits bs = fromIntegral (foldl bit 0 bs)
  where bit w b = w `shiftL` 1 .|. fromEnum (b::Bool)
