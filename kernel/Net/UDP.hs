module Net.UDP(module Net.UDP,Port(..)) where

-- User Datagram Protocol
-- See http://www.networksorcery.com/enp/protocol/udp.htm

import Net.PacketParsing
import Net.Packet(outLen)
import Kernel.Bits(Word16)
import Net.PortNumber(Port(..))
import Net.Utils(Container(..))

data Packet content = Packet
                    { sourcePort, destPort :: !Port,
		      len :: !Word16, -- length of header (i.e. 8) + data
                      checksum :: !Word16, -- don't store?
                      content :: !content }
		    deriving (Show)

template sp dp c = Packet sp dp 0 0 c

instance Functor   Packet where fmap f p = p{content=f (content p)}
instance Container Packet where contents = content

instance Parse a => Parse (Packet a) where
  parse = do (sp,dp,len,sum) <- parse
	     trunc (fromIntegral len-8) -- discard padding
	     Packet sp dp len sum # parse
  -- Should also check the checksum.

instance Unparse a => Unparse (Packet a) where
  unparse (Packet sp dp l0 s c) = unparse (sp,dp,l,nosum,uc)
    where
      uc=doUnparse c
      l=fromIntegral(8+outLen uc) `asTypeOf` l0

      -- If the checksum is cleared to zero, then checksumming is disabled
      nosum=0::Word16
