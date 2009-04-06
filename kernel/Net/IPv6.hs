module Net.IPv6 where

-- IPv6, Internet Protocol version 6
-- See http://www.networksorcery.com/enp/protocol/ipv6.htm

import Kernel.Bits
import Net.IPv4(Protocol)
import Net.PacketParsing

data Addr = Addr !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16
          deriving (Eq)

instance Parse Addr where
  parse = Addr # parse <# parse <# parse <# parse
              <# parse <# parse <# parse <# parse

instance Unparse Addr where
  unparse (Addr w1 w2 w3 w4 w5 w6 w7 w8) = unparse ((w1,w2,w3,w4),(w5,w6,w7,w8))

newtype Word4 = Word4 Word8 -- extra bits should be zero
newtype Word20 = Word20 Word32 -- extra bits should be zero

instance Parse Word4 where parse = Word4 # bits 4
instance Parse Word20 where parse = Word20 # bits 20

data Packet content = Packet
                    { version        :: !Word4
		    , traffic_class  :: !Word8
		    , flow_label     :: !Word20
		    , payload_length :: !Word16
		    , next_header    :: !Protocol -- 8 bits
		        -- Need to add extension header types to Protocol
		    , hop_limit      :: !Word8
		    , source         :: !Addr -- 128 bits
		    , dest           :: !Addr -- 128 bits
		      -- + extension headers!
		    , content        :: !content
                       -- The next_header field of the last extention header
                       -- determines the type of content!
		    }

instance Functor Packet where fmap f p = p{content=f (content p)}

instance Parse content => Parse (Packet content) where
  parse = Packet # parse <# parse <# parse <# parse <# parse <# parse <# parse <# parse <# parse
{-
    where
       packet w1 = Packet v t f
          where v = fromIntegral (w1 `shiftR` 28)
		t = fromIntegral ((w1 `shiftR` 20) .&. 0xff)
		f = w1 .&. 0xfffff
-}

instance Unparse content => Unparse (Packet content) where
  unparse (Packet (Word4 v) t (Word20 f) p n h s d c) =
      unparse ((w1,p,n,h),(s,d),c)
    where w1 = ext v `shiftL` 28 .|. ext t `shiftL` 20 .|. f
          ext = fromIntegral
