module Net.ARP where

-- Address Resolution Protocol, limited to IPv4 over Ethernet
-- See http://www.networksorcery.com/enp/protocol/arp.htm

import Kernel.Bits(Word8,Word16)
import Net.Ethernet as Eth(Addr)
import Net.IPv4 as IPv4(Addr)
import Net.PacketParsing

data Packet = Packet
            { --hwType       :: !Word16,   -- Always 0x0001 (Ethernet)
              --protocolType :: !Eth.PacketType,   -- Always 0x0800 (IPv4)
	      --hLen, pLen   :: !Word8,    -- Always 0x06, 0x04
	      opcode    :: Operation, -- 16 bits, see below
	      senderHA  :: Eth.Addr,
	      senderIP  :: IPv4.Addr,
	      targetHA  :: Eth.Addr,
	      targetIP  :: IPv4.Addr }
            deriving (Show)

data Operation = Request   -- 0x0001, RFC 826
               | Reply     -- 0x0002, RFC 826
               deriving (Eq,Show)

instance Parse Packet where
    parse =
       return Packet #! check16 0x0001 #! check16 0x0800
	             #! check8 0x06 #! check8 0x04
	             <# parse <# parse <# parse <# parse <# parse
		     #! therest

instance Unparse Packet where
   unparse (Packet op sHA sIP tHA tIP) =
      unparse (1::Word16,0x800::Word16,6::Word8,4::Word8) .
      unparse (op,sHA,sIP,tHA,tIP)

instance Parse Operation where
    parse = lift . conv =<< word16
      where conv 1 = Just Request
	    conv 2 = Just Reply
	    conv w = Nothing

instance Unparse Operation where
  unparse op = unparse (conv op)
    where conv Request = 1::Word16
          conv Reply = 2
