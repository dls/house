module Net.TFTP where

-- TFTP - Trivial File Transfer Protocol, RFC 1350, STD 33
-- See http://www.networksorcery.com/enp/protocol/tftp.htm
--     http://www.networksorcery.com/enp/rfc/rfc1350.txt

import Kernel.Bits
import Net.Packet
import Net.PacketParsing

data Packet
  = RRQ Filename Mode
  | WRQ Filename Mode
  | Data BlockNr Data
  | Ack BlockNr
  | Error ErrorCode ErrMsg
  deriving (Show)

type Filename = String
type Mode = String
type BlockNr = Word16
type Data = UArray Int Word8 -- could be changed
newtype ErrorCode = E Word16 deriving (Eq,Show)
type ErrMsg = String

--------------------------------------------------------------------------------

instance Parse Packet where
  parse =
    do opcode <- word16
       case opcode of
         1 -> RRQ # string <# string
	 2 -> WRQ # string <# string
	 3 -> Data # parse <# parse
	 4 -> Ack # parse
	 5 -> Error # parse <# string
	 _ -> fail "bad TFTP opcode"

instance Parse ErrorCode where parse = E # parse

string = do b <- word8
	    if b==0
	      then return []
	      else (char b:) # string
  where char :: Word8 -> Char
	char = toEnum . fromEnum

--------------------------------------------------------------------------------

instance Unparse Packet where
  unparse p =
    case p of
      RRQ f m -> w 1 . unstring f . unstring m
      WRQ f m -> w 2 . unstring f . unstring m
      Data n d -> w 3 . unparse (n,d)
      Ack n -> w 4 . unparse n
      Error n s -> w 5 . unparse n . unstring s
    where w n = unparse (n::Word16)

instance Unparse ErrorCode where unparse (E w) = unparse w

unstring s = unparse (s,'\0')
