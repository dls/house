-- | Port numbers (used by UDP and TCP) and some standard port numbers
module Net.PortNumber where
import Kernel.Bits(Word16)
import Net.PacketParsing

newtype Port = Port Word16 deriving (Eq,Ord,Bounded,Show) -- Enum

-- GHC allows deriving the Enum instance...
instance Enum Port where
  toEnum = Port . toEnum
  fromEnum (Port p) = fromEnum p

instance Parse Port where parse = Port # parse
instance Unparse Port where unparse (Port p) = unparse p


--- Standard ports:

echo = Port 7
telnet = Port 23
bootps = Port 67
bootpc = Port 68
tftp = Port 69
http = Port 80
