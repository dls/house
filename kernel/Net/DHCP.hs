module Net.DHCP where

-- Dynamic Host Configuration Protocol, RFC 2131
-- See http://www.networksorcery.com/enp/protocol/dhcp.htm
--     http://rfc.sunsite.dk/rfc/rfc2131.html
--     http://rfc.sunsite.dk/rfc/rfc1533.html (DHCP/BOOTP options)
 
import Kernel.Bits(Word8,Word16,Word32,testBit)
import qualified Net.IPv4 as IP
import qualified Net.Ethernet as Eth
import Net.PacketParsing
import Net.PortNumber

serverPort = bootps
clientPort = bootpc

data Packet = Packet
            { opcode::Operation, -- 1 byte
	      --hwType::Word8, -- always 0x01 (Ethernet)
	      --hLen::Word8, -- always 6
              --hOps::Word8, -- 0 except when booting via relay agents
	      xid::Word32, -- Transaction ID, randomly chosen by the client
	      secs::Word16,
	      flags::Flags, -- 2 bytes
	      ciaddr,yiaddr,siaddr,giaddr::IP.Addr,
	      chaddr::Eth.Addr, -- 16 bytes!
	      sname::String, -- null terminated, 64 bytes
	      file::String, -- null terminated, 128 bytes
	      options::Options -- upto 312 bytes for options
	    }
	    deriving (Show)

data Operation = BootRequest | BootReply deriving (Eq,Bounded,Enum,Show)
data Flags = Flags {broadcast::Bool} deriving (Eq,Show)

newtype Options = Options [Option] deriving (Eq,Show)

data Option
  = Pad -- 0
  | End -- 255
  | SubnetMask IP.Addr -- 1
  | TimeOffset -- 2
  | Routers [IP.Addr] -- 3
  | DNS_Servers [IP.Addr] -- 6
  | HostName String -- 12
  | DomainName String -- 15
  | BroadcastAddress IP.Addr -- 28
  | NTP_Servers [IP.Addr] -- 42
  | RequestedIPAddress IP.Addr -- 50
  | LeaseTime Word32 -- 51
  | OptionOverload Word8 -- 52
  | MessageType MessageType -- 53
  | ServerIdentifier IP.Addr -- 54
  | Unknown Word8 [Word8] -- unimplemented/unsupported option
  deriving (Eq,Show)

data MessageType
  = Discover -- 1
  | Offer
  | Request
  | Decline
  | Ack
  | Nak
  | Release
  | Inform -- 8
  deriving (Eq,Bounded,Enum,Show)

--------------------------------------------------------------------------------

template mac =
     Packet { opcode=BootRequest,
	      xid=0,
	      secs=0,
	      flags=Flags{broadcast=False},
	      ciaddr=z,yiaddr=z,siaddr=z,giaddr=z,
	      chaddr=mac,
	      sname="",
	      file="",
	      options=Options []
	    }
  where
    z = IP.Addr 0 0 0 0

--------------------------------------------------------------------------------

instance Parse Operation where parse = bounded 1 =<< word8
instance Parse MessageType where parse = bounded 1 =<< word8
instance Unparse MessageType where unparse t = unparse (unEnum 1 t::Word8)
instance Unparse Operation where unparse t = unparse (unEnum 1 t::Word8)

instance Parse Flags where
  parse = do w <- word16
	     return Flags{broadcast=testBit w 15}

instance Unparse Flags where
  unparse Flags{broadcast=b} = unparse (if b then 0x8000 else 0::Word16)

magic = [99,130,83,99::Word8]

instance Parse Options where
  parse = do bs <- bytes 4
	     if bs==magic 
		then Options # po
		else return (Options [])
    where
      po = do o <- parse
	      case o of
	        End -> return []
		_ -> (o:) # po

instance Unparse Options where
  unparse (Options []) = unparse ()
  unparse (Options os) = unparse (magic,os,End) -- pad to 312 bytes?

instance Unparse Option where
  unparse End = unparse (255::Word8)
  unparse Pad = unparse (0::Word8)
  unparse (RequestedIPAddress ip) = unparse  ([50,4::Word8],ip)
  unparse (MessageType t) = unparse ([53,1::Word8],t)
  unparse (ServerIdentifier ip) = unparse  ([54,4::Word8],ip)
  unparse (Unknown b bs) = unparse (b,n,bs)
    where n = fromIntegral (length bs)::Word8

instance Parse Option where
  parse = do b <- word8
	     case b of
	       0   -> return Pad
	       1   -> do check8 4
		         SubnetMask # parse
	       3   -> Routers # ips
	       6   -> DNS_Servers # ips
	       255 -> return End
	       51  -> return LeaseTime        #! check8 4 <# parse
	       53  -> return MessageType      #! check8 1 <# parse
	       54  -> return ServerIdentifier #! check8 4 <# parse
	       _   -> do n <- fromIntegral # word8
		         bs <- bytes n
		         return (Unknown b bs)
    where
      ips = do n <- fromIntegral # word8
               parses (n `div` 4)

parses n = sequence (replicate n parse)

instance Parse Packet where
  parse = Packet #  parse #! check8 1 #! check8 6 #! word8
		 <# parse <# parse <# parse
		 <# parse <# parse <# parse <# parse
		 <# parse
		 #! bytes 10
		 <# zstring 64
		 <# zstring 128
		 <# parse
		 #! therest

instance Unparse Packet where
  unparse (Packet op xid secs flags ci yi si gi ch sname file options) =
      unparse ((op,[1,6,0::Word8],xid,secs,flags),(ci,yi,si,gi,ch),
	       replicate 10 (0::Word8),
	       (zstring 64 sname,zstring 128 file),
	       options)
   where
     zstring n s = take n (s++repeat '\0')

--------------------------------------------------------------------------------

zstring :: Int -> PacketParser String
zstring n = map (toEnum.fromIntegral) . takeWhile (/=0) # bytes n

bounded z n = bounded' undefined (fromIntegral n-z)
  where
    bounded' :: (Bounded a,Enum a) => a -> Int -> PacketParser a
    bounded' r i =
	     if 0<=i && i<=fromEnum (maxBound `asTypeOf` r)
		then return (toEnum i)
		else fail "out of range"

unEnum z t = fromIntegral (fromEnum t+z)
