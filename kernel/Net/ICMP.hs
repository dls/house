module Net.ICMP where

-- Internet Control Message Protocol
-- http://rfc.net/rfc792.html

import Kernel.Bits
import Net.Utils
import Net.Packet
import Net.PacketParsing

data Packet = EchoRequest EchoMsg
	    | EchoReply EchoMsg
	    | Other { type_    :: !MessageType,
		      code     :: !Word8,
		      chksum   :: !Word16,
		      content  :: UArray Int Word8
		    }
	    deriving Show

data EchoMsg = Echo { ident       :: !Word16
                    , seqNum      :: !Word16
                    , echoData    :: UArray Int Word8
                    } deriving Show 

instance Parse Packet where
  parse = do (t,c,s) <- parse
	     case t of
	       Echo_Request -> EchoRequest # parse
	       Echo_Reply   -> EchoReply   # parse
	       _            -> Other t c s # parse

instance Parse EchoMsg where parse = Echo # parse <# parse <# parse

{-
-- XXX: Assuming only Echo messages
icmpParse              :: InPacket -> EchoMsg 
icmpParse p             = Echo 
                      { reply     = (p `byteAt` 0) == 0 
                      , ident     = p `wordAt` 4
                      , seqNum    = fromIntegral (p `wordAt` 6)
                      , echoData  = toUArray (p { from = from p + 8, len = len p - 8 })
                      }
-}

instance Unparse Packet where unparse = unparse . icmpUnparse

icmpUnparse (EchoRequest m) = echoUnparse False m
icmpUnparse (EchoReply m) = echoUnparse True m
--icmpUnparse (Other ...) = 

echoUnparse        :: Bool -> EchoMsg -> OutPacket
echoUnparse reply m = addChunk (array (0,7) (zip [0..] [a1,a2,a3,a4,b1,b2,b3,b4]))
                    $ addChunk (echoData m)
                    $ emptyOutPack
  where a1          = if reply then 0 else 8
        a2          = 0
        (a3,a4)     = (check .!. 1, check .!. 0)
        (b1,b2)     = (ident m .!. 1, ident m .!. 0)
        (b3,b4)     = (seqNum m .!. 1, seqNum m .!. 0)
        check       = checksum $ bytes_to_words_big ([a1,a2,0,0,b1,b2,b3,b4] ++ elems (echoData m))



data MessageType    = Echo_Reply
                    | Unknown1
                    | Unknown2
                    | Destination_Unreachable
                    | Source_Quench
                    | Redirect
                    | Unknown6
                    | Unknown7
                    | Echo_Request
                    | Unknown9
                    | Unknown10
                    | Time_Exceeded
                    | Parameter_Problem
                    | Timestamp
                    | Timestamp_Reply
                    | Information_Request
                    | Information_Reply
		    | UnknownOther
                      deriving (Eq,Enum,Bounded,Show)

instance Parse MessageType where
  parse = toEnum' # word8
    where toEnum' w = if n<fromEnum lo || n>fromEnum hi
		      then UnknownOther
		      else toEnum (fromEnum n)
	    where n=fromEnum w
	  lo,hi::MessageType
	  lo=minBound
	  hi=maxBound

{-

-- dependent type, the type determines the shape of 
-- code & content
data Header         = Header
                    { msgType     :: !MessageType
                    , msgCode     :: !Word8
                    , msgChecksum :: !Word16
                    , msgContent  :: ![Word8]
                    }








                    


--------------------------------------------------------------------------------
-- Destination unreachable
--------------------------------------------------------------------------------

data Dest           = Net_unreachable
                    | Host_unreachable
                    | Protocol_unreachable
                    | Port_unreachable
                    | Fragmentation_needed_and_DF_set
                    | Source_route_failed
                      deriving (Show,Enum)


--------------------------------------------------------------------------------
-- Time exceeded
--------------------------------------------------------------------------------

data TimeExceeded   = Time_to_live_exceeded_in_transit
                    | Fragment_reassembly_time_exceeded
                      deriving (Show,Enum)


--------------------------------------------------------------------------------
-- Parameter problem
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Source quench
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Redirect message
--------------------------------------------------------------------------------

data RedirectMsg    = Netwrok  
                    | Host
                    | ToS_Network
                    | ToS_Host 

--------------------------------------------------------------------------------
-- Echo & echo reply messages
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Time stamp & time stamp reply reply messages
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Information request & information reply message
--------------------------------------------------------------------------------

-}
