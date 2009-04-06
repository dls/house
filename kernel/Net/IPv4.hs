module Net.IPv4 where

-- Internet Protocol
-- reference: http://rfc.net/std0005.html

import Data.Char
import Kernel.Bits
import Net.Utils
import Net.Packet
import Net.PacketParsing

data Addr           = Addr !Word8 !Word8 !Word8 !Word8
                      deriving (Eq,Ord)

loopbackAddr = Addr 127 0 0 1
broadcastAddr a = a `orAddr` complAddr (defaultNetmask a)

type Netmask = Addr
netmaskA = Addr 255 0 0 0
netmaskB = Addr 255 255 0 0
netmaskC = Addr 255 255 255 0

defaultNetmask (Addr b _ _ _)
  | b<128 = netmaskA
  | b<192 = netmaskB
  | b<224 = netmaskC

sameNet (netIP,netmask) ip = ip `andAddr` netmask == netIP `andAddr` netmask

liftA1 f (Addr b1 b2 b3 b4) = Addr (f b1) (f b2) (f b3) (f b4)
liftA2 f (Addr a1 a2 a3 a4) (Addr b1 b2 b3 b4) =
  Addr (f a1 b1) (f a2 b2) (f a3 b3) (f a4 b4)

andAddr   = liftA2 (.&.)
orAddr    = liftA2 (.|.)
complAddr = liftA1 complement

--instance Bits Addr where 
-- We don't need numeric operations or shifting...
-- It's a pity that the is only one big monolithic Bits class...

instance Parse Addr where
  parse = Addr # parse <# parse <# parse <# parse

instance Unparse Addr where
  unparse (Addr b1 b2 b3 b4) = unparse (b1,b2,b3,b4)

instance Show Addr where
  show (Addr a b c d)
                    = show a ++ "." 
                   ++ show b ++ "." 
                   ++ show c ++ "." 
                   ++ show d 

instance Read Addr where 
  readsPrec _ s = [(Addr a b c d,r)|(a,r1)<-num s,  (_,r2)<-dot r1,
		                    (b,r3)<-num r2, (_,r4)<-dot r3,
		                    (c,r5)<-num r4, (_,r6)<-dot r5,
		                    (d,r )<-num r6]
    where dot s = [((),r)|'.':r<-[s]]
	  num s = [(read n,r)|(n@(_:_),r)<-[span isDigit s]]

-- 3 bits
data Precedence     = Routine
                    | Priority 
                    | Immediate 
                    | Flash 
                    | Flash_Override  
                    | CRITIC_ECP 
                    | Internetwork_Control 
                    | Network_Control 
                      deriving (Show,Enum)
                      
instance Parse Precedence where
  parse = toEnum # bits 3

-- an IP version 4 packet
data Packet content = Packet
                    { version       :: !Word8{-4-}
                    , headerLen     :: !Int{-4-} -- in units of 4 bytes
		    , tos           :: !TypeOfService{-8-}
                    , totalLen      :: !Word16 -- in bytes
                    , identifier    :: !Word16 
                    , flags         :: !Flags{-3-}
                    , fragOff       :: !Word16{-13-}  -- in units of 8 bytes
                    , timeToLive    :: !Word8{-8-}
                    , protocol      :: !Protocol{-8-}
                    , headerCheck   :: !Word16
                    , source, dest  :: !Addr
                    , options       :: ![Word8]
                    , content       :: !content
                    } deriving Show

data TypeOfService  = TOS
                    { precedence    :: !Precedence{-3-}
                    , lowDelay      :: !Bool{-1-}
                    , highThrough   :: !Bool{-1-}
                    , highReal      :: !Bool{-1-} 
                   -- reserved     :: Bits 2 -- should be zero
                    }
                   deriving Show

instance Parse TypeOfService where
  parse = TOS # parse <# parse <# parse <# parse #! skip 2

data Flags          = Flags
                    { -- reserved   :: Bits 1 -- should be zero
                      don'tFrag     :: !Bool{-1-}
                    , moreFrags     :: !Bool{-1-}
                    }
                   deriving Show

instance Parse Flags where
  parse = return Flags #! skip 1 <# parse <# parse

-- For skipping upto 32 bits:
skip :: Int -> PacketParser Word32
skip = bits

template proto src dst body =
           -- A quick hack, I haven't read the RFC... /TH
           Packet   { version       = 4
                    , headerLen     = 5 -- minimum header length
                    , tos           = TOS Routine False False False
                    , totalLen      = 0
                    , identifier    = 0
                    , flags         = Flags False False
                    , fragOff       = 0
                    , timeToLive    = 64
                    , protocol      = proto
                    , headerCheck   = 0
                    , source        = src
                    , dest          = dst
                    , options       = []
                    , content       = body
                    }

instance Functor   Packet where fmap f p = p { content = f (content p) }
instance Container Packet where contents = content

data Protocol       = ICMP 
                    | TCP
                    | UDP 
                    | Unknown !Word8
                    deriving (Show,Eq)

num_prot           :: [(Int,Protocol)]
num_prot            = [ (1,ICMP)
		      , (6,TCP)
                      , (17,UDP)
                      ]

prot_num           :: [(Protocol,Int)]
prot_num            = map swap num_prot
  where swap (x,y)  = (y,x)

instance Enum Protocol where
  fromEnum (Unknown x)  = fromIntegral x
  fromEnum x            = case lookup x prot_num of
                            Just n -> n
                            _      -> error ("bug: Protcol number for " ++ show x ++ " is missing.")

  toEnum x              = case lookup x num_prot of
                            Nothing -> Unknown (fromIntegral x)
                            Just x  -> x

instance Parse Protocol where
  parse = toEnum . fromIntegral # word8

instance Unparse Protocol where
  unparse p = unparse (b::Word8)
     where b = fromIntegral (fromEnum p)

-- TODO:
data Option         = Short Word8
                    | Long 
                        { optType :: OptType
                        , optLen  :: Word8    -- includes type & self
                        , optData :: [Word8] }

data OptType        = OptType 
                        { optCopied :: Bool     {-1-}
                        , optClass  :: OptClass {-2-}
                        , optNumber :: Word8    {-5-}
                        } 

data OptClass       = Control | Reserved1 | DebugMeasure | Reserved4
                      deriving Enum
                            

--instance Parse (Packet InPacket) where parse = ipv4parse # therest

instance Parse contents => Parse (Packet contents) where
  parse =
    do v    <- bits 4     -- version       :: !Word8{-4-}
       hl   <- bits 4     -- headerLen     :: !Int{-4-} -- in units of 4 bytes
       let olen = (hl-5)*4  -- length of options and padding
       tos <- parse       -- tos           :: !TypeOfService
       totlen <- parse    -- totalLen      :: !Int{-16-} -- in bytes
       let datalen = fromIntegral totlen - 4*hl
       Packet v hl tos totlen
	     # parse      -- identifier    :: !Word16 
	    <# parse      -- flags         :: !Flags{-3-}
	    <# bits 13    -- fragOff       :: !Int{-13-}  -- in units of 8 bytes
	    <# parse      -- timeToLive    :: !Word8{-8-}
	    <# parse      -- protocol      :: !Protocol
	    <# parse      -- headerCheck   :: !Word16
	    <# parse      -- source        :: !Addr
	    <# parse      -- dest          :: !Addr
	    <# bytes olen -- options       :: ![Word8]
            #! trunc datalen -- discard padding
	    <# parse      -- content       :: !content

{-
ipv4parse          :: InPacket -> Packet InPacket
ipv4parse p         = let headerLen   = fromIntegral (a1 .&. 0x0F)
                          hdrByteLen  = headerLen * 4
                          optBytes    = 4 * headerLen - 20 
                          totLen      = fromIntegral (p `wordAt` 2)
                          a1          = p `byteAt` 0
                          a2          = p `byteAt` 1 
                          b34         = p `wordAt` 6
                      in Packet
                           { version       = a1 `shiftR` 4 
                           , headerLen     = headerLen             
                           , precedence    = toEnum (fromIntegral ((a2 .&. 0xE0) `shiftR` 5))
                           , lowDelay      = a2 `testBit` 4    
                           , highThrough   = a2 `testBit` 3
                           , highReal      = a2 `testBit` 2
                           , totalLen      = totLen 
                           , identifier    = p `wordAt` 4
                           , don'tFrag     = b34 `testBit` 14
                           , moreFrags     = b34 `testBit` 13
                           , fragOff       = fromIntegral (0x1FFF .&. b34)
                           , timeToLive    = fromIntegral (p `byteAt` 8)
                           , protocol      = toEnum (fromIntegral (p `byteAt` 9))
                           , headerCheck   = p `wordAt` 10
                           , source        = Addr (p `byteAt` 12)(p `byteAt` 13)(p `byteAt` 14)(p `byteAt` 15)
                           , dest          = Addr (p `byteAt` 16)(p `byteAt` 17)(p `byteAt` 18)(p `byteAt` 19)
                           , options       = [] -- XXX: take optBytes rest
                           , content       = p { from = from p + hdrByteLen, len = totLen - hdrByteLen }
                           }
-}

instance Unparse a => Unparse (Packet a) where
  unparse p = unparse (ipv4unparse (fmap doUnparse p))

ipv4unparse        :: Packet OutPacket -> OutPacket
ipv4unparse p       = addChunk realHeader (content p)
  where -- computed fields
        hL          = 5 + optWords
        optLen      = length (options p)    --- XXX
        optWords    = (optLen+3) `div` 4
        padLen      = 4 * optWords - optLen
        tL          = hL * 4 + outLen (content p)

        realHeader  = listArray (0,hL * 4 - 1) (header c3 c4)
        header c3 c4 = 
                     [ a1 , a2 , a3 , a4 
                     , b1 , b2 , b3 , b4
                     , c1 , c2 , c3 , c4
                     , d1 , d2 , d3 , d4
                     , e1 , e2 , e3 , e4 ]
                     ++ options p ++ replicate padLen 0

        -- yuk
        -- perhaps use a diff array here
        -- then the list in OutPacket may be made out of different arrays
        check       = checksum (bytes_to_words_big (header 0 0))


        -- 1
        t           = tos p
        a1          = (4 `shiftL` 4) .|. (fromIntegral hL .&. 0x0F)
        a2          = bit 4 (lowDelay t)
                    $ bit 3 (highThrough t)
                    $ bit 2 (highReal t) 
                    $ (fromIntegral (fromEnum (precedence t)) `shiftL` 5) 
        a3          = tL .!. 1
        a4          = tL .!. 0

        -- 2
        f           = flags p
        b1          = identifier p .!. 1
        b2          = identifier p .!. 0
        b3          = bit 6 (don'tFrag f)
                    $ bit 5 (moreFrags f)
                    $ (fragOff p .!. 1)
        b4          = fragOff p .!. 0  

        -- 3
        c1          = fromIntegral (timeToLive p)
        c2          = fromIntegral $ fromEnum $ protocol p
        c3          = check .!. 1
        c4          = check .!. 0

        -- 4
        Addr d1 d2 d3 d4  = source p
        
        -- 5
        Addr e1 e2 e3 e4  = dest p

        bit n b a   = if b then a `setBit` n else a
