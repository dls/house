{-|
Driver for Intel 8255x 10/100 Mbps Ethernet Controllers
See http://www.intel.com/design/network/manuals/8255X_OpenSDM.htm
-}
module Kernel.Driver.Intel8255x.Driver(initPCI,initialize) where
import qualified Net.Concurrent as IO
import Control.Monad(when,unless,zipWithM_)
import Data.Array.Unboxed(listArray)

import qualified Net.Ethernet as Eth
import qualified Net.Interface as Net

import Net.Packet(toInPack,OutPacket,outLen)
import Net.PacketParsing(doParse)

import Kernel.PCI.Device(config,devInfo,rawClass,revision)
import Kernel.PCI.ConfigSpace(getB)
import Kernel.PCI.BaseAddr(AddrType(..),addr,getBAR)
import Kernel.Interrupts(registerIRQHandler)
import Kernel.Bits

import H.AdHocMem
import H.Concurrency(newChan,writeChan,readChan,yield,forkH,threadDelay)
import H.IOPorts(Port,inL,outL,inB,outB,outW,inW)
import H.Utils(ptrToWord32)
import H.Monad(liftIO)

import Kernel.Driver.Intel8255x.Util -- Storable instances etc

supported vendor device = vendor==0x8086 && device `elem` devices
  where devices = [0x1013]

initPCI putStrLn dev =
  do base <- getBAR dev 1
     let i=devInfo dev
     debug $ "Class and revision is "++showHex (rawClass i)
                                ++" "++showHex (revision i)
     debug $ "Base is "++show base
     case base of
       IO base ->
         do irq <- (toEnum . fromIntegral) `fmap` getB (config dev) 0x3C
	    debug $ "IRQ is "++show irq
	    Just `fmap` initialize debug irq (fromIntegral (addr base))
       _ -> return Nothing
  where
    debug = putStrLn . ("8255x: "++)

initialize debug irq base =
  do eventChan <- newChan
     rxChan <- newChan
     let irqHandler =
	   do cause <- inB (base+1)
	      outB (base+1) cause -- immediately acknowledge all interrupts
	      writeChan eventChan (CardEvent cause)
     registerIRQHandler irq irqHandler
     let io = Net.Interface { Net.rx = IO.readChan rxChan,
			      Net.tx = IO.writeChan eventChan . Tx }
         -- Should read the cards individual MAC address from the EEPROM !!
         --mac = Eth.Addr 0x52 0x54 0x00 0x12 0x34 0x56 -- quick hack
	 mac = Eth.Addr 0x00 0xD0 0x59 0x84 0xAE 0x2A -- quick hack, my thinkpad
     debug . ("SCB command & status = "++) . showHex =<< inL base
     forkH (driver debug base mac rxChan eventChan)
     return Eth.Interface { Eth.myMAC = mac, Eth.io = io }

--------------------------------------------------------------------------------

data Event          = CardEvent Word8
		    | Rx -- check buffer for more packets
                    | Tx (Eth.Packet OutPacket)

instance Show Event where
    show (CardEvent cause) = "CardEvent "++showHex cause
    show (Tx _)    = "Tx _"
    show Rx        = "Rx"

data DriverState = S { cuSt::CUState,ruSt::RUState,statBuf::Ptr Word32 }
type CommandPtr = Ptr Word32
type RFDPtr     = Ptr Word32
type CUState = Maybe (CommandPtr,[ActionCommand]) -- idle or (current,pending)
type RUState = (Bool,[RFDPtr]) -- (RU ready?,circular list of receive buffers)

driver debug base mac rxChan eventChan =
    do debug "Setup"
       state0 <- setup debug base mac
       debug "Entering main loop"
       loop state0
  where
    loop state = loop =<< handleEvent state =<< readChan eventChan

    handleEvent state event =
      do --debug $ show event
         --debug . ("SCB command & status = "++) . showHex =<< inL base
	 case event of
           CardEvent cause -> handleInterrupts cause state
	   Tx ethpacket -> withCU (txPacket ethpacket) state
           _ -> return state

    handleInterrupts cause state =
        bit 7 (withCU handleCX) =<<
        bit 6 (withRU handleFR) =<<
	bit 4 (withRU handleRNR) state
      where bit n f = if testBit cause n then f else return

    withCU m s@S{cuSt=cu,statBuf=buf} = do cu' <- m cu buf; return s{cuSt=cu'}
    withRU m s@S{ruSt=ru} = do ru' <- m ru; return s{ruSt=ru'}

    txPacket ethpacket cu _ = doAction debug base cu (Transmit ethpacket)

    handleCX cu statbuf =
	do --debug "CU command completion"
           --debug . ("SCB command & status = "++) . showHex =<< inL base
	   case cu of
	     Just (current,pending) ->
	       do --cmd <- peek current
                  --debug $ "completed command: "++ showHex cmd
		  --when (isTransmit cmd) $ showStats statbuf
		  --when (isTransmit cmd) $ debug "Transmit commmand completed"
		  free current
		  case pending of
		    [] -> return Nothing
		    cmd:cmds ->
		      do ptr <- startAction debug base cmd
			 return $ Just (ptr,cmds)
	     _ -> return cu
{-
    showStats statbuf =
        do tellCU debug base DumpStatisticalCounters
	   waitForSCBCommand debug base
	   let sh hdr cnt = debug (hdr++show cnt)
	   sh "Transmit good frames:           " =<< peek statbuf
	   sh "Transmit max collitions errors: " =<< peekElemOff statbuf 1
	   sh "Transmit underrun errors:       " =<< peekElemOff statbuf 3
	   sh "Transmit lost carrier sense:    " =<< peekElemOff statbuf 4
	   sh "Transmit deferred:              " =<< peekElemOff statbuf 5
	   sh "Transmit single collision:      " =<< peekElemOff statbuf 6
	   sh "Receive good frames:            " =<< peekElemOff statbuf 9
	   sh "Receive CRC errors:             " =<< peekElemOff statbuf 10
	   sh "Receive resource errors:        " =<< peekElemOff statbuf 12
-}
    handleRNR (_ready,bufs) =
      do debug "RNR (RU Not Ready)"
         return (False,bufs)

    handleFR (ready,bufs) =
       do (n,bufs') <- handleFR' 0 bufs
	  --debug $ "Received number of frames: "++show n
	  when ({-n>0 &&-} not ready) $ tellRU debug base RU_Resume
	  return (True,bufs')

    handleFR' cnt ru@(last:ru'@(first:_)) =
        do status <- peek first
	   if testBit status 15 -- complete?
	     then do count <- peekElemOff first 3
	             --debug $ "Frame Received, status & count: "
                     --        ++showHex status++" "++showHex count
	             receiveFrame (fromIntegral (count .&. 0x3fff))
	     else return (cnt,ru)
      where
        receiveFrame size =
	  do -- Copy and deliver received frame:
	     bytes <- fmap toInPack (peekArray (first `plusPtr` 16) size)
             maybe err (writeChan rxChan) (doParse bytes)
             -- Prepare buffer for new packet:
	     poke first (bit 30) -- set suspend bit in this frame, reset status
	     pokeByteOff first 12 (0::Word16) -- clear EOF & F flags
	     pokeByteOff last 2 (0::Word16) -- reset S bit in previous buffer
	     handleFR' (cnt+1) ru'
        err = debug "Short ethernet packet recevied"

setup debug base mac =
    do outL (base+8) 0 -- PORT Software Reset command (Section 6.3.3.1)
       threadDelay 1000 -- microseconds
       disableIRQs base
       detectPHY
       cu0 <- setupCU
       --cu1 <- doAction debug base cu0 NOP
       cu1 <- doAction debug base cu0 (Configure configuration)
       cu <- doAction debug base cu1 (IndividualAddressSetup mac)
       statbuf <- mallocBytes 128
       tellCU debug base (LoadDumpCountersAddress (ptrToWord32 statbuf))
       ru <- setupRU
       enableIRQs base
       return S{cuSt=cu,ruSt=ru,statBuf=statbuf}
  where
    setupCU =
      do tellCU debug base (Load_CU_Base 0)
         return Nothing -- Nothing -> Idle,
                        -- Just (current,cmds) -> busy, commands pending

    setupRU =
      do tellRU debug base (Load_RU_Base 0)
	 bufs@(last:first:_) <- setupRFA 50
         -- load HDS (Header Data Size)?
         tellRU debug base (RU_Start (ptrToWord32 first))
         return (True,bufs)

    -- Section 7.2.3
    detectPHY =
      do w1 <- readMDI 2 -- PHY Identification Register (Word 1)
	 w2 <- readMDI 3 -- PHY Identification Register (Word 2)
	 let w = w1 `nextTo` w2 :: Word32
	 let manufacturer = w `shiftR` 10
	     model = (w `shiftR` 4) .&. 0x3f
	     revision = w .&. 0xf
	 debug $ "PHY manufacturer, model, revision = "
		   ++showHex manufacturer++", "
		   ++showHex model++", "
		   ++showHex revision

    -- Section 6.3.5
    mdi = base + 0x10 -- MDI control register (interface to the PHY)
    phy = 1 -- default PHY address (Section 7.1)

    -- Section 6.3.5.3
    readMDI reg =
      do waitForMDI
	 writeMDI' 2 reg 0
	 waitForMDI
	 inW mdi

    writeMDI = writeMDI' 1

    -- Section 6.3.5.2
    writeMDI' op reg dat =
      do waitForMDI
         outL mdi (shiftL op 26 .|. shiftL phy 21 .|. shiftL reg 16 .|. 
		   fromIntegral (dat::Word16)) -- Section 6.3.5.1

    waitForMDI =
      do b <- inL mdi
	 unless (testBit b 28) $ do debug "waiting for MDI"
				    yield
				    waitForMDI
doAction debug base cu cmd =
  case cu of
    Just (current,pending) -> return $ Just (current,pending++[cmd])
    _ -> do ptr <- startAction debug base cmd
	    return $ Just (ptr,[])

startAction debug base cmd =
  do ptr <- storeSingleAction cmd
     tellCU debug base (CU_Start (ptrToWord32 ptr))
     return ptr

--------------------------------------------------------------------------------

disableIRQs base = outL (base+3) 1 -- disable all
enableIRQs  base = outL (base+3) 0 -- enable all

tellCU debug base = showit debug "CU" $
                    writeSCBCommandWord debug base . unparseCUCmd
tellRU debug base = showit debug "RU" $
                    writeSCBCommandWord debug base . unparseRUCmd
showit debug u doit cmd = do --debug $ "Tell "++u++" "++show cmd
		             doit cmd

-- When we write a command word, we leave the interrupt mask bits unchanged.
writeSCBCommandWord debug base (cmdbits,optptr) =
  do waitForSCBCommand debug base
     maybe (return ()) (outL (base+4)) optptr
     outB (base+2) cmdbits

-- Wait until previous command has been accepted before issuing a new command
waitForSCBCommand debug base =
    do cmd <- inB (base+2)
       when (cmd/=0) (debug "waiting" >> yield >> waitForSCBCommand debug base)

--------------------------------------------------------------------------------

-- Command Unit commands (to be written the SCB command Word at base+0,
-- and the SCB General Pointer at base+4 when a pointer argument is present)
data CUCmd                          -- Bits 23:20
  = CU_Nop                          -- 0000
  | CU_Start CB                     -- 0001
  | CU_Resume                       -- 0010
-- | CU_HPQ_Start CB                -- 0011
  | LoadDumpCountersAddress Word32  -- 0100
  | DumpStatisticalCounters         -- 0101
  | Load_CU_Base Word32             -- 0110
  | DumpAndResetStatisticalCounters -- 0111
-- | CU_StaticResume                 -- 1010 -- only in 82558 and later devices
-- | CU_HPQ_Resume                  -- 1011
  deriving (Show)

unparseCUCmd cmd =
  case cmd of
    CU_Nop                          -> noptr 0
    CU_Start cbl                    -> ptr   1 cbl
    CU_Resume                       -> noptr 2
--  CU_HPQ_Start CB                 -> 
    LoadDumpCountersAddress addr    -> ptr   4 addr
    DumpStatisticalCounters         -> noptr 5
    Load_CU_Base base               -> ptr   6 base
    DumpAndResetStatisticalCounters -> noptr 7
--  CU_StaticResume                 -> noptr 10
--  CU_HPQ_Resume                   -> noptr 11
  where
    noptr bits     = (cu bits,Nothing)
    ptr   bits ptr = (cu bits,Just (ptr::Word32))
    cu bits = shiftL bits 4::Word8

-- Pointer to a Command Block List (relative to CU Base):
type CB = Word32

--------------------------------------------------------------------------------

data RUCmd                    -- Bits 18:16
  = RU_Nop                    -- 000
  | RU_Start RFD              -- 001
  | RU_Resume                 -- 010
-- | Receive_DMA_Redirect RFD  -- 011 -- only in 82558 and later devices
  | RU_Abort                  -- 100
  | Load_HDS Word16           -- 101 -- 14 bit non-zero, even number
  | Load_RU_Base Word32       -- 110
  deriving (Show)

-- Pointer to Receive Frame Area (relative to RU Base):
type RFD = Word32

unparseRUCmd cmd =
  case cmd of
    RU_Nop                    -> noptr 0
    RU_Start rfd              -> ptr   1 rfd
    RU_Resume                 -> noptr 2
--  Receive_DMA_Redirect rfd  -> ptr   3 rfd
    RU_Abort                  -> noptr 4
    Load_HDS hds              -> ptr   5 (fromIntegral hds)
    Load_RU_Base base         -> ptr   6 base
  where
    noptr bits     = (cu bits,Nothing)
    ptr   bits ptr = (cu bits,Just ptr)
    cu bits = bits::Word8

--------------------------------------------------------------------------------

-- Commands that can be part of the CBL

data ActionCommand                       -- Bits 18:16 of first word
    = NOP                                -- 000
    | IndividualAddressSetup Eth.Addr    -- 001
    | Configure [Word8]                  -- 010,  8-22 configuration bytes
--  | MulticastAddressSetup ...          -- 011
    | Transmit (Eth.Packet OutPacket)    -- 100
--  | LoadMicrocode ...                  -- 101
--  | Dump ...                           -- 110
--  | Diagnose ...                       -- 111
    --deriving (Show)

transmitTag = 4
isTransmit cmd = cmdtag cmd==transmitTag
cmdtag cmdword = (cmdword `shiftR` 16) .&. 7

storeSingleAction cmd =
  case cmd of
    NOP -> -- Section 6.4.2.1
      do ptr <- mallocBytes 8
	 pokeHeader ptr 0
	 return ptr
    IndividualAddressSetup addr -> -- Section 6.4.2.2
      do ptr <- mallocBytes 16
	 pokeHeader ptr 1
	 pokeByteOff ptr 8 addr
	 return ptr
    Configure bs -> -- Section 6.4.2.3
      do ptr <- mallocBytes (8+length bs)
	 pokeHeader ptr 2
	 zipWithM_ (pokeByteOff ptr) [8..] bs
	 return ptr
    Transmit ethpacket -> -- Section 6.4.2.5
      do ptr <- mallocBytes (16+psize)
	 pokeHeader ptr transmitTag
	 pokeElemOff ptr 2 nullPtr -- TBD Array (not used in simplified mode)
	 pokeElemOff ptr 3 (    0                   -- TBD Number = 0
                            .|. (1 `shiftL` 16)     -- Transmit Threshold
			    .|. bit 15              -- EOF (simplified mode)
		            .|. fromIntegral psize) -- TCB Byte Count
	 pokeOutPacket (ptr `plusPtr` 16) pack
	 return ptr
      where
	pack = Eth.unparse ethpacket
	psize0 = outLen pack
	psize = psize0 -- requires hardware padding to be enabled

  where
    pokeHeader ptr cmdtag =
      do poke ptr -- EL=1, I=1, CMD=cmdtag
                  (bit 31 .|. bit 29 .|. (cmdtag `shiftL` 16))
	 pokeElemOff ptr 1 nullPtr

    nullPtr = 0xffffffff::Word32

--------------------------------------------------------------------------------
-- Section 6.4.3.1

setupRFA bufcount =
  do ptrs0@(last:first:_) <- sequence (replicate (max 2 bufcount) mallocBuf)
     let ptrs = cycle ptrs0
     zipWithM_ initRFD ptrs0 (tail ptrs)
     poke last (hdr .|. bit 30) -- set the Suspend flag in the last frame
     return (cycle ptrs)
  where
    mallocBuf = mallocBytes (16+bufsize)
    bufsize = 1536 -- max ethernet packet size is 1522

    hdr = 0::Word32

    initRFD ptr next =
      do poke ptr hdr
	 pokeElemOff ptr 1 (ptrToWord32 next)
	 pokeElemOff ptr 3 (fromIntegral bufsize `shiftL` 16)


--------------------------------------------------------------------------------

-- Section 6.4.2.3.1
configuration :: [Word8]
configuration = fromIntegral (1+length bytes):bytes
  where
    bytes =
      [0x08, -- byte  1: Transmit FIFO limit=0, Receive FIFO limit=8
       0,    -- byte  2: Adaptive IFS
       0,    -- byte  3: reserved on 82557 (must be 0)
       0,    -- byte  4: Receive DMA Maximum Byte Count
       0,    -- byte  5: Bit 7: DMA Maximum Byte Count Enable
             --          Bit 6:0: Transmit DMA Maximum Byte Count
       0x32, -- byte  6: Bit 7: Save Bad Frames
             --          Bit 6: Discard Overrun Receive Frames
             --          Bit 5: Extended Statistical Counter(must be 1 on 82557)
             --          Bit 4: Extended TxCB (must be 1 on 82557)
             --          Bit 3: CU Idle Interrupt
             --          Bit 2: 82557: TNO, 82559: TCO statisical counters
             --          Bit 1: reserved, must be 1
             --          Bit 0: Late SCB (must be 0 on 82558 and 82559)
       0x03, -- Byte  7: Bit 7-6: must be 0 on 82557
             --          Bit 5-3: reserved, must be 0
             --          Bit 2-1: Underrun Retry (0-3 retransmissions)
             --          Bit 0: Discard Short Receive (1=discard)
       0x01, -- Byte  8: Bit 7-1: must be 0 on 82557
             --          Bit 0: must be 1 on 82558 and 82559
             --                 should be 0 on 82557+82503 (need PHY detection)
       0,    -- Byte  9: must be 0 on 82557
       0x2e, -- Byte 10: Bit 7-6: Loopback (00=normal operation)
             --          Bit 5-4: Preamble (10 recommended)
             --          Bit 3: 1=No Source Address Insertion
             --          Bit 2-0, reserved, must be 110
       0,    -- Byte 11: must be 0 on 82558 and 82559
       0x60, -- Byte 12: interspace framing
       0,    -- Byte 13: must be 0 on 82557
       0xf2, -- Byte 14: reserved on 82557 and 82559
       0x48, -- Byte 15: Bit 1: broadcast disable
             --          Bit 0: promiscuous
       0,    -- Byte 16: reserved on 82557 (flow control on 82558 and 82559)
       0x40, -- Byte 17: reserved on 82557 (flow control on 82558 and 82559)
       0xf2, -- Byte 18: Bit 1: padding enable
       0x80, -- Byte 19: Bit 7: Full Duplex Pin Enable
       0x3f, -- Byte 20
       0x05] -- Byte 21
