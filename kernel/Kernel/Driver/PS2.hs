module Kernel.Driver.PS2
    ( launchKeyboardDriver
    , resetProcessor
    , launchMouseDriver
    ) where
{-P:
import Prelude hiding (putStr,putStrLn)
-}
--import Control.Concurrent ( yield )
--import Control.Concurrent.Chan
import Data.Bits
import Data.Word
import Control.Monad(when)

--import Kernel.Debug
import Kernel.Interrupts(registerIRQHandler)
import H.Monad(H)
import H.Concurrency(yield,Chan,newChan,writeChan)
import H.IOPorts
import H.Interrupts (IRQ(..),enableIRQ,disableIRQ)

keyboardIRQ :: IRQ
keyboardIRQ = IRQ1

mouseIRQ = IRQ12 :: IRQ

ps2DataPort :: Port
ps2DataPort = 0x60

ps2CtrlPort :: Port
ps2CtrlPort = 0x64

ps2StatPort :: Port
ps2StatPort = 0x64 -- XXX: What to do ??


readModeCommand :: Word8
readModeCommand = 0x20 -- Read mode bits

writeModeCommand :: Word8
writeModeCommand = 0x60 -- Write mode bits

getVersionCommand :: Word8
getVersionCommand = 0xA1 -- Get controller version

disableMouseCommand :: Word8
disableMouseCommand = 0xA7 -- Disable mouse interface

enableMouseCommand :: Word8
enableMouseCommand = 0xA8 -- Enable mouse interface

testMouseCommand :: Word8
testMouseCommand = 0xA9 -- Mouse interface test

selfTestCommand :: Word8
selfTestCommand = 0xAA -- Controller self test

keboardTestCommand :: Word8
keboardTestCommand = 0xAB -- Keyboard interface test

disableKeyboardCommand :: Word8
disableKeyboardCommand = 0xAD -- Keyboard interface disable

enableKeyboardCommand :: Word8
enableKeyboardCommand = 0xAE -- Keyboard interface enable

readOutputPortCommand :: Word8 -- Read from output port
readOutputPortCommand = 0xD0

writeOutputPortCommand :: Word8 -- Write to output port 
writeOutputPortCommand = 0xD1

writeAuxOutputBufferCommand :: Word8
writeAuxOutputBufferCommand = 0xD3 -- Write to output buffer as if
                                   -- initiated by the auxiliary device
writeMouseCommand :: Word8
writeMouseCommand = 0xD4 -- Write the following byte to the mouse

{-
If we had Template Haskell inside hOp, we could do this :

$(mkAccessors
  (     flag "ParityError"
    <<> flag "GeneralTimeout"
    <<> flag "MouseOutputFull"
    <<> flag "KeyboardUnlocked"
    <<> flag "CommandWrote"
    <<> flag "SelfTestSucessful"
    <<> flag "InputFull"
    <<> flag "KeyboardOutputFull"
  ))

$(mkAccessors $
  (     flag "RFUMode"
    <<> flag "KCCMode"
    <<> flag "MouseDisabled"
    <<> flag "KeyboardDisabled"
    <<> flag "NoKeyLockMode"
    <<> flag "SystemMode" -- ?
    <<> flag "MouseInterruptMode"
    <<> flag "KeyboardInterruptMode"
  ))
-}

setKeyboardInterruptMode w = setBit w 0
setMouseInterruptMode w = setBit w 1
setSystemMode w = setBit w 2
setKCCMode w = setBit w 6

testKeyboardOutputFull w = testBit w 0
testMouseOutputFull w = testBit w 5
testInputFull w = testBit w 1

defaultMode :: Word8
defaultMode = setKeyboardInterruptMode $
	      setMouseInterruptMode $
	      setSystemMode $
	      setKCCMode 0

getDeviceIDCommand :: Word8
getDeviceIDCommand = 0xF2

setRateCommand :: Word8
setRateCommand = 0xF3

enableScanningCommand :: Word8
enableScanningCommand = 0xF4

disableScaningCommand :: Word8
disableScaningCommand = 0xF5

setDefaultsCommand = 0xF6 :: Word8

resetCommand :: Word8
resetCommand = 0xFF

powerOnResetReply :: Word8
powerOnResetReply = 0xAA

ackReply :: Word8
ackReply = 0xFA

resendReply :: Word8
resendReply = 0xFE

commandTimeOut = 750

--readStatus :: H Word8
readStatus = inB ps2StatPort

--readData :: H Word8
readData = inB ps2DataPort

--waitWrite :: H ()
waitWrite =
    do status <- readStatus
       if not (testInputFull status)
	  then return ()
	  else do yield  -- FIXME: Should we bound this ?
	          waitWrite


waitRead =
    do status <- readStatus
       if testKeyboardOutputFull status
	  then return status
	  else do -- yield  -- FIXME: Should we bound this ?
	          waitRead

--writeData :: Word8 -> H ()
writeData w =
    do waitWrite
       outB ps2DataPort w

--writeController :: Word8 -> H ()
writeController w =
    do waitWrite
       outB ps2CtrlPort w

writeMouse w =
  do writeController writeMouseCommand
     writeData w
     getData "mouse cmd ack"

--getMode :: H Word8 -- Mode?
getMode =
    do writeController readModeCommand
       readData

--setMode :: Word8 {- Mode? -} -> H ()
setMode mode =
    do writeController writeModeCommand
       writeData mode

--enableKeyboard :: H ()
enableKeyboard = writeController enableKeyboardCommand

--enableMouse :: H ()
enableMouse = writeController enableMouseCommand

--disableKeyboard :: H ()
disableKeyboard = writeController disableKeyboardCommand

launchKeyboardDriver :: H (Chan Word8)
launchKeyboardDriver =
    do --putStrLn "Launching keyboard driver."
       chan <- newChan
       --putStrLn "> got channel"
       enableKeyboard
       --putStrLn "> keyboard enabled"
       let --handler :: H ()
	   handler = readScanCodes
	   --readScanCodes :: H ()
	   readScanCodes =
	       do status <- readStatus
		  if testOutputFull status
		     then do w <- readData
			     writeChan chan w
			     readScanCodes
		     else return ()
       registerIRQHandler keyboardIRQ handler
       --putStrLn "Keyboard driver launched."
       return chan

testOutputFull :: Word8 {- Status? -} -> Bool
testOutputFull status = testKeyboardOutputFull status
			&& not (testMouseOutputFull status)

launchMouseDriver :: H (Chan Word8)
launchMouseDriver =
    do --putStrLn "Launching mouse driver."
       chan <- newChan
       --putStrLn "> got channel"
       let handler = do status <- readStatus
			if testMouseOutputFull status
			   then do writeChan chan =<< readData
				   -- read only one byte per interrupt
				   --handler
			   else return ()

       n <- flushData
       --when (n>0) $ putStrLn ("Flushed "++show n)
       --disableKeyboard
       --readAck "Disable Keyboard"
       --irq0 <- inB (0x21::Word8IOPort)
       --disableIRQ 1

       setMode defaultMode
       --readAck "Set Mode"
       enableMouse
       --readAck "Enable Mouse"
       --putStrLn "> mouse enabled"
       writeMouse getDeviceIDCommand
       getData "Device ID"
       --writeMouse setDefaultsCommand -- set defaults (incl enable stream mode)
       writeMouse enableScanningCommand -- enable stream mode
       registerIRQHandler mouseIRQ handler
       --putStrLn "Mouse driver launched."

       enableKeyboard
       --readAck "Enable Keyboard"
       --if testBit irq0 1 then return () else enableIRQ 1
       
       return chan

getData _ =
  do waitRead
     readData
{-
getData msg =
  do putStr (msg++": ")
     status <- waitRead
     putStr ("status "++ showHex status)
     putStrLn . (" data "++) . showHex =<< readData
-}

flushData =
  do status <- readStatus
     if testKeyboardOutputFull status
       then readData >> fmap succ flushData
       else return 0
{-
showHex w = [hex!!(n `div` 16),hex!!(n `mod` 16)]
  where
    n = fromIntegral w
    hex="0123456789ABCDEF"
-}

resetProcessor :: H ()
resetProcessor =
    do writeController readOutputPortCommand
       outputPort <- readData
       writeController writeOutputPortCommand
       writeData (outputPort .|. 1) 
       resetProcessor
