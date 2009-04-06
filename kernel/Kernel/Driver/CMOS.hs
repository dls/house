module Kernel.Driver.CMOS
    ( getRawTime,
      clearShutdownStatus
    ) where

import Data.Bits
import Data.Word

import H.Unsafe (unsafePerformH)
import H.IOPorts
import H.Concurrency(QSem,newQSem,withQSem)

{-# NOINLINE s_cmosAccess #-}
s_cmosAccess :: QSem
s_cmosAccess = unsafePerformH $ newQSem 1

-- Port number for CMOS address register
prw8AddressPort :: Port
prw8AddressPort = 0x70

-- Port number for CMOS data register
prw8DataPort :: Port
prw8DataPort = 0x71

-- CMOS Registers
type CMOSRegister = Word8

rtcSeconds :: CMOSRegister
rtcSeconds = 0x00

rtcAlarmSeconds :: CMOSRegister
rtcAlarmSeconds = 0x01

rtcMinutes :: CMOSRegister
rtcMinutes = 0x02

rtcAlarmMinutes :: CMOSRegister
rtcAlarmMinutes = 0x03

rtcHours :: CMOSRegister
rtcHours = 0x04

rtcAlarmHours :: CMOSRegister
rtcAlarmHours = 0x05

rtcDayOfWeek :: CMOSRegister
rtcDayOfWeek = 0x06

rtcDayOfMonth :: CMOSRegister
rtcDayOfMonth = 0x07

rtcMonth :: CMOSRegister
rtcMonth = 0x08

rtcYear :: CMOSRegister
rtcYear = 0x09

rtcControl :: CMOSRegister
rtcControl = 0x0B

shutdownStatus :: CMOSRegister
shutdownStatus = 0x0F

floppyDrives :: CMOSRegister
floppyDrives = 0x10

testDMBinary w = testBit w 2

--getRegister :: CMOSRegister -> H Word8
getRegister reg =
    -- We need to keep the bit 7 of address data :
    -- it controls the NMI
    do addr <- inB prw8AddressPort
       outB prw8AddressPort (keepBit addr reg 7)
       inB prw8DataPort

--setRegister :: CMOSRegister -> Word8 -> H ()
setRegister reg value =
    -- We need to keep the bit 7 of address data :
    -- it controls the NMI
    do addr <- inB prw8AddressPort
       outB prw8AddressPort (keepBit addr reg 7)
       outB prw8DataPort value

keepBit :: (Bits a) => a -> a -> Int -> a
keepBit src dest pos =
    if testBit src pos
       then setBit dest pos
       else clearBit dest pos

bcd2bin :: (Bits a) => a -> a
bcd2bin bcd = (bcd .&. 15) + ((shiftR bcd 4) * 10)

bin2bcd :: (Bits a, Integral a) => a -> a
bin2bcd bin =
    let (d, r) = bin `divMod` 10
     in shiftL d 4 + r

--getRawTime :: H (Word8, Word8, Word8, Word8, Word8, Word8)
getRawTime = withQSem s_cmosAccess $
    do control <- getRegister rtcControl
       let f = if testDMBinary control
	          then return
		  else return . bcd2bin
       year <- getRegister rtcYear >>= f
       month <- getRegister rtcMonth >>= f
       day <- getRegister rtcDayOfMonth >>= f
       hour <- getRegister rtcHours >>= f
       min <- getRegister rtcMinutes >>= f
       sec <- getRegister rtcSeconds >>= f
       return (year, month, day, hour, min, sec)

--clearShutdownStatus :: H ()
clearShutdownStatus = withQSem s_cmosAccess $
    setRegister shutdownStatus 0
