-- | This module defines an abstraction for configuration spaces.
module Kernel.PCI.ConfigSpace 
  ( H, ConfigSpace, configSpace
  , bus, device, function
  , getB, getW, getL
  , setB, setW, setL
  ) where

import H.IOPorts
import Kernel.Bits

import Data.Word
import Data.Bits

newtype ConfigSpace = C Word16  

bus                :: ConfigSpace -> Word8
bus (C x)           = x .!. 1

device             :: ConfigSpace -> Word8 {- Word5 -}
device (C x)        = (x .!. 0) `shiftR` 3

function           :: ConfigSpace -> Word8 {- Word3 -}
function (C x)      = (x .!. 0) .&. 7

configSpace        :: Word8 -> Word8 -> Word8 -> ConfigSpace
configSpace x y z   = C (  fromIntegral x          `shiftL` 8
                       .|. fromIntegral (y `shiftL` 3)
                       .|. fromIntegral (z .&. 7)  
                        )

instance Show ConfigSpace where
  show x            = show (bus x) ++ ":"
                   ++ show (device x) ++ "."
                   ++ show (function x)


-- Access to the configuration space -------------------------------------------

getB               :: ConfigSpace -> Word8 -> H Word8
getB loc off        = do aim loc off
                         inB (0xCFC + byteOffset off)

getW               :: ConfigSpace -> Word8 -> H Word16
getW loc off        = do aim loc off
                         inW (0xCFC + wordOffset off)

getL               :: ConfigSpace -> Word8 -> H Word32
getL loc off        = do aim loc off
                         inL 0xCFC                         

setB               :: ConfigSpace -> Word8 -> Word8 -> H ()
setB loc off value  = do aim loc off
                         outB (0xCFC + byteOffset off) value
 
setW               :: ConfigSpace -> Word8 -> Word16 -> H ()
setW loc off value  = do aim loc off
                         outW (0xCFC + wordOffset off) value

setL               :: ConfigSpace -> Word8 -> Word32 -> H ()
setL loc off value  = do aim loc off
                         outL 0xCFC value


-- Private ---------------------------------------------------------------------


aim                :: ConfigSpace -> Word8 -> H ()
aim (C x) off       = outL 0xCF8 
                    (  1 `shiftL` 31                        -- enable config
                   .|. fromIntegral x `shiftL` 8            -- the space
                   .|. fromIntegral (off .&. complement 3)  -- dword align
                    )

byteOffset x        = fromIntegral (x .&. 3)
wordOffset x        = fromIntegral (x .&. 2)


