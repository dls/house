module Kernel.PCI.DevInfo 
  ( DevInfo, getDevInfo
  , vendorId, deviceId, revision, rawClass, headerType
  , isPresent, isMultiFun, isPCItoPCIbridge
  , showDevInfo
  ) where

import H.Monad(H)
import Kernel.PCI.ConfigSpace
import Kernel.PCI.ParseVendors(ClassTable, VendorTable)
import Kernel.Bits(showHex)

import Data.PackedString
import Data.Bits
import Data.Word

data DevInfo        = D { dev_ven   :: !Word32
                        , class_rev :: !Word32
                        , header    :: !Word8
                        }

getDevInfo         :: ConfigSpace -> H DevInfo
getDevInfo c        = do dev_ven    <- c `getL` 0x00
                         class_rev  <- c `getL` 0x08
                         hdr        <- c `getB` 0x0E
                         return $ D
                                { dev_ven   = dev_ven 
                                , class_rev = class_rev 
                                , header    = hdr
                                }



vendorId           :: DevInfo -> Word16
vendorId d          = fromIntegral (dev_ven d .&. 0xFFFF)

deviceId           :: DevInfo -> Word16
deviceId d          = fromIntegral (dev_ven d `shiftR` 16)

revision           :: DevInfo -> Word8
revision d          = fromIntegral (class_rev d .&. 0xFF)

rawClass           :: DevInfo -> Word32 -- 24
rawClass d          = class_rev d `shiftR` 8

headerType         :: DevInfo -> Word8
headerType d        = header d `clearBit` 7

isPresent          :: DevInfo -> Bool
isPresent d         = not (vendorId d == 0 || vendorId d == -1)

isMultiFun         :: DevInfo -> Bool
isMultiFun d        = header d `testBit` 7

isPCItoPCIbridge   :: DevInfo -> Bool
isPCItoPCIbridge d  = headerType d == 1


instance Show DevInfo where
  show d            = showHex (vendorId d) ++ " "
                   ++ showHex (deviceId d) ++ " "
                   ++ showRev d
  

showDevInfo        :: (VendorTable,ClassTable) -> DevInfo -> String
showDevInfo (v,_) d = case v (vendorId d) of
                        Nothing -> show d
                        Just (t,devs) -> unpackPS t ++ " " ++
                          case devs (deviceId d) of
                            Nothing -> showHex (deviceId d) ++ " " ++ showRev d
                            Just (t,_) -> unpackPS t ++ " " ++ showRev d
  where


showRev d         = "(rev " ++ show (revision d) ++ ")"







