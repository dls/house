-- | PCI to PCI bridges
module Kernel.PCI.PCIBridge 
  ( PCIBridge, toPCIBridge
  , primBus, secBus, subBus
  ) where

import Kernel.PCI.ConfigSpace 
import Kernel.PCI.DevInfo    
import Kernel.PCI.Device
import Data.Word
import H.Monad(H)
import Kernel.Bits

data PCIBridge      = P { _primBus     :: Word8
                        , _secBus      :: Word8
                        , _subBus      :: Word8
                        }

toPCIBridge        :: Dev () -> H (Maybe (Dev PCIBridge))
toPCIBridge d        
  | not (isPCItoPCIbridge (devInfo d)) = return Nothing
  | otherwise       = do x <- config d `getL` 0x18
                         return $ Just 
                                $ d { devData = P
                                    { _primBus = x .!. 0
                                    , _secBus  = x .!. 1
                                    , _subBus  = x .!. 2
                                    }}

primBus            :: Dev PCIBridge -> Word8
primBus d           = _primBus (devData d)

secBus             :: Dev PCIBridge -> Word8
secBus d            = _secBus (devData d)

subBus             :: Dev PCIBridge -> Word8
subBus d            = _subBus (devData d)

                                    
                         
                         
                    




