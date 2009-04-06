module Kernel.PCI.Device 
  (Dev(..), showDev, module Kernel.PCI.DevInfo
  ) where

import Kernel.PCI.ConfigSpace
import Kernel.PCI.DevInfo

data Dev t          = Dev 
                    { config  :: ConfigSpace
                    , devInfo :: DevInfo
                    , devData :: t
                    }

instance Show (Dev t) where
  show d            = show (config d) ++ " " ++ show (devInfo d)

showDev t d         = show (config d) ++ ": " ++ showDevInfo t (devInfo d)


