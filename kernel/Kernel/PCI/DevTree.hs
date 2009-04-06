module Kernel.PCI.DevTree 
  ( DevTree(..)
  , drawTree
  , findDev
  ) where

import H.Monad(H)
import Kernel.PCI.Device
import Kernel.PCI.PCIBridge(PCIBridge)
import Kernel.PCI.ParseVendors(VendorTable,ClassTable)

import Data.Word
import Data.List(find)
import Control.Monad(msum,mplus)

data DevTree        = DevTree 
                        { busNumber :: Word8
                        , devices   :: [Dev ()]
                        , bridges   :: [(Dev PCIBridge, DevTree)]
                        }

drawTree :: (String -> H ()) -> (VendorTable,ClassTable) -> DevTree -> H ()
drawTree pr tbl t   = treeLines 0 t
  where
  treeLines o t     = do mapM_ (pr . showDev tbl) (devices t) 
                         mapM_ (bridgeLines o) (bridges t)

  bridgeLines o (d,t) = do pr (tab o (showDev tbl d))
                           treeLines (o+2) t

tab o xs            = replicate o ' ' ++ xs


-- Does not consider bridges
findDev            :: Word16 -> Word16 -> DevTree -> Maybe (Dev ())
findDev ven dev t   = find isMe (devices t) 
              `mplus` msum (map (findDev ven dev . snd) (bridges t))
  where
  isMe   :: Dev t -> Bool
  isMe d  = vendorId (devInfo d) == ven && deviceId (devInfo d) == dev

