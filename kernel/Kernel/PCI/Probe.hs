module Kernel.PCI.Probe 
  ( probe
  ) where

import H.Monad(H)
import Kernel.PCI.ConfigSpace
import Kernel.PCI.DevInfo 
import Kernel.PCI.Device 
import Kernel.PCI.DevTree
import Kernel.PCI.PCIBridge

import Data.Maybe(catMaybes)
import Data.Word


probe               = probeBus 0

-- Private ---------------------------------------------------------------------

probeBus           :: Word8 -> H DevTree
probeBus bus        = do devs <- concat `fmap` forEach [0..31] (probeDev bus)
                         return (DevTree bus devs [])
                         loop devs (DevTree bus [] [])
  where
  loop [] t         = return t
  loop (d:ds) t     = loop ds =<< tryDev d t

  tryDev d t        = do mb <- toPCIBridge d
                         case mb of
                           Nothing -> return (t { devices = d : devices t })
                           Just b  -> 
                             do t' <- probeBus (secBus b)
                                return (t { bridges = (b,t') : bridges t})


probeDev           :: Word8 -> Word8 -> H [Dev ()]
probeDev bus dev    = do x <- probeConfig (configSpace bus dev 0)
                         case x of
                           Nothing -> return []
                           Just d 
                             | isMultiFun (devInfo d) -> 
                                 ((d:) . catMaybes) `fmap`
                                 forEach [1..7] 
                                   (probeConfig . configSpace bus dev)
                             | otherwise  -> return [d] 

probeConfig        :: ConfigSpace -> H (Maybe (Dev ()))
probeConfig c       = pick `fmap` getDevInfo c
  where 
  pick d            
    | isPresent d   = Just (Dev { config  = c
                                , devInfo = d
                                , devData = ()
                                })
    | otherwise     = Nothing
                      


forEach xs f        = mapM f xs

