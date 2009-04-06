module Kernel.Driver.NE2000.Test where

import Kernel.Driver.NE2000.Driver as NE2000
import Net.Test as Net(initialize)
import H.Interrupts(IRQ(..))
import Net.H()

testnet putStrLn pci config =
  case pci of
    Nothing -> do eth <- NE2000.initialize putStrLn IRQ9 0x300
                  Just `fmap` start eth
    Just dev  -> do eth <- NE2000.initPCI putStrLn dev
                    case eth of
                      Nothing -> return Nothing
                      Just eth -> Just `fmap` start eth
  where
  start eth = Net.initialize putStrLn config eth

