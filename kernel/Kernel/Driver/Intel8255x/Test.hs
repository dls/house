module Kernel.Driver.Intel8255x.Test(testnet) where
import Net.Test as Net(initialize)
import Kernel.Driver.Intel8255x.Driver
import Net.H()

testnet debug Nothing _ = return Nothing
testnet debug (Just dev) config = initNet =<< initPCI debug dev
  where
    initNet Nothing = return Nothing
    initNet (Just eth) = Just `fmap` start eth

    start eth = Net.initialize debug config eth
       
