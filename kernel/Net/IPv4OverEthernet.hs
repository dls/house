module Net.IPv4OverEthernet where
--import Control.Concurrent

import qualified Net.Ethernet as Eth
import Net.Interface
--import Net.PacketParsing(doParse,doUnparse)

initialize lookupMAC eth = Interface (Eth.rx eth) tx
  where
    tx (nextIP,ip) = maybe dropit sendit =<< lookupMAC nextIP
      where
	dropit = return ()
        sendit nextMAC = Eth.tx eth p
          where p = Eth.Packet nextMAC (Eth.myMAC eth) Eth.IPv4 ip
