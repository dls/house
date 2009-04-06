module Net.EthernetClient where

import Net.Concurrent(fork)
import Net.Ethernet as Eth(Interface(..),Packet,PacketType(..),rx,tx,packType,content)
import qualified Net.IPv4 as IPv4(Packet)
import qualified Net.ARP as ARP(Packet)
import Net.PacketParsing
import Net.Wire
import qualified Net.Interface as Net
import Monad.Util(loop)

type Client_ m p = Eth.Interface m p (Packet p)
type Client m p = Eth.Interface m (p InPacket) (Packet (p OutPacket))

data Clients m
    = Clients {
--        ethernet,
--        unknown   :: Client m Packet,
        ipv4      :: Client m IPv4.Packet,
--      ipv6      :: Client m IPv6.Packet,
	arp       :: Client_ m ARP.Packet
      }

initialize debug eth =
  do Net.Interface{Net.rx=rxIPv4,Net.tx=toIPv4} <- newWire()
     Net.Interface{Net.rx=rxARP,Net.tx=toARP}   <- newWire()
     let demultiplex =
	   do p <- rx eth
	      let t = packType p
		  conv dest = maybe warning dest (doParse (content p))
		  warning = debug (show t++" packet parser failed "++show p)
	      --debug (show t++" received")
	      case t of
		--Ethernet n -> to' ethernet
		IPv4       -> conv toIPv4
		--IPv6       -> return () -- not implemented yet
		ARP        -> conv toARP
		--Unknown n  -> to' unknown
		_ -> debug $ show t++" packet dropped"
         --client :: IO (...) -> Net.IOInterface ... ...
	 client rx = eth {io=io}
           where
	     io = Net.Interface {Net.rx=rx,
				 Net.tx=tx eth . fmap doUnparse}
     fork $ loop demultiplex
     return Clients {ipv4 =client rxIPv4, arp=client rxARP}
     --return (client rxIPv4,client rxARP)
