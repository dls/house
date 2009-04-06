module Net.IPv4Client where

import Net.Concurrent

import Net.IPv4
--import qualified Net.Ethernet as Eth
import qualified Net.UDP  as UDP(Packet)
import qualified Net.TCP  as TCP(Packet)
import qualified Net.ICMP as ICMP(Packet)
--import Net.Packet(loopback)
import Net.PacketParsing(InPacket,OutPacket,doParse,doUnparse)
import Net.Interface as Net
import Net.Wire
import Net.Utils(emap)
import Monad.Util(loop)

type Client m p = Client' m (p InPacket) (p OutPacket)
type Client_ m p =  Client' m p p
type Client' m i o = Net.Interface m (Packet i) (Packet o)

data Clients m
    = Clients {
        icmp     :: Client_ m ICMP.Packet,
        udp      :: Client m UDP.Packet,
        tcp      :: Client m TCP.Packet{-,
        unknown  :: Client' m InPacket OutPacket-}
      }

initialize debug link =
  do Interface{rx=rxICMP,tx=toICMP} <- newWire()
     Interface{rx=rxUDP, tx=toUDP}  <- newWire()
     Interface{rx=rxTCP, tx=toTCP}  <- newWire()
     let rx = do ip <- Net.rx link
                 let t = protocol ip
		     conv to = maybe warning to (emap doParse ip)
		     warning = debug (show t++" packet parser failed")
		 --debug (show t++" received")
		 case t of
		   ICMP       -> conv toICMP
		   UDP        -> conv toUDP
		   TCP        -> conv toTCP
--		   Unknown n  -> to unknown
		   _ -> debug (show t++" packet dropped")
     fork $ loop rx
     let client rx = Interface rx (Net.tx link . fmap doUnparse)
     return Clients { icmp = client rxICMP,
		      udp = client rxUDP,
		      tcp = client rxTCP }
--     return (client rxICMP)
