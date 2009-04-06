module Net.Test(module Net.Test,module Net.ClientInterface) where

--import qualified Net.Ethernet as Eth
import qualified Net.IPv4 as IP
import qualified Net.ICMP as ICMP
import qualified Net.ARP_Protocol as ARP
import qualified Net.EthernetClient as EC
import qualified Net.IPv4Client as IC
import Net.IPv4OverEthernet as IPv4oE
import Net.IPv4Link as IPv4Link
import qualified Net.UDP_Client as UC
import qualified Net.TCP_Client as TC
--import Net.Utils
import Net.Packet(listArray)
--import Net.PacketParsing(doUnparse)
import qualified Net.Interface as Net
import Monad.Util(loop)
import qualified Net.DHCP_Client as DHCP(init)
import Net.ClientInterface
import Net.Servers

import Net.Concurrent

initialize putStrLn config eth =
    do iconfig@(myIP,routerIP,netmask) <-
	   case config of
	     DHCP -> DHCP.init putStrLn eth
	     Fixed me router netmask -> return (me,Just router,netmask)
       EC.Clients {EC.ipv4=ipv4Eth,EC.arp=arpEth} <- EC.initialize putStrLn eth
       arp <- ARP.initialize putStrLn arpEth myIP
       --iplink <- link ipv4Eth (ARP.lookup arp)
       ipv4clients <-
	  let ipoe = IPv4oE.initialize (ARP.lookup arp) ipv4Eth
              iplink = IPv4Link.initialize (myIP,netmask) routerIP ipoe
          in IC.initialize putStrLn iplink
       let i = IC.icmp ipv4clients
       icmpHandler putStrLn myIP i
       udpclient <- UC.initialize putStrLn myIP (IC.udp ipv4clients)
       tcpclient <- TC.initialize putStrLn myIP (IC.tcp ipv4clients)
       let net = Net { ping = sendPing (Net.tx i) myIP,
		       dump  = ARP.dump arp,
		       udp  = udpclient,
		       tcp  = tcpclient }
       fork $ udpEchoServer putStrLn net 
       fork $ tcpEchoServer putStrLn net
       return (iconfig,net)

{-
ignore putStrLn iface =
    fork $ loop $ callback =<< Net.rx iface
  where
    callback ipPack =
      putStrLn $ "Ignored a packet: "++show ipPack
-}
icmpHandler putStrLn myIP iface =
    fork $ loop $ icmpCallback =<< Net.rx iface
  where
    txIP = Net.tx iface
    icmpCallback ipPack =
      let icmpPack = IP.content ipPack in 
      case icmpPack of
	ICMP.EchoRequest echoMsg ->
            do putStrLn $ "Replying to ping from "++show srcIP
	       txIP ipRep
	  where
	    icmpRep = ICMP.EchoReply echoMsg
	    srcIP   = IP.source ipPack
	    ipRep   = ipPack
			{ IP.source  = myIP
			, IP.dest    = srcIP
			, IP.content = icmpRep
			}
	_  -> -- forward ping reply to ping client!
	      putStrLn (show (IP.source ipPack,icmpPack))

sendPing txIP myIP dstIP unique n = txIP p
  where
    p = IP.template IP.ICMP myIP dstIP icmpReq
    icmpReq = ICMP.EchoRequest
	      ICMP.Echo { ICMP.ident    = unique
                        , ICMP.seqNum   = n
                        , ICMP.echoData = listArray (0,5) [0..5]
                        }
