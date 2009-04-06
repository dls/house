module Net.DHCP_Client where

import Control.Monad(unless)
import Data.Maybe(fromMaybe,listToMaybe)
import Data.Bits(xor)
import Monad.Util(whileM)

import Net.DHCP
import qualified Net.IPv4 as IP
import qualified Net.Ethernet as Eth
import qualified Net.UDP as UDP
import Net.PacketParsing(doParse,doUnparse)
import Net.Concurrent(fork,delay,newRef,readRef,writeRef)
import Net.Utils({-emap,-}contents)
--import System.Random(randomIO) -- Missing in hOp
--import Kernel.Timer(readTimer)
--import H.Monad(runH)

init putStrLn eth =
  do --xid <- fmap (xor 0x7f23ae64 . fromIntegral) ({-runH-} readTimer)
     let xid = 0x7f23ae64
        -- xid should be chosen randomly!!!
     let d = dhcpDiscover xid
     (offer,serverMAC) <- let req = do debug $ "Discover " -- ++show d
				       tx d
                          in solicit req (rx (isOffer xid))
     debug $ "Offer " -- ++show offer
     let myIP = yiaddr offer
	 Options os = options offer
	 serverIP = head [sIP|ServerIdentifier sIP<-os]
         request = dhcpRequest xid serverIP serverMAC myIP
     (ack,_) <- let req = do debug $ "Request " -- ++show request
			     tx request
		in solicit req (rx (isAck xid serverMAC))
     debug $ "Ack " -- ++show ack
     let ip = yiaddr ack
	 Options os = options ack
         router = listToMaybe [r|Routers rs<-os,r<-rs]
	 dm = IP.defaultNetmask ip
	 netmask = fromMaybe dm $ listToMaybe [m|SubnetMask m<-os]
	 net = (ip,router,netmask)
     --debug $ show net
     return net
  where
    debug = putStrLn . ("DHCP init: "++)

    mac = Eth.myMAC eth

    tx p = Eth.tx eth (fmap doUnparse p)

    rx expected =
        do ep <- Eth.rx eth
	   if Eth.packType ep/=Eth.IPv4
	      then again "" --"Eth type IPv4"
	      else try "IP" ep $ \ ip ->
		   if IP.protocol ip/=IP.UDP
		   then again "protocol UDP"
		   else try "UDP" ip $ \ udp ->
		        if UDP.sourcePort udp/=serverPort ||
			   UDP.destPort udp/=clientPort
			then again "DHCP ports"
			else try "DHCP" udp $ \ dhcp ->
			     cont dhcp (Eth.source ep)
      where try msg = flip (maybe (again msg)) . doParse . contents
            again msg = do unless (null msg) $ debug $ "not "++msg
			   rx expected
            cont p sMAC =
		if expected sMAC p
		then return (p,sMAC)
		else do debug "unexpected DHCP packet"
			rx expected

    isAck uid sMac sMac' p =
        opcode p==BootReply && ack && sMac'==sMac && xid p == uid
      where
	Options os = options p
	ack = not $ null [()|MessageType Ack<-os]

    isOffer uid _ p = opcode p==BootReply && offer && xid p == uid
      where
	Options os = options p
	offer = not $ null [()|MessageType Offer<-os]

    --c3 = contents . contents . contents
    --c3 = id

    dhcpDiscover uid = bcastIP (dhcpUDP discover)
      where
        discover = (template mac){xid=uid,
				  options=Options [MessageType Discover]}

    dhcpRequest uid sIP sMAC myIP =
        ucastIP myIP sIP sMAC (dhcpUDP request)
      where
        request = (template mac){xid=uid,
				 options=Options [MessageType Request,
						  ServerIdentifier sIP,
						  RequestedIPAddress myIP]}

    dhcpUDP p =  UDP.template clientPort serverPort p

    bcastIP p = bcastEth (IP.template IP.UDP z bcast p)
      where
        z     = IP.Addr 0 0 0 0
	bcast = IP.Addr 255 255 255 255

    bcastEth p = Eth.Packet Eth.broadcastAddr mac Eth.IPv4 p

    ucastIP srcIP dstIP dstMAC p =
      ucastEth dstMAC (IP.template IP.UDP srcIP dstIP p)

    ucastEth dst p = Eth.Packet dst mac Eth.IPv4 p

-- Nice enough to move to Net.Utils?
solicit req = solicit' 3000000 req -- microseconds

solicit' timeout request response =
  do waiting <- newRef True
     fork $ whileM (readRef waiting) $
	    do request
	       delay timeout
     r <- response
     writeRef waiting False
     return r
