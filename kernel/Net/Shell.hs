module Net.Shell where
import qualified Net.IPv4 as IP
import qualified Net.UDP_Client as UDP
import Net.TFTP_Client(tftpGet,tftpPut)
import Net.Servers(telnetServer,telnet)
import qualified Net.PortNumber as Port
import qualified Net.Interface as Net
import Net.ClientInterface
import Net.Concurrent
import Net.PacketParsing(doUnparse)

import Util.CmdLineParser

import Data.Maybe(fromMaybe)
import Data.Array.IArray(listArray,elems)
import Monad.Util

netcmd' netstate fail cmd = maybe nonet (cmd.snd) =<< readMVar netstate
  where nonet = fail "Start the network with the command 'net' first."

netcommands execute2 runBytes findNetDevice netstate user =
    oneof
    [cmd "net" net <@ netConfig -: "Show setting or initialize network",
     cmd "ping" ping <@ ipAddr,
     cmd "udping" udping <@ ipAddr <@ opt (arg "<message>"),
     let tftpcmd =
	   oneof [cmd "show" tftpshow <@ ipAddr <@ path,
		  cmd "run" tftprun <@ ipAddr <@ path,
--		  cmd "gif" tftpgif <@ ipAddr <@ path,
		  cmd "put" tftpput <@ ipAddr <@ path <@ arg "<contents>"
		 ]
     in cmd "tftp" id <@ tftpcmd,
     cmd "telnet" myTelnet <@ ipAddr <@ opt port,
     cmd "arp" arp -: "Show arp cache"]
  where
    ipAddr = readP "<IPaddr>"
    port = Port.Port #@ readP "<port>"

    netConfig =
      oneof [cmd "dhcp" DHCP,
	     config #@ opt ((,) #@ ipAddr <@ opt ((,) #@ ipAddr <@ opt ipAddr))]
    config Nothing = fixed defaultIP defaultRouterIP
    config (Just (myIP,Nothing)) = fixed myIP defaultRouterIP
    config (Just (myIP,Just (routerIP,Nothing))) = fixed myIP routerIP
    config (Just (myIP,Just (routerIP,Just netmask))) =
      Fixed myIP routerIP netmask

    defaultIP       = IP.Addr 172 20 0 2
    defaultRouterIP = IP.Addr 172 20 0 1


    net config =
        do net <- maybe start (return.Just) =<< takeMVar netstate
	   putMVar netstate net
	   maybe done showNetwork net
      where
        showNetwork (config,_) =
            putStrLn $ "The network is running: "++show config

        start = maybe nonet net' =<< findNetDevice
	  where
	    nonet = do putStrLn "Found no supported PCI ethernet device"
		       return Nothing

	    net' (testnet,dev) =
		do net <- testnet putStrLn (Just dev) config
		   case net of
		     Nothing -> putStrLn "Could not start the network"
		     Just net -> do fork $ myTelnetServer (snd net)
				    done
		   return net

    myTelnetServer = telnetServer nodebug "House> " execute
      where
        nodebug _ = done
        execute iface s = execute2 iface (words s)

    netcmd = netcmd' netstate putStrLn

    ping dstIP = netcmd $ \ Net{ping=ping} -> ping dstIP 55555 1

    udping ip optmsg =
       netcmd $ \ Net{udp=udp} ->
       do (p,iface) <-  UDP.listenAny udp
	  let msg = fromMaybe "Hello!" optmsg
          Net.txT iface (ip,UDP.template p Port.echo (doUnparse msg))
	  reply <- Net.rxT iface (Just 1000000)
	  print reply
	  UDP.unlisten udp p

    arp = netcmd $ \ Net{dump=dump} -> print =<< dump

    tftpshow = tftpget putStrLn

    tftprun = tftpget runBytes

--  tftpgif = tftpget (showGifInfo . bytes)
--    where bytes bs = listArray (0,length bs-1) bs

    tftpget action serverIP filename =
      netcmd $ \ Net {udp=udp} ->
	either print act =<< tftpGet putStrLn udp serverIP filename "octet"
      where act = action . map (toEnum.fromEnum) . concatMap elems

    tftpput serverIP path contents =
      netcmd $ \ Net {udp=udp} ->
      print =<< tftpPut putStrLn udp serverIP path "octet" contents

    myTelnet ip optPort = netcmd $ \ net -> telnet user net (ip,port)
      where port = fromMaybe Port.telnet optPort

    putStr = Net.tx user
    putStrLn s = putStr s >> putStr "\n"
    print x = putStrLn (show x)
