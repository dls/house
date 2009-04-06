import Control.Concurrent
import System(getArgs)
import IO(hFlush,stdout)
import Monad(unless)
import Char(isPrint,isAscii)
import Time(getClockTime)
import Maybe(fromJust)

import Net.Test as Test
import qualified Net.PortNumber as Port
import qualified Net.IPv4 as IP
import Monad.Util
import Net.Packet(loopbackout)
import Net.PacketParsing(doUnparse,doParse)
import Net.Interface as Net
import Net.UDP_Client as UDP(Interface(..),Packet(..),Port(..))
import Net.TCP_Client as TCP(Interface(..),Active(..))
import Net.TFTP_Client as TFTP
import Net.Servers
import EthernetOverTun as Eth
import Data.Array.IArray(elems)

mac n = Eth.Addr 0x52 0x54 0x00 0x12 0x34 (0x56+n)

main = tst =<< parse =<< getArgs
  where
    parse [] = return defaultIP 
    parse [arg] = givenIP `fmap` readIO arg
    parse _ = fail "Usage: tst [<router ip>]"

    defaultIP n = IP.Addr 172 20 0 (n+1)
    givenIP (IP.Addr a b c d) n = IP.Addr a b c (d+n)

tst netIP =
  do attach <- ethOverTun()
     let host n =
	     do net <- Test.initialize debug conf =<< attach (mac n)
		forkIO $ myTelnetServer net
		return net
	   where
	     conf = fixed (netIP n) (netIP 0)
             debug msg = putStrLn (show n++": "++msg)
     forkIO $ do host 2 ; return () -- a rather passive host
     shell =<< host 1 -- a host with a shell

shell net =
    loop $ do putStr "1> "; hFlush stdout
              execute net user =<< getLine

user = Net.Interface { rx=getLine, tx=putStr }

execute (conf,net@Net{ping=ping,dump=arp,udp=udp,tcp=tcp}) user cmd =
  case words cmd of
    [] -> return ()
    "echo":args -> mapM_ putChar (unwords args) >> putStrLn ""
    ["date"] -> print =<< getClockTime
    ["ifconfig"] -> print conf
    ["arp"] -> putStr . unlines . map show =<< arp
    ["ping",arg] -> ping (read arg) 55555 1
    ["udping",arg] -> udping (read arg) "Hello!"
    ["tftp","get",server,filename] -> tftpget server filename
    ["telnet",host,port] -> telnet user net (read host,Port.Port (read port))
    ["telnet",host]      -> telnet user net (read host,Port.telnet)
    _ -> putStrLn (cmd++"??")
  where
    putStr = tx user
    getLine = rx user
    putStrLn s = putStr s >> putStr "\n"
    putChar c = putStr [c]
    print x = putStrLn (show x)

    udping ip msg =
       do (p,iface) <- listenAny udp
	  Net.txT iface (ip,Packet{sourcePort=p,destPort=Port.echo,
				   len=8+fromIntegral (length msg),
				   checksum=0,
				   content=doUnparse msg})
	  reply <- Net.rxT iface (Just 1000000)
	  print reply
	  unlisten udp p

    tftpget server filename =
	either print decode =<<
	  tftpGet putStrLn udp (read server) filename "octet"
      where
        decode = putStrLn . map (toEnum.fromEnum) . concatMap elems

myTelnetServer env@(_,net) = telnetServer nodebug "> " (execute env) net

nodebug _ = return ()
