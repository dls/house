module Net.UDP_Client(
    initialize,UDP_API,Interface(..),Packet(..),template,Port(..)
  ) where

import Net.Concurrent
import Control.Monad.State
import Control.Monad.Trans(lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List((\\))
--import System.Random(randomRIO)

import Net.UDP
import qualified Net.IPv4 as IPv4
import qualified Net.Interface as Net
import Net.Utils(doReq)
import Net.Packet(InPacket,OutPacket)
import Net.Wire
import Monad.Util

type UDP_API m =
  Net.TimedInterface m (IPv4.Addr,Packet InPacket) (IPv4.Addr,Packet OutPacket)

data Interface m
   = Interface {
       listen :: Port -> m (UDP_API m),
       listenAny :: m (Port,UDP_API m),
       unlisten :: Port -> m ()
   }

data Req m
  = Listen Port (UDP_API m->m ())
  | ListenAny ((Port,UDP_API m)->m ())
  | Unlisten Port
  | FromNetwork (IPv4.Packet (Packet InPacket))

type Clients m = Map Port ((IPv4.Addr,Packet InPacket)->m ())

initialize putStrLn myIP iface =
  do reqChan <- newChan
     fork $ loop $ writeChan reqChan . FromNetwork =<< Net.rx iface
     fork $ server debug myIP iface reqChan
     let listen = doReq reqChan . Listen
	 listenAny = doReq reqChan ListenAny
	 unlisten = writeChan reqChan . Unlisten
     return $ Interface listen listenAny unlisten
  where
    debug = putStrLn . ("UDP: "++)

server debug myIP iface reqChan =
    flip evalStateT init $ loop (handle=<<readChan reqChan)
  where
    init = Map.empty

    handle req =
      case req of
	Listen port reply -> listen port reply
	ListenAny reply -> do port <- pickPort
			      let reply' iface = reply (port,iface)
			      listen port reply'
	Unlisten port -> modify (Map.delete port)
	FromNetwork ipPack ->
	   do let udpPacket = IPv4.content ipPack
		  src = IPv4.source ipPack
		  dst = IPv4.dest ipPack
		  sp = sourcePort udpPacket
		  port = destPort udpPacket
	      clients <- get
	      case Map.lookup port clients of
		Just toClient -> lift $ toClient (src,udpPacket)
		_ -> lift $ debug $ "Dropped packet from "++show (src,sp)
			            ++" to "++show (IPv4.dest ipPack,port)
      where
        listen port reply =
	  do Net.TimedInterface{Net.rxT=rx,Net.txT=toClient} <- timedWire()
	     lift $ reply (Net.TimedInterface rx tx)
	     modify (\clients -> Map.insert port toClient clients)
	pickPort = do inuse <- gets Map.keys
		      return $ head (map Port [32768..65535]\\inuse)

    tx (destIP,updPacket) =
      Net.tx iface (IPv4.template IPv4.UDP myIP destIP updPacket)
