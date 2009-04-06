module Net.ARP_Protocol(
  Interface(..),CacheDump,initialize)
  where

-- Implementation of the ARP Protocol and an ARP cache
-- See http://rfc.net/rfc0826.html

import Net.Concurrent
import Control.Monad
import qualified Net.Ethernet as Eth
import qualified Net.IPv4 as IP
import Net.ARP as ARP
--import Net.PacketParsing
import Text.Show.Functions
import Net.Utils(doReq)
import Monad.Util(loop)

data Interface m
    = Interface {
      lookup :: IP.Addr -> m (Maybe Eth.Addr),
      dump :: m CacheDump
    }

type Callback m = ARP.Packet -> m ()

--------------------------------------------------------------------------------
type Cache m = [(IP.Addr,CacheEntry m)]

data CacheEntry m = Waiting [Maybe Eth.Addr->m ()]
                  | Known Eth.Addr

initCache = [] :: Cache m

findIP ip cache =
    case break ((==ip).fst) cache of
      (other1,(_,entry):other2) -> Just (entry,other1++other2)
      _ -> Nothing

type CacheDump = [(IP.Addr,Maybe Eth.Addr)]

dumpCache cache = [(ip,dumpEntry e)|(ip,e)<-cache]
  where
    dumpEntry (Known mac) = Just mac
    dumpEntry _ = Nothing

--------------------------------------------------------------------------------

data Req m
   = Lookup IP.Addr (Maybe Eth.Addr->m ())
   | Dump (CacheDump-> m ())
   | FromNetwork ARP.Packet
   | Timeout Int IP.Addr
   deriving (Show)

--initialize :: (String->IO())->Eth.Interface->IP.Addr-> IO (Interface,Callback)
initialize debug eth myIP =
  do reqChan <- newChan
     let ask ipaddr = if ipaddr==bcastIP
		      then return (Just Eth.broadcastAddr)
		      else doReq reqChan (Lookup ipaddr)
	 dump = doReq reqChan Dump
	 tx = Eth.tx eth
	 me = (Eth.myMAC eth,myIP)
         iface = Interface ask dump
     fork $ loop $ writeChan reqChan . FromNetwork =<< Eth.rx eth
     fork (server debug tx me reqChan initCache)
     return iface
  where
    bcastIP = IP.broadcastAddr myIP


server debug tx me@(_, myIP) reqChan = loop
  where
    loop cache =
      do msg <- readChan reqChan
	 --debug (show msg)
	 case msg of
	   Dump reply -> reply (dumpCache cache) >> loop cache
	   Lookup ipaddr reply ->
	     case findIP ipaddr cache of
	       Just (Known ha,_) -> do reply (Just ha); loop cache
	       Just (Waiting clients,other) ->
		   loop ((ipaddr,Waiting (reply:clients)):other)
	       _ ->
		 do txreq 3 ipaddr
		    loop ((ipaddr,Waiting [reply]):cache)
	   FromNetwork p ->
	     do cache' <- update_cache cache p
		when (targetIP p == myIP && opcode p==Request) $
		  tx (arpReply me (senderHA p) (senderIP p))
		loop cache'
	   Timeout 0 ipaddr ->
             do debug $ "ARP request for "++show ipaddr++" timed out"
                loop =<< update_timeout cache ipaddr
	   Timeout n ipaddr ->
	     case findIP ipaddr cache of
	       Just (Waiting _,_) -> txreq (n-1) ipaddr >> loop cache
	       _  -> loop cache

    txreq retries ipaddr =
      do tx (arpRequest me ipaddr)
         fork $ do delay 500000
		   writeChan reqChan (Timeout retries ipaddr)

    update_timeout cache ipaddr =
	case findIP ipaddr cache of
          Just (entry,other) ->
            case entry of
	      Known oldHA -> return cache -- timeout after valid reply
	      Waiting clients ->
                do mapM_ ($ Nothing) clients
		   return other -- request really timed out

    update_cache cache Packet{senderIP=sIP,senderHA=sHA,targetIP=tIP} =
	case findIP sIP cache of
          Just (entry,other) ->
            case entry of
	      Known oldHA -> return $ if sHA==oldHA
				      then cache
				      else entry':other
	      Waiting clients ->
                do mapM_ ($ (Just sHA)) clients
		   return $ entry':other
          _ -> return $ if tIP==myIP
		         then entry':cache
			 else cache
       where
         entry' = (sIP,Known sHA)

arpRequest me = arpMessage Request me Eth.broadcastAddr
arpReply = arpMessage Reply

arpMessage op (myMAC, myIP) targetMAC targetIP =
    Eth.Packet { Eth.dest = targetMAC,
		 Eth.source = myMAC,
		 Eth.packType = Eth.ARP,
		 Eth.content = arpPacket }
  where
    arpPacket = Packet op myMAC myIP targetMAC targetIP
