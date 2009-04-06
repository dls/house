module EthernetOverTun(ethOverTun,Addr(..)) where

-- A virtual ethernet network to which many interfaces can be attached

--import Control.Concurrent
import Data.FiniteMap

import Tun
import Net.Ethernet as Eth
import qualified Net.Interface as Net
import Net.PacketParsing(doParse)
import Net.Packet(InPacket,OutPacket,loopback)
import Net.Utils(doReq)
import Monad.Util(loop)
import Net.IO

type Attach = Eth.Addr -> IO EthIface
type EthIface = Interface IO (Packet InPacket) (Packet OutPacket)

data Req = FromNetwork (Packet InPacket)
	 | FromLocal (Packet OutPacket)
         | Attach Eth.Addr (EthIface->IO ())

ethOverTun () =
  do net <- openTun tap
     reqChan <- newChan
     let tx = Net.tx net . Eth.unparse
	 rx = maybe bad return . doParse =<< Net.rx net
	   where
	     bad = putStrLn "Dropped bad ethernet packet" >> rx
	 attach = doReq reqChan . Attach
     fork $ loop $ writeChan reqChan . FromNetwork =<< rx
     fork $ server tx reqChan
     return attach

server tx reqChan = loop emptyFM
  where
    loop ifaces =
      do r <- readChan reqChan
	 case r of
	   Attach mac reply ->
             do rxChan <- newChan
		let rx = readChan rxChan
		    tx = writeChan reqChan . FromLocal
		reply (Eth.Interface mac (Net.Interface rx tx))
		loop (addToFM ifaces mac rxChan)
	   FromLocal packet ->
	     do let out = tx packet
		deliver ifaces out out (fmap loopback packet)
		loop ifaces
	   FromNetwork packet ->
	     do deliver ifaces (drop packet) ignore packet
		loop ifaces

    ignore = return ()
    drop Packet{packType=t,dest=dst} =
       putStrLn $ "Dropped "++show t++" packet to "++show dst

    deliver ifaces out bcast packet =
	     do let dst = dest packet
		    fwd = flip writeChan packet
		    t = packType packet
                if dst==broadcastAddr
	          then mapM_ fwd (eltsFM ifaces) >> bcast
		  else maybe out fwd (lookupFM ifaces dst)
