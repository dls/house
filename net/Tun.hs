module Tun(openTun,tun,tap) where

-- A virtual Ethernet network card that talks to the host system through
-- the /dev/net/tun virtual network

import Control.Concurrent
import Control.Monad

import System.Posix.Types(Fd(..))
import System.Posix.IO
--import System.Posix.Signals

import qualified Net.Interface as Net
import Net.Packet(InPacket,toInPack,OutPacket,outBytes)
import Data.Array.Unboxed

openTun :: TunFlags -> IO (Net.Interface IO InPacket OutPacket)
openTun mode =
  do fd@(Fd fdint) <- openFd "/dev/net/tun" ReadWrite Nothing defaultFileFlags
     when (fd<0) $ fail "openTun failed"
     rxchan <- newChan
{-   -- Solution using with a signal handler:
     let readPacket = do (s,n) <- fdRead fd 2048
			 when (n>0) $ do writeChan rxchan (s,n)
					 readPacket
     installHandler pollableEvent (Catch readPacket) Nothing
     configTun fd mode True
-}
     -- Solution using threadWaitRead to prevent read from blocking all threads:
     configTun fd mode False
     let readPacket = do threadWaitRead (fromIntegral fdint)
		         (s,n) <- fdRead fd 2048
	                 when (n>0) $ writeChan rxchan (s,n)
	                 readPacket
     forkIO readPacket
     let tx p = do fdWrite fd . outPacketToString $ p
		   return ()
     let rx = do (s,bc) <- readChan rxchan
		 let n = fromIntegral bc::Int
	         return (stringToInPacket $ (s,n))
     return (Net.Interface rx tx)

--------------------------------------------------------------------------------
outPacketToString :: OutPacket -> String
outPacketToString = map (toEnum.fromEnum) . outBytes

stringToInPacket :: (String,Int) -> InPacket
stringToInPacket (s,n) = toInPack (listArray(0,n-1) (map (toEnum.fromEnum) s))

--------------------------------------------------------------------------------

newtype TunFlags = Flags Int
tun = Flags 1
tap = Flags 2

foreign import ccall "opentun.h config_tun" configTun :: Fd -> TunFlags -> Bool -> IO ()
