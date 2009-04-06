module Net.Servers where
import Control.Monad(unless)
import Data.Char(isPrint,isAscii)
import Data.Maybe(fromJust)

import Net.Concurrent
import Net.ClientInterface
import Net.Interface as Net
import qualified Net.TCP_Client as TCP
import qualified Net.UDP_Client as UDP
import qualified Net.PortNumber as Port
import Monad.Util
import Net.Packet(outLen,loopbackout)
import Net.PacketParsing(doParse,doUnparse)

tcpEchoServer debug net = server =<< TCP.listen (tcp net) Port.echo
  where
    server socket = loop $ fork . echo =<< TCP.accept socket

    echo (peer,con) =
      do debug $ "echo server accepted a connection from "++show peer
	 loop
	 debug $ "echo server closing connection to "++show peer
	 TCP.close con
      where
        loop = do p <- loopbackout # TCP.rx con
		  let n=outLen p
	          unless (n==0) $
		    do debug $ "echo server echoing "++show n++" bytes"
		       TCP.tx con p
		       loop

udpEchoServer debug net = server =<< UDP.listen (udp net) Port.echo
  where
    server iface = loop $ echo =<< rxT iface Nothing
      where
	echo (Just (srcIP,udpP)) =
	    do debug $ "replying to UDP echo request from "++show srcIP
	       txT iface (srcIP,loopbackout # reply)
	  where
	    reply = udpP{UDP.sourcePort=Port.echo,
			 UDP.destPort=UDP.sourcePort udpP}

simpleTCPServer debug net port simpleServer =
    listener =<< TCP.listen (tcp net) port
  where
    listener socket = loop $ fork . clientHandler =<< TCP.accept socket

    clientHandler (peer,con) =
      do debug $ "server on "++show port
		 ++" accepted a connection from "++show peer
	 simpleServer Interface { rx=fmap doParse $ TCP.rx con,
				  tx=TCP.tx con.doUnparse}
	 debug $ "server on "++show port++" closing connection to "++show peer
	 TCP.close con

{- --old:
lineBuffered debug iface =
  do inCh <- newChan
     let getInput = do opts <- rx iface
		       case opts of
		         Just s -> do writeChan inCh s
				      unless (null s) getInput
			 _ -> do debug "Line buffered input parser failure"
				 writeChan inCh []
     fork $ do getInput
	         debug "End of line buffered input"
     lns <- lines . concat . takeWhile (not.null) # getChanContents inCh
     linesCh <- newChan
     fork $ mapM_ (writeChan linesCh) lns >> writeChan linesCh []
     return iface { rx=readChan linesCh }
-}
lineBuffered debug iface =
  do bufVar <- newMVar []
     let rxloop buf = case break (=='\n') buf of
		        (line,'\n':buf') -> return (line,buf')
		        _ -> maybe (rxloop buf) (rxloop . (buf++)) =<< rx iface
         rxLine = do buf <- takeMVar bufVar
		     (line,buf') <- rxloop buf
		     putMVar bufVar buf'
		     return line
     return iface { rx=rxLine }

-- | A simple telnet server for line-based services
telnetServer debug prompt execute net =
    simpleTCPServer debug net Port.telnet (server @@ lineBuffered debug)
  where
    server iface =
      do debug "Telnet server prompting for a command"
	 tx iface prompt
	 s <- rx iface
	 debug $ "Got a command: "++show s
	 if null s || s=="logout\r"
	   then do debug "Telnet session ending"
		   return ()
	   else do let cmd = [c|c<-s,isPrint c && isAscii c]
		   execute iface cmd
	           server iface

-- | A simple telnet client for line-based services
telnet user net peer =
     maybe timeout telnetSession =<< TCP.connect (tcp net) peer
  where
    timeout = tx user "Connection attempt timed out\n"

    telnetSession iface =
	do tx user $ "Connected to "++show peer++"\n"
	   stopRef <- newRef False
	   fork $
	     do repeatM $ do s<- fromJust . doParse # TCP.rx iface
			     tx user s
			     return $ not $ null s
		writeRef stopRef True
	   repeatM $
	     do line <- addcr # rx user
		stop <- (line==".\r" ||) # readRef stopRef
		unless stop $ TCP.tx iface . doUnparse $ line++"\n"
		return (not stop)
	   TCP.close iface
	   tx user $ "Connection to "++show peer++" closed\n"
    addcr s = if null s || last s/='\r' then s++"\r" else s
