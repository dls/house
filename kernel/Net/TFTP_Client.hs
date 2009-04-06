module Net.TFTP_Client(tftpGet,tftpPut) where

-- TFTP Client, protocol described in RFC 1350
-- See http://www.networksorcery.com/enp/rfc/rfc1350.txt

import Net.Concurrent

import Net.UDP_Client as UDP(Packet(..),template,listenAny,unlisten)
import qualified Net.PortNumber as Port
import Net.TFTP
import qualified Net.Interface as Net
import Net.Packet(listArray)
import Net.PacketParsing(doUnparse,doParse)
import Net.Utils(arraySize)
--import Monad.Util(whileM)

tftpGet debug udp serverIP filename mode =
  do (tftp,close) <- initialize udp serverIP
     result <- getBlocks tftp [] 0
     close
     return result
  where
    rrq = RRQ filename mode

    getBlocks tftp bs last =
      do p <- txRetry tftp (if last==0 then rrq else Ack last)
	 case p of
	   Nothing -> return $ Left "Timeout"
           Just (Data nr b) ->
	       if nr==last+1 
	       then if arraySize b<512
		    then do Net.txT tftp (Ack nr)
			    return (Right (reverse (b:bs)))
		    else getBlocks tftp (b:bs) nr
	       else if nr==last
		    then getBlocks tftp bs last -- ignore dupl
		    else return (Left "unexpected data block")
	   Just (Error c msg) ->
	       return $ Left $ "Server said: "++show c++" "++msg
	   Just msg -> return $ Left $ "Unexpected packet: "++show msg

tftpPut debug udp serverIP filename mode contents =
  do (tftp,close) <- initialize udp serverIP
     result <- putBlocks tftp (blocks contents) (WRQ filename mode) 0
     close
     return result
  where
    putBlocks tftp bs packet current =
      do --debug $ "tftpPut send "++show packet
         p <- txRetry tftp packet
         --debug $ "tftpPut receive "++show p
	 case p of
	   Nothing -> return $ Left "Timeout"
           Just (Ack nr) ->
	       if nr==current
	       then case bs of
                      [] -> return (Right ())
                      b:bs -> putBlocks tftp bs (Data nr b) nr
		        where nr=current+1
	       else if nr==current-1
		    then putBlocks tftp bs packet current -- hmm, ignore dupl
		    else return (Left "unexpected ack")
	   Just (Error c msg) ->
	       return $ Left $ "Server said: "++show c++" "++msg
	   Just msg -> return $ Left $ "Unexpected packet: "++show msg

    blocks :: String -> [Data]
    blocks s =
      case splitAt 512 s of
	([],_) -> []
	(s1,s2) -> listArray (1,length s1) (conv s1):blocks s2
      where
        conv = map (fromIntegral.fromEnum)

txRetry tftp packet = rx 2
  where
     rx n =
       do Net.txT tftp packet
          maybe retry (return . Just) =<< Net.rxT tftp (Just t)
       where
         retry = if n>0 then rx (n-1) else return Nothing
         t = 2000000 -- microseconds

{-
txRetry tftp packet = rx 2
  where
     rx n =
	 do waiting <- newRef True
	    fork $ whileM (readRef waiting) $ Net.tx tftp packet >> delay t
	    p <- Net.rx tftp
	    writeRef waiting False
	    return (Just p)
       where
         t = 2000000 -- microseconds
-}

initialize udp serverIP =
  do (port,uclient) <- UDP.listenAny udp
     portM <- newMVar Nothing
     let rx t =
	   do r <- Net.rxT uclient t
	      case r of
		Nothing -> return Nothing
		Just (fromIP,udpP) ->
		  let sPort = sourcePort udpP in
		  case doParse (content udpP) of
		    Nothing -> rx t -- reduce t!!
		    Just p -> if fromIP==serverIP -- also check port number
			      then do optsp <- takeMVar portM
				      case optsp of
					Nothing ->
					    do putMVar portM (Just sPort)
					       return (Just p)
					Just port ->
					    do putMVar portM optsp
					       if port==sPort
						  then return (Just p)
						  else rx t -- wrong port!
			      else rx t
	 tx msg =
	     -- The initial request is sent to serverPort. After that,
	     -- messages are sent to the port chosen by the server.
	     do sPort <- maybe serverPort id `fmap` readMVar portM
		Net.txT uclient (serverIP,udpP sPort)
            where
              udpP sPort = UDP.template port sPort bs
              bs = doUnparse msg
						    
     return (Net.TimedInterface rx tx,unlisten udp port)

serverPort = Port.tftp -- standard TFTP server port
