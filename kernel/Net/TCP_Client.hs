module Net.TCP_Client(
    initialize,Active(..),tx,rx,Passive(..),Interface(..),Peer,Port(..)
  ) where

-- Transmission Control Protocol
-- See http://www.networksorcery.com/enp/protocol/tcp.htm 
--     http://www.networksorcery.com/enp/rfc/rfc793.txt

import Net.Concurrent
import Control.Monad.State
import Control.Monad.Trans(lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List((\\))
import Data.Word(Word8,Word16,Word32)

import Net.TCP as TCP
import Net.PortNumber
import qualified Net.IPv4 as IPv4
import qualified Net.Interface as Net
import Net.Utils as Util(doReq,contents,checksum,bytes_to_words_big)
import Net.Packet(InPacket,len,dropInPack,
		  OutPacket,outLen,outBytes,emptyInPack,
		  emptyOutPack,appendOutPack,splitOutPack)
import Net.PacketParsing(doUnparse)
import Monad.Util

data Active  m = Active  { close:: m (), io::Net.Interface m InPacket OutPacket }
data Passive m = Passive { accept::m (Peer,Active m), unlisten::m () }
type Peer = (IPv4.Addr,Port)

tx = Net.tx . io
rx = Net.rx . io

data Interface m
  = Interface {
      listen  :: Port -> m (Passive m),
      connect :: Peer -> m (Maybe (Active m))
    }

--------------------------------------------------------------------------------

data Req m
  = Listen  Port (Passive m->m ())
  | Unlisten Port
  | Connect Peer (Maybe (Active m) ->m ())
  | Disconnect Port Peer -- from connection handling thread
  | FromNetwork TCPPacketIn

data State m = T { listeners::Listeners m, connections::Connections m }
type Connections m = Map (Port,Peer) (TCPPacketIn->m ())
type Listeners m = Map Port (Listening m)
type Listening m = (Peer,Active m)->m ()

type TCPPacketIn = TCPPacket InPacket
type TCPPacketOut = TCPPacket OutPacket
type TCPPacket contents = IPv4.Packet (Packet contents)

type TCPIPLink m = Net.Interface m TCPPacketIn TCPPacketOut

{-# NOINLINE initialize #-}
initialize putStrLn myIP iface =
  do reqChan <- newChan
     fork $ loop $ writeChan reqChan . FromNetwork =<< Net.rx iface
     fork $ server debug myIP iface reqChan
     return $ Interface { listen = doReq reqChan . Listen,
			  connect = doReq reqChan . Connect
			}
  where
    debug = putStrLn . ("TCP: "++)

server debugIO myIP iface reqChan =
    flip evalStateT init $ loop (handle=<<lift (readChan reqChan))
  where
    -- _ = iface::TCPIPLink

    init = T {listeners=Map.empty, connections=Map.empty}
    debug msg = lift (debugIO msg)

    handle req =
      case req of
        FromNetwork ipPacket  -> handlePacket ipPacket
        Listen     port reply -> addListener port reply
	Connect    peer reply -> activate peer (sendSyn reply)
        Unlisten   port       -> modify $ unlisten port
        Disconnect port peer  -> modify $ disconnect (port,peer)

    -- State updates:
    listen   port accept s@T{listeners=l} = s{listeners=Map.insert port accept l}
    unlisten port        s@T{listeners=l} = s{listeners=Map.delete port l}
    connect    c fwd  s@T{connections=cs} = s{connections=Map.insert c fwd cs}
    disconnect c      s@T{connections=cs} = s{connections=Map.delete c cs}

    addListener port reply =
      do -- check that port is not already listening
	 acceptCh <- lift newChan
	 lift $
	   reply Passive { accept=readChan acceptCh,
			   unlisten=writeChan reqChan (Unlisten port) }
	 let accept = writeChan acceptCh
	 modify $ listen port accept

    handlePacket ipPacket =
      if okTCPchksum ipPacket
      then handleOkPacket ipPacket
      else debug "Dropping packet with bad checksum"
    handleOkPacket ipPacket =
      do let packet = IPv4.content ipPacket
	     peer = (IPv4.source ipPacket,sourcePort packet)
	     me   = (IPv4.dest ipPacket,port)
	     c    = (me,peer)
	     port = destPort packet
	     CB{ack=a,syn=s} = controlBits packet
	     acknr = ackNr packet
	     dropit =
	       debug $ "Dropped packet from "++show peer ++" to "++show me
			++ "\n"++show ipPacket
	 optcon <- gets (Map.lookup (port, peer).connections)
	 case optcon of
	   Just toConnection -> do --debug $ "Forwarding "++show c
				   lift $ toConnection ipPacket
	   _ -> do optlistener <- gets (Map.lookup port . listeners)
		   case optlistener of
		     Just listener | s && not a ->
		       activate' c (synReceived ipPacket listener)
		     _ | a -> reset c acknr -- Half-open connection detected
		     _ -> dropit

    reset c acknr =
        do let rst = minBound{rst=True}
	   debug $ "RST "++show c
	   lift $ Net.tx iface (setTCPchksum (tcpPacket () rst c acknr 0))

    pickPort = do T{listeners=l,connections=c} <- get
		  let inuse = Map.keys l++map fst (Map.keys c) -- duplicates, slow?
		  return $ head (map Port [32768..65535]\\inuse)

    activate peer handler =
	do port <- pickPort -- find an unused port
	   let me = (myIP,port)
	   activate' (me,peer) handler

    activate' c@(me@(_,port),peer) handler =
        do outCh <- lift newChan -- packets from client to connection
	   inCh <- lift newChan -- packets from connection to client
	   flowctl <- lift $ newMVar () -- for client output flow control
           let cdebug msg = debugIO $ show me++"<->"++show peer++"\n    "++msg
	       forward = writeChan outCh . ConFromNetwork
	   modify $ connect (port,peer) forward
           let io = Net.Interface { Net.rx=readChan outCh,
				    Net.tx=Net.tx iface . setTCPchksum }
	       active = Active { close=writeChan outCh Close,
				 io=Net.Interface {
				      Net.rx=readChan inCh,
				      Net.tx=tx}}
                 where tx p = do --debugIO $ "takeMVar flowctl "++show (outLen p)
                                 takeMVar flowctl
				 writeChan outCh (ConTx p)
				 --debugIO $ "tookMvar flowctl"
	   lift $ fork $
             do t <- fork $ timer (writeChan outCh . Tick)
                handler c cdebug (writeChan inCh)  io flowctl active
		kill t
                writeChan reqChan (Disconnect port peer)
           return ()

--------------------------------------------------------------------------------
synReceived ipPacket reply c@(_,peer) debug deliver io flowctl active =
    do --debug $ "SYN received " ++show rxSeqNr
       let synackP = synackPacket c txSeqNr (rxSeqNr+1)
       --debug $ "Sending SYN ACK " ++show txSeqNr++" "++show (rxSeqNr+1)
       maybe done gotAck =<< waitForAck synackP
  where
    gotAck ip =
      do --debug $ "Got ACK, connection is established"
	 reply (peer,active) -- not until connection is established
	 let tcp = contents ip
	     dat = contents tcp
	     l = fromIntegral (len dat)
	 when (l>0) $ do debug $ "ACK and delever initial bytes "++show l
			 Net.tx io (ackPacket c (txSeqNr+1) (rxSeqNr+1+l))
			 deliver dat
	 established c debug deliver io flowctl (rxSeqNr+1+l,txSeqNr+1,txWindow)

    tcp = contents ipPacket
    rxSeqNr = seqNr tcp
    txWindow = window tcp
    txSeqNr = 10000000 -- should be chosen randomly!!!

    waitForAck synackP = solicitPacket debug io synackP expected
      where
        expected p = if cb==minBound{ack=True} &&
		        ackNr tcp==txSeqNr+1 && seqNr tcp==rxSeqNr+1
		     then Just p
		     else Nothing
	  where tcp = contents p
		cb = controlBits tcp
--------------------------------------------------------------------------------

solicitPacket debug io request expected = loop 3 0
  where
    loop 0 _ = return Nothing
    loop retries 0 =
      do debug "Retrying"
         Net.tx io request
	 loop (retries-1) (3*ticksPerSecond)
    loop retries t =
      do r <- Net.rx io
	 case r of
	   Tick _ -> loop retries (t-1)
	   ConFromNetwork p -> case expected p of
			         Just r -> return (Just r)
				 _ -> loop retries t
	   _ -> loop retries t

--------------------------------------------------------------------------------
sendSyn reply c debug deliver io flowctl active =
    do let synP = synPacket c iss 0
       --debug "Sent SYN, waiting for SYN ACK"
       maybe noreply gotAck =<< waitForAck synP
  where
    iss = 20000000 -- Initial Send Sequence number, should be chosen randomly!!!

    noreply = reply Nothing

    gotAck (irs,txWindow) =
      do --debug $ "Got SYN ACK, sending ACK, IRS="++show irs
         Net.tx io (ackPacket c (iss+1) (irs+1))
         reply (Just active)
         established c debug deliver io flowctl (irs+1,iss+1,txWindow)

    waitForAck synP = solicitPacket debug io synP expected
      where
        expected p =
	  let tcp = contents p
	      cb = controlBits tcp
	  in if cb==minBound{ack=True,syn=True} && ackNr tcp==iss+1
	     then Just (seqNr tcp,window tcp)
	     else Nothing

--------------------------------------------------------------------------------
dataPacket dat = tcpPacket dat minBound{ack=True}
ackPacket      = dataPacket ()
finPacket      = emptyPacket minBound{ack=True,fin=True}
synPacket   = emptyPacket minBound{syn=True}
synackPacket   = emptyPacket minBound{syn=True,ack=True}
emptyPacket    = tcpPacket ()

tcpPacket dat cb ((myIP,myPort),(peerIP,peerPort)) seqnr acknr =
    iptemplate tcp{content=doUnparse dat}
  where
    tcp = template{sourcePort=myPort,destPort=peerPort,
		   ackNr=acknr,seqNr=seqnr,controlBits=cb}

    iptemplate = IPv4.template IPv4.TCP myIP peerIP

--------------------------------------------------------------------------------

-- Requests to connection handling thread:
data ConReq = Close
	    | ConTx OutPacket
	    | ConFromNetwork TCPPacketIn
            | Tick Int

data ConState = S { phase::Phase,
		    now,roundTripTime::Int,
		    unackedData::[(Word32,Int,OutPacket)],
		    unsentData::OutPacket,
		    txUnacked,txSeq,txWindow,rxSeq,rxWindow::Word32 }
data Phase = Established | CloseWait
	   | Closing | FinWait1 | FinWait2 | LastAck
	   | TimeWait | Closed
	   deriving (Eq,Ord,Show)

conReq disc tx rx tick req =
  case req of
    Close -> disc
    ConTx p -> tx p
    ConFromNetwork p -> rx p
    Tick t -> tick t

ticksPerSecond=10

timer m = loop 0
  where
    loop t = do delay us
		m t
		loop (t+1)
    us = 1000000 `div` ticksPerSecond

established c debugIO deliver io flowctl (rxseq,txseq,txwin) =
    flip evalStateT state0 $
       do debug $ "Transmit window = "++show txwin
          whileM ((<TimeWait) # gets phase) (handle=<<rx)
	  p <- gets phase
	  --when (p==TimeWait) $ do debug "Waiting"
	  delay 30000000 -- wait 30 seconds before reusing the same port number
	  --debug "Closed"
  where
    handle = conReq close conTx conRx tick

    state0 = S {phase=Established,
		now=0,
		roundTripTime=3*ticksPerSecond, -- Use 3s as the intitial RTT
		unackedData=[],
		unsentData=emptyOutPack,
		txUnacked=txseq,
		txSeq=txseq,
		txWindow=fromIntegral txwin,
		rxSeq=rxseq,
		rxWindow=1400}

    fakeMSS = 512 -- !!! maximum segment size

    debug = lift . debugIO
    rx = lift (Net.rx io)
    tx = lift . Net.tx io

    acknowledge acknr =
	do s@S{txSeq=seq} <- get
	   tx (ackPacket c seq acknr)
	   put s{rxSeq=acknr}

    sendData dat =
        do s@S{txSeq=seq,now=t,unackedData=ps} <- get
	   let l=fromIntegral (outLen dat)
	   when (l>0) $ 
	       do sendData' dat seq
                  put s{txSeq=seq+l,unackedData=ps++[(seq,t,dat)]}
	          --debug $ "Sent "++show l++" bytes upto "++show (seq+l)
    sendData' dat seq = tx . dataPacket dat c seq =<< gets rxSeq

    trySendData =
        do S{txSeq=seq,txUnacked=unacked,txWindow=win} <- get
	   when (seq-unacked<win) $
             do let n=fromIntegral (unacked+win-seq)
		sendData =<< unqueueData n

    queueData p =
	do --debug $ "Adding "++show (outLen p)++" bytes to transmit queue"
	   s@S{unsentData=old,txWindow=win} <- get
	   let new=appendOutPack old p
           put s{unsentData=new}
	   -- Allow client to queue more output?
	   if outLen' new<win
	      then do --debug "Unblocking client..."
		      putMVar flowctl ()
		      --debug "Unblocked client"
	      else debug "Leaving client blocked"

    outLen' = fromIntegral . outLen

    unqueueData n =
	do s@S{unsentData=old,txWindow=win} <- get
           let (p1,p2) = splitOutPack (min fakeMSS n) old
           put s{unsentData=p2}
	   let l = outLen' p1
               q = outLen' p2
           -- Allow client to queue more output:
           --{-
	   when (outLen' old>=win && q<win) $
		do debug "(Delayed) unblocking client..."
		   putMVar flowctl ()
		   debug "(Delayed) unblocked client"
		   
           --}
           {-
	   when (l>0 || q>0) $
		debug $ "Sending "++show l++" bytes, "
			  ++show q++ " bytes left in transmit queue"
           --}
	   return p1

    sendFin =
	do s@S{txSeq=seq,rxSeq=ack} <- get
	   tx (finPacket c seq ack)
	   put s{txSeq=seq+1}

    goto p = do modify $ \ s -> s{phase=p}
		--debug $ "Go to state "++show p

    -- Some time has passed:
    tick now =
      do rtt <- gets roundTripTime
         (ps',timeout) <-
            flip runStateT False . mapM (retransmit rtt) =<< gets unackedData
	 when timeout $ modify $ \ s -> s{roundTripTime=backoff rtt}
	 modify $ \ s -> s{now=now,unackedData=ps'}
      where
         backoff rtt = min (5*ticksPerSecond) (2*(max 1 rtt))

         retransmit rtt p@(seq,t,buf) =
	     if now>1+t+2*rtt
	     then do lift $ debug $ "Retransmitting seqNr "++show seq
                                     ++ " len "++show (outLen buf)
                                     ++" after "++show(now-t)++" ticks"
                     lift $ sendData' buf seq
		     put True
		     return (seq,now,buf)
	     else return p

    -- Local request to close the connection:
    close =
      do p <- gets phase
	 case p of
	   Established -> do sendFin ; goto FinWait1
	   CloseWait   -> do sendFin ; goto LastAck
	   _ -> debug "Buggy local client closing more than once"

    -- Local request to send some data:
    conTx dat =
      do p <- gets phase
	 --let l=fromIntegral (outLen dat)
	 --when (l>0) $
	 if p>CloseWait
	   then debug "Buggy local client sending after closing"
	   else do queueData dat
	           trySendData

    -- Receiver a packet from the network:
    conRx ip | rst (controlBits (contents ip)) = -- also check seqNr
      -- Should probably notify client that the connection was reset
      -- and not just closed the normal way...
      do p <- gets phase
	 when (p==Established) $ lift (deliver emptyInPack) -- EOS
	 goto TimeWait -- or Closed?
    conRx ip =
      do let tcp   = contents ip
	     got   = seqNr tcp
	     dat   = contents tcp
	     l=fromIntegral (len dat)
	     cb=controlBits tcp
	 --debug $ "Got packet with "++show cb++" and "++show l++" bytes of data"
	 expecting <- gets rxSeq
	 -- also check RST flag!
	 when (l>0) $
	     do let new=got+l-expecting
                rxwin <- gets rxWindow
		if new>0 && new<=rxwin
		   then
		     do --debug $ "ACK upto "++show ack
			    --    ++" and deliver "++show new++" bytes"
			let ack=got+l
			    dup=fromIntegral (l-new)
			modify $ \ s->s{rxSeq=ack}
			lift $ deliver (skipIn dup dat)
		   else do acknowledge expecting
			   debug $ "got duplicate input "
			           ++show (got,l,expecting)
	 S{phase=p,rxSeq=expecting} <- get
	 if fin cb
	   then let finseq=got+l
                    ack=finseq+1
		    ackgoto p = do acknowledge ack;goto p
	        in if finseq/=expecting
		   then debug "FIN with unexpected sequence number"
		   else case p of
			  Established -> do lift (deliver emptyInPack) -- EOS
					    ackgoto CloseWait
			  FinWait1    -> do ackgoto Closing
			  FinWait2    -> do ackgoto TimeWait
			  _ -> debug "Unexpected FIN"
           else acknowledge expecting
	 S{txUnacked=unacked,txSeq=seq} <- get
	 let acknr = ackNr tcp
	 when (ack cb && acknr/=unacked &&
	       acknr-unacked<=seq-unacked) $ -- !! modulo arithmetic
	      do s@S{now=now,roundTripTime=oldrtt,unackedData=ps} <- get
		 let (ps1,ps2) =span (isAcked acknr) ps
		     newrtt=if null ps1
			     then oldrtt
			     else maximum [ now-t | (_,t,_)<-ps1]
		     rtt=(oldrtt+newrtt) `div` 2
		 put s{txUnacked=acknr,unackedData=ps2,roundTripTime=rtt}
		 {-
		 debug $ "Update ACKed output to "++show acknr
                         ++", "++show (length ps2)++" unacked packets"
			 ++", new roundtrip time="++show rtt++" ticks"
		 -}
		 trySendData
		 seq <- gets txSeq
		 when (acknr==seq) $ -- when everything sent has been acked
		   case phase s of
		     FinWait1 -> goto FinWait2
		     Closing  -> goto TimeWait
		     LastAck  -> goto Closed
		     _ -> return ()

isAcked acknr (seq,t,buf) = seq+fromIntegral (outLen buf)<=acknr

--------------------------------------------------------------------------------
okTCPchksum ip = tcp_chksum ip == 0

setTCPchksum ip = ip{IPv4.content=tcp'}
  where
    tcp' = tcp{TCP.checksum=tcp_chksum ip}
    tcp = contents ip

tcp_chksum ip = outPacketChecksum pseudoTCP
  where
    tcp = contents ip
    pseudoHeader = (IPv4.source ip,IPv4.dest ip,0::Word8,IPv4.TCP,tcpLength)
    tcpLength = fromIntegral (outLen utcp)::Word16
    pseudoTCP = doUnparse (pseudoHeader,utcp)
    utcp = doUnparse tcp -- TCP packet will be serialized twice!!
    outPacketChecksum = Util.checksum . bytes_to_words_big . outBytes

--pre: n<=len p
skipIn n p = dropInPack n p
