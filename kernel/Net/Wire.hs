module Net.Wire where

import Net.Concurrent
import Net.Interface as Net

type Wire m a = Interface m a a

-- | An unbounded buffer
--unbounded :: ChannelIO c m => m (Wire m a)
unbounded () = 
   do channel <- newChan
      return Interface{rx=readChan channel,tx=writeChan channel}

newWire () = bounded (unbounded ())
newWire' n = bounded' n (unbounded ())

bounded new = bounded' (10::Int) new


-- | A bounded buffer that discards input when full
bounded' buffersize newWire =
    do wire <- newWire
       size <- newMVar 0
       let tx p = do current <- takeMVar size
		     if current<buffersize
		       then do Net.tx wire p
			       putMVar size (succ current)
		       else putMVar size current -- discard p!
	   rx   = do putMVar size . pred =<< takeMVar size
		     Net.rx wire
       return Interface{rx=rx,tx=tx}


timedWire () =
  do channel <- newChan
     let rxTimeout t =
	     do u <- newRef () -- just something unique
		t <- fork $ do delay t ; writeChan channel (Left u)
		let read = do m <- readChan channel
			      case m of
				Right x -> return (Just x)
				Left v | v==u -> return Nothing
				       | otherwise -> read
		x <- read
		kill t
		return x
         rxWait = do m <- readChan channel
		     case m of
		       Right x -> return (Just x)
		       _ -> rxWait
	 rx = maybe rxWait rxTimeout
	 tx = writeChan channel . Right
     return (TimedInterface rx tx)
