module Net.H(module Net) where
import H.Concurrency as H
import H.Mutable as H
import H.Monad(H)
import Net.Concurrent as Net

instance ForkIO H where
  fork = forkH
  kill = killH

instance DelayIO H where
  delay = H.threadDelay

instance ChannelIO Chan H where
  newChan = H.newChan
  writeChan = H.writeChan
  readChan = H.readChan

instance MVarIO MVar H where
  newEmptyMVar = H.newEmptyMVar
  newMVar = H.newMVar
  putMVar = H.putMVar
  takeMVar = H.takeMVar
  readMVar = H.readMVar

instance RefIO H.Ref H where
  newRef = H.newRef
  readRef = H.readRef
  writeRef = H.writeRef
