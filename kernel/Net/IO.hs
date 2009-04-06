module Net.IO(module Net) where
import qualified Control.Concurrent as IO
import Data.IORef
import Net.Concurrent as Net

instance ForkIO IO where
  fork = IO.forkIO
  kill = IO.killThread

instance DelayIO IO where
  delay = IO.threadDelay

instance ChannelIO IO.Chan IO where
  newChan = IO.newChan
  readChan = IO.readChan
  writeChan = IO.writeChan

instance MVarIO IO.MVar IO where
  newEmptyMVar = IO.newEmptyMVar
  newMVar = IO.newMVar
  putMVar = IO.putMVar
  takeMVar = IO.takeMVar
  readMVar = IO.readMVar

instance RefIO IORef IO where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
