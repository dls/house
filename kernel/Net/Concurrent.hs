-- | Concurrency primitives abstracted away from the IO monad
module Net.Concurrent where
import Control.Monad.Trans
import Control.Concurrent(ThreadId)

class (Functor io,Monad io) => ForkIO io where
  fork :: io () -> io ThreadId
  kill :: ThreadId -> io ()

class (Functor io,Monad io) => DelayIO io where
  delay :: Int -> io () -- microseconds

class (Functor io,Monad io) => ChannelIO c io | io->c where
  newChan :: io (c a)
  readChan :: c a -> io a
  writeChan :: c a -> a -> io ()
--isEmptyChan :: c a -> io Bool

class (Functor io,Monad io) => MVarIO v io | io->v where
  newEmptyMVar :: io (v a)
  newMVar :: a -> io (v a)
  putMVar :: v a -> a -> io ()
  takeMVar, readMVar :: v a -> io a
--tryPutMVar :: v a -> a -> io Bool
--withMVar :: v a -> (a -> io b) -> io b

  newMVar a = do v <- newEmptyMVar
		 putMVar v a
		 return v

class (Functor io,Monad io) => RefIO r io | io->r where
  newRef :: a -> io (r a)
  readRef :: r a -> io a
  writeRef :: r a -> a -> io ()

instance (MonadTrans t,Monad m,Functor (t m),Monad (t m),DelayIO m) => DelayIO (t m) where
  delay = lift . delay

instance (MonadTrans t,Monad m,Functor (t m),Monad (t m),ChannelIO c m) => ChannelIO c (t m) where
  newChan = lift newChan
  readChan c = lift $ readChan c
  writeChan c x = lift $ writeChan c x

instance (MonadTrans t,Monad m,Functor (t m),Monad (t m),MVarIO v m) => MVarIO v (t m) where
  newEmptyMVar = lift newEmptyMVar
  putMVar v x = lift $ putMVar v x
  takeMVar v = lift $ takeMVar v
  readMVar v = lift $ readMVar v


instance (MonadTrans t,Monad m,Functor (t m),Monad (t m),RefIO c m) => RefIO c (t m) where
  newRef = lift . newRef
  readRef c = lift $ readRef c
  writeRef c x = lift $ writeRef c x
