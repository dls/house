
> module Monad.IO where

> import Monad.MonadT

> class Monad m => IOMonad m where
>   writeln :: String -> m ()

> instance IOMonad IO where
>   writeln msg = putStrLn msg

> instance (MonadT t, IOMonad m, Monad (t m)) => IOMonad (t m) where
>   writeln msg = lift (writeln msg)


