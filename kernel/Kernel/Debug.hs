module Kernel.Debug
    ( v_defaultConsole
    , Kernel.Debug.putStr
    , Kernel.Debug.putStrLn
    ) where

{-P:
import Prelude hiding (putStr,putStrLn)
-}
import H.Monad(H)
import H.Unsafe(unsafePerformH)


--import Control.Concurrent.Chan
import H.Concurrency 

import Kernel.Console

{-# NOINLINE v_defaultConsole #-}
v_defaultConsole :: MVar Console
v_defaultConsole = unsafePerformH $ newEmptyMVar

putStr :: String -> H ()
putStr = wrap putString

putStrLn :: String -> H ()
putStrLn = wrap putStringLn

wrap :: (Console -> String -> H ()) -> String -> H ()
wrap f str =
    do empty <- isEmptyMVar v_defaultConsole
       if empty
	  then return ()
	  else do chan <- readMVar v_defaultConsole
	          f chan str
