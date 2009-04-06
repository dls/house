diff -u ghc-xen/libraries/base/GHC/Conc.lhs ghc-house/libraries/base/GHC/Conc.lhs
--- ghc-xen/libraries/base/GHC/Conc.lhs	2008-05-29 21:34:09.000000000 +0000
+++ ghc-house/libraries/base/GHC/Conc.lhs	2008-02-14 01:06:12.000000000 +0000
@@ -40,8 +40,8 @@
 
 	-- * Waiting
 	, threadDelay	  	-- :: Int -> IO ()
-	, registerDelay		-- :: Int -> IO (TVar Bool)
 #ifndef house_HOST_OS
+	, registerDelay		-- :: Int -> IO (TVar Bool)
 	, threadWaitRead	-- :: Int -> IO ()
 	, threadWaitWrite	-- :: Int -> IO ()
 #endif
@@ -681,6 +681,10 @@
 -- when the delay has expired, but the thread will never continue to
 -- run /earlier/ than specified.
 --
+#ifdef house_HOST_OS
+threadDelay :: Int -> IO ()
+threadDelay (I# time#) = IO $ \s -> case delay# time# s of s -> (# s, () #)
+#else
 threadDelay :: Int -> IO ()
 threadDelay time
   | threaded  = waitForDelayEvent time
@@ -697,9 +701,11 @@
 registerDelay usecs 
   | threaded = waitForDelayEventSTM usecs
   | otherwise = error "registerDelay: requires -threaded"
+#endif /* !house_HOST_OS */
 
 foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool
 
+#ifndef house_HOST_OS
 waitForDelayEvent :: Int -> IO ()
 waitForDelayEvent usecs = do
   m <- newEmptyMVar
@@ -721,6 +727,7 @@
 calculateTarget usecs = do
     now <- getUSecOfDay
     return $ now + (fromIntegral usecs)
+#endif /* !house_HOST_OS */
 
 
 -- ----------------------------------------------------------------------------
