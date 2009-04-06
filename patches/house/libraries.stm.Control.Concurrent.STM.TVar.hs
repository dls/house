diff -u ghc-xen/libraries/stm/Control/Concurrent/STM/TVar.hs ghc-house/libraries/stm/Control/Concurrent/STM/TVar.hs
--- ghc-xen/libraries/stm/Control/Concurrent/STM/TVar.hs	2007-12-10 18:26:29.000000000 +0000
+++ ghc-house/libraries/stm/Control/Concurrent/STM/TVar.hs	2008-02-14 01:19:02.000000000 +0000
@@ -19,7 +19,7 @@
 	readTVar,
 	writeTVar,
 	newTVarIO,
-#ifdef __GLASGOW_HASKELL__
+#if defined(__GLASGOW_HASKELL__) && !defined(house_HOST_OS)
 	registerDelay
 #endif
   ) where
