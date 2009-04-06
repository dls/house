diff -u ghc-xen/libraries/base/Control/Concurrent.hs ghc-house/libraries/base/Control/Concurrent.hs
--- ghc-xen/libraries/base/Control/Concurrent.hs	2008-05-29 21:34:09.000000000 +0000
+++ ghc-house/libraries/base/Control/Concurrent.hs	2008-02-14 00:57:01.000000000 +0000
@@ -64,7 +64,7 @@
 #endif
 	-- $merge
 
-#ifdef __GLASGOW_HASKELL__
+#if defined(__GLASGOW_HASKELL__) && !defined(house_HOST_OS)
 	-- * Bound Threads
 	-- $boundthreads
 	rtsSupportsBoundThreads,
@@ -256,7 +256,7 @@
     mapIO f xs = sequence (map f xs)
 #endif /* __HUGS__ */
 
-#ifdef __GLASGOW_HASKELL__
+#if defined(__GLASGOW_HASKELL__) && !defined(house_HOST_OS)
 -- ---------------------------------------------------------------------------
 -- Bound Threads
 
@@ -298,7 +298,6 @@
 -- fail.
 foreign import ccall rtsSupportsBoundThreads :: Bool
 
-
 {- |
 Like 'forkIO', this sparks off a new thread to run the 'IO' computation passed as the
 first argument, and returns the 'ThreadId' of the newly created
@@ -413,7 +412,7 @@
                 Right result -> return result
         else action
 	
-#endif /* __GLASGOW_HASKELL__ */
+#endif /* __GLASGOW_HASKELL__ && !house_HOST_OS */
 
 -- ---------------------------------------------------------------------------
 -- More docs
