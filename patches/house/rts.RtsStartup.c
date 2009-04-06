diff -u ghc-xen/rts/RtsStartup.c ghc-house/rts/RtsStartup.c
--- ghc-xen/rts/RtsStartup.c	2008-05-29 21:34:10.000000000 +0000
+++ ghc-house/rts/RtsStartup.c	2008-02-13 12:09:40.000000000 +0000
@@ -200,12 +200,14 @@
     /* Call the user hook to reset defaults, if present */
     defaultsHook();
 
+#ifndef house_HOST_OS
     /* Parse the flags, separating the RTS flags from the programs args */
     if (argc != NULL && argv != NULL) {
 	setFullProgArgv(*argc,*argv);
 	setupRtsFlags(argc, *argv, &rts_argc, rts_argv);
 	setProgArgv(*argc,*argv);
     }
+#endif
 
     /* initTracing must be after setupRtsFlags() */
     initTracing();
@@ -242,7 +244,7 @@
     initTypeableStore();
 
     /* initialise file locking, if necessary */
-#if !defined(mingw32_HOST_OS)    
+#if !defined(mingw32_HOST_OS) && !defined(house_HOST_OS)
     initFileLocking();
 #endif
 
@@ -473,7 +475,7 @@
     exitTypeableStore();
 
     /* free file locking tables, if necessary */
-#if !defined(mingw32_HOST_OS)    
+#if !defined(mingw32_HOST_OS) && !defined(house_HOST_OS)
     freeFileLocking();
 #endif
 
