diff -u ghc-xen/rts/Timer.c ghc-house/rts/Timer.c
--- ghc-xen/rts/Timer.c	2008-05-29 21:34:10.000000000 +0000
+++ ghc-house/rts/Timer.c	2008-02-14 09:57:39.000000000 +0000
@@ -99,17 +99,21 @@
 void
 startTimer(void)
 {
+#ifndef house_HOST_OS
     if (RtsFlags.MiscFlags.tickInterval != 0) {
         startTicker();
     }
+#endif
 }
 
 void
 stopTimer(void)
 {
+#ifndef house_HOST_OS
     if (RtsFlags.MiscFlags.tickInterval != 0) {
         stopTicker();
     }
+#endif
 }
 
 void
