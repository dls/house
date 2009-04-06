diff -u ghc-xen/rts/RtsSignals.h ghc-house/rts/RtsSignals.h
--- ghc-xen/rts/RtsSignals.h	2008-05-29 21:34:10.000000000 +0000
+++ ghc-house/rts/RtsSignals.h	2008-02-13 12:14:31.000000000 +0000
@@ -19,8 +19,7 @@
 
 #elif defined(house_HOST_OS)
 
-extern int signals_pending();
-extern void startSignalHandlers(Capability *cap);
+#include "house/Signals.h"
 
 #else /* PAR */
 
