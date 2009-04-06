diff -u ghc-xen/includes/RtsFlags.h ghc-house/includes/RtsFlags.h
--- ghc-xen/includes/RtsFlags.h	2008-05-29 21:34:09.000000000 +0000
+++ ghc-house/includes/RtsFlags.h	2008-02-13 11:48:50.000000000 +0000
@@ -9,7 +9,11 @@
 #ifndef RTSFLAGS_H
 #define RTSFLAGS_H
 
+#ifdef house_HOST_OS
+typedef struct _IO_FILE FILE;
+#else
 #include <stdio.h>
+#endif
 
 /* For defaults, see the @initRtsFlagsDefaults@ routine. */
 
