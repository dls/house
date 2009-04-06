diff -u ghc-xen/rts/RtsUtils.c ghc-house/rts/RtsUtils.c
--- ghc-xen/rts/RtsUtils.c	2007-12-10 18:11:31.000000000 +0000
+++ ghc-house/rts/RtsUtils.c	2008-02-13 23:13:21.000000000 +0000
@@ -319,6 +319,7 @@
     return(__GenSymCounter);
 }
 
+#ifndef house_HOST_OS
 /* -----------------------------------------------------------------------------
    Get the current time as a string.  Used in profiling reports.
    -------------------------------------------------------------------------- */
@@ -341,6 +342,7 @@
     }
     return nowstr;
 }
+#endif /* !house_HOST_OS */
 
 /* -----------------------------------------------------------------------------
  * Reset a file handle to blocking mode.  We do this for the standard
@@ -348,7 +350,7 @@
  * clean up for us.
  * -------------------------------------------------------------------------- */
 
-#if !defined(mingw32_HOST_OS)
+#if !defined(mingw32_HOST_OS) && !defined(house_HOST_OS)
 void
 resetNonBlockingFd(int fd)
 {
@@ -453,6 +455,7 @@
 }
 #endif
 
+#ifndef house_HOST_OS
 /* 
  * It seems that pthreads and signals interact oddly in OpenBSD & FreeBSD
  * pthreads (and possibly others). When linking with -lpthreads, we
@@ -486,4 +489,5 @@
     mkRtsInfoPair("Tables next to code",     GhcEnableTablesNextToCode);
     printf(" ]\n");
 }
+#endif /* !house_HOST_OS */
 
