diff -u ghc-xen/libraries/base/include/HsBase.h ghc-house/libraries/base/include/HsBase.h
--- ghc-xen/libraries/base/include/HsBase.h	2008-05-29 21:34:09.000000000 +0000
+++ ghc-house/libraries/base/include/HsBase.h	2008-02-14 01:23:50.000000000 +0000
@@ -680,7 +680,7 @@
 
 // gettimeofday()-related
 
-#if !defined(__MINGW32__)
+#if !defined(__MINGW32__) && !defined(house_HOST_OS)
 
 INLINE HsInt sizeofTimeVal(void) { return sizeof(struct timeval); }
 
@@ -699,12 +699,16 @@
     p->tv_sec  = usecs / 1000000;
     p->tv_usec = usecs % 1000000;
 }
-#endif /* !defined(__MINGW32__) */
+#endif /* !defined(__MINGW32__) && !defined(house_HOST_OS) */
 
 /* ToDo: write a feature test that doesn't assume 'environ' to
  *    be in scope at link-time. */
+#ifdef house_HOST_OS
+INLINE char **__hscore_environ() { return NULL; }
+#else
 extern char** environ;
 INLINE char **__hscore_environ() { return environ; }
+#endif
 
 /* lossless conversions between pointers and integral types */
 INLINE void *    __hscore_from_uintptr(uintptr_t n) { return (void *)n; }
