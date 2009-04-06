diff -u ghc-xen/rts/RtsFlags.c ghc-house/rts/RtsFlags.c
--- ghc-xen/rts/RtsFlags.c	2008-05-29 21:34:10.000000000 +0000
+++ ghc-house/rts/RtsFlags.c	2008-05-22 21:11:58.000000000 +0000
@@ -95,6 +95,7 @@
    Static function decls
    -------------------------------------------------------------------------- */
 
+#ifndef house_HOST_OS
 static int		/* return NULL on error */
 open_stats_file (
     I_ arg,
@@ -105,6 +106,7 @@
 
 static I_ decode(const char *s);
 static void bad_option(const char *s);
+#endif
 
 #if defined(GRAN)
 static void enable_GranSimLight(void);
@@ -166,7 +168,7 @@
 #endif
 
 #ifdef DEBUG
-    RtsFlags.DebugFlags.scheduler	= rtsFalse;
+    RtsFlags.DebugFlags.scheduler	= rtsFalse; // handy
     RtsFlags.DebugFlags.interpreter	= rtsFalse;
     RtsFlags.DebugFlags.weak		= rtsFalse;
     RtsFlags.DebugFlags.gccafs		= rtsFalse;
@@ -324,6 +326,7 @@
 #endif
 }
 
+#ifndef house_HOST_OS
 static const char *
 usage_text[] = {
 "",
@@ -1320,6 +1323,8 @@
     }
 }
 
+#endif /* !house_HOST_OS */
+
 #if defined(GRAN)
 
 static void
@@ -2293,6 +2298,7 @@
 
 #endif /* PAR */
 
+#ifndef house_HOST_OS
 static void
 stats_fprintf(FILE *f, char *s, ...)
 {
@@ -2379,6 +2385,7 @@
   errorBelch("bad RTS option: %s", s);
   stg_exit(EXIT_FAILURE);
 }
+#endif /* !house_HOST_OS */
 
 /* -----------------------------------------------------------------------------
    Getting/Setting the program's arguments.
@@ -2443,6 +2450,7 @@
 void
 setFullProgArgv(int argc, char *argv[])
 {
+#ifndef house_HOST_OS
     int i;
     full_prog_argc = argc;
     full_prog_argv = stgCallocBytes(argc + 1, sizeof (char *),
@@ -2453,5 +2461,6 @@
         strcpy(full_prog_argv[i], argv[i]);
     }
     full_prog_argv[argc] = NULL;
+#endif
 }
 
