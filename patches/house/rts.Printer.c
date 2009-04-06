diff -u ghc-xen/rts/Printer.c ghc-house/rts/Printer.c
--- ghc-xen/rts/Printer.c	2007-12-10 18:11:31.000000000 +0000
+++ ghc-house/rts/Printer.c	2008-05-15 06:30:39.000000000 +0000
@@ -33,7 +33,7 @@
  * ------------------------------------------------------------------------*/
 
 static void    printStdObjPayload( StgClosure *obj );
-#ifdef USING_LIBBFD
+#if defined(USING_LIBBFD) && !defined(house_HOST_OS)
 static void    reset_table   ( int size );
 static void    prepare_table ( void );
 static void    insert        ( StgWord value, const char *name );
@@ -186,7 +186,9 @@
 	break;
 
     case BCO:
+#ifndef house_HOST_OS
             disassemble( (StgBCO*)obj );
+#endif
             break;
 
     case AP:
@@ -452,6 +454,7 @@
 
         StgClosure* c = (StgClosure*)(*sp);
         printPtr((StgPtr)*sp);
+#ifdef ALLOW_INTERPRETER
         if (c == (StgClosure*)&stg_ctoi_R1p_info) {
            debugBelch("\t\t\tstg_ctoi_ret_R1p_info\n" );
 	} else
@@ -471,7 +474,9 @@
            debugBelch("\t\t\t");
            debugBelch("BCO(...)\n"); 
         }
-        else {
+        else
+#endif
+        {
            debugBelch("\t\t\t");
            printClosure ( (StgClosure*)(*sp));
         }
@@ -754,7 +759,7 @@
 static nat table_size;
 static struct entry* table;
 
-#ifdef USING_LIBBFD
+#if defined(USING_LIBBFD) && !defined(house_HOST_OS)
 static nat max_table_size;
 
 static void reset_table( int size )
@@ -957,7 +962,7 @@
 /* Causing linking trouble on Win32 plats, so I'm
    disabling this for now. 
 */
-#ifdef USING_LIBBFD
+#if defined(USING_LIBBFD) && !defined(house_HOST_OS)
 
 #include <bfd.h>
 
