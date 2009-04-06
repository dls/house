diff -u ghc-xen/rts/ProfHeap.c ghc-house/rts/ProfHeap.c
--- ghc-xen/rts/ProfHeap.c	2008-05-29 21:34:10.000000000 +0000
+++ ghc-house/rts/ProfHeap.c	2008-02-14 00:42:55.000000000 +0000
@@ -396,6 +396,7 @@
 
 void initProfiling2 (void)
 {
+#ifndef house_HOST_OS
   if (RtsFlags.ProfFlags.doHeapProfile) {
     /* Initialise the log file name */
     hp_filename = stgMallocBytes(strlen(prog_name) + 6, "hpFileName");
@@ -411,6 +412,7 @@
   }
   
   initHeapProfiling();
+#endif
 }
 
 void endProfiling( void )
@@ -422,11 +424,13 @@
 static void
 printSample(rtsBool beginSample, StgDouble sampleValue)
 {
+#ifndef house_HOST_OS
     StgDouble fractionalPart, integralPart;
     fractionalPart = modf(sampleValue, &integralPart);
     fprintf(hp_file, "%s %" FMT_Word64 ".%02" FMT_Word64 "\n",
             (beginSample ? "BEGIN_SAMPLE" : "END_SAMPLE"),
             (StgWord64)integralPart, (StgWord64)(fractionalPart * 100));
+#endif
 }
 
 /* --------------------------------------------------------------------------
@@ -435,6 +439,7 @@
 nat
 initHeapProfiling(void)
 {
+#ifndef house_HOST_OS
     if (! RtsFlags.ProfFlags.doHeapProfile) {
         return 0;
     }
@@ -494,6 +499,7 @@
 	initRetainerProfiling();
     }
 #endif
+#endif /* !house_HOST_OS */
 
     return 0;
 }
@@ -501,6 +507,7 @@
 void
 endHeapProfiling(void)
 {
+#ifndef house_HOST_OS
     StgDouble seconds;
 
     if (! RtsFlags.ProfFlags.doHeapProfile) {
@@ -543,6 +550,7 @@
     printSample(rtsTrue, seconds);
     printSample(rtsFalse, seconds);
     fclose(hp_file);
+#endif /* !house_HOST_OS */
 }
 
 
@@ -1201,4 +1209,3 @@
   stat_endHeapCensus();
 #endif
 }    
-
