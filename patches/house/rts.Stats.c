diff -u ghc-xen/rts/Stats.c ghc-house/rts/Stats.c
--- ghc-xen/rts/Stats.c	2007-12-10 18:11:31.000000000 +0000
+++ ghc-house/rts/Stats.c	2008-02-13 12:24:24.000000000 +0000
@@ -73,12 +73,20 @@
 
 Ticks stat_getElapsedGCTime(void)
 {
+#ifdef house_HOST_OS
+    return 0;
+#else
     return GCe_tot_time;
+#endif
 }
 
 Ticks stat_getElapsedTime(void)
 {
+#ifdef house_HOST_OS
+    return 0;
+#else
     return getProcessElapsedTime() - ElapsedTimeStart;
+#endif
 }
 
 /* mut_user_time_during_GC() and mut_user_time()
@@ -96,15 +104,23 @@
 double
 mut_user_time_during_GC( void )
 {
+#ifdef house_HOST_OS
+  return 0;
+#else
   return TICK_TO_DBL(GC_start_time - GC_tot_time - PROF_VAL(RP_tot_time + HC_tot_time));
+#endif
 }
 
 double
 mut_user_time( void )
 {
+#ifdef house_HOST_OS
+  return 0;
+#else
     Ticks user;
     user = getProcessCPUTime();
     return TICK_TO_DBL(user - GC_tot_time - PROF_VAL(RP_tot_time + HC_tot_time));
+#endif
 }
 
 #ifdef PROFILING
@@ -129,6 +145,7 @@
 void
 initStats(void)
 {
+#ifndef house_HOST_OS
     nat i;
   
     if (RtsFlags.GcFlags.giveStats >= VERBOSE_GC_STATS) {
@@ -142,6 +159,7 @@
     for (i = 0; i < RtsFlags.GcFlags.generations; i++) {
 	GC_coll_times[i] = 0;
     }
+#endif
 }    
 
 /* -----------------------------------------------------------------------------
@@ -151,15 +169,18 @@
 void
 stat_startInit(void)
 {
+#ifndef house_HOST_OS
     Ticks elapsed;
 
     elapsed = getProcessElapsedTime();
     ElapsedTimeStart = elapsed;
+#endif
 }
 
 void 
 stat_endInit(void)
 {
+#ifndef house_HOST_OS
     Ticks user, elapsed;
 
     getProcessTimes(&user, &elapsed);
@@ -183,6 +204,7 @@
     papi_is_reporting = 1;
 
 #endif
+#endif
 }
 
 /* -----------------------------------------------------------------------------
@@ -194,6 +216,7 @@
 void
 stat_startExit(void)
 {
+#ifndef house_HOST_OS
     Ticks user, elapsed;
 
     getProcessTimes(&user, &elapsed);
@@ -215,11 +238,13 @@
     papi_is_reporting = 0;
 
 #endif
+#endif
 }
 
 void
 stat_endExit(void)
 {
+#ifndef house_HOST_OS
     Ticks user, elapsed;
 
     getProcessTimes(&user, &elapsed);
@@ -232,6 +257,7 @@
     if (ExitElapsedTime < 0) {
 	ExitElapsedTime = 0;
     }
+#endif
 }
 
 /* -----------------------------------------------------------------------------
@@ -248,6 +274,7 @@
 void
 stat_startGC(void)
 {
+#ifndef house_HOST_OS
     nat bell = RtsFlags.GcFlags.ringBell;
 
     if (bell) {
@@ -280,7 +307,7 @@
       papi_start_gc_count();
     }
 #endif
-
+#endif /* !house_HOST_OS */
 }
 
 /* -----------------------------------------------------------------------------
@@ -291,6 +318,7 @@
 stat_endGC (lnat alloc, lnat live, lnat copied, 
 	    lnat scavd_copied, lnat gen)
 {
+#ifndef house_HOST_OS
     if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {
 	Ticks time, etime, gc_time, gc_etime;
 	
@@ -356,6 +384,7 @@
       papi_start_mutator_count();
     }
 #endif
+#endif /* !house_HOST_OS */
 }
 
 /* -----------------------------------------------------------------------------
@@ -365,11 +394,13 @@
 void
 stat_startRP(void)
 {
+#ifndef house_HOST_OS
     Ticks user, elapsed;
     getProcessTimes( &user, &elapsed );
 
     RP_start_time = user;
     RPe_start_time = elapsed;
+#endif
 }
 #endif /* PROFILING */
 
@@ -490,6 +521,7 @@
 void
 stat_exit(int alloc)
 {
+#ifndef house_HOST_OS
     if (RtsFlags.GcFlags.giveStats != NO_GC_STATS) {
 
 	char temp[BIG_STRING_LEN];
@@ -647,6 +679,7 @@
     if (GC_coll_times)
       stgFree(GC_coll_times);
     GC_coll_times = NULL;
+#endif /* !house_HOST_OS */
 }
 
 /* -----------------------------------------------------------------------------
@@ -731,17 +764,21 @@
 static void
 statsFlush( void )
 {
+#ifndef house_HOST_OS
     FILE *sf = RtsFlags.GcFlags.statsFile;
     if (sf != NULL) {
 	fflush(sf);
     }
+#endif
 }
 
 static void
 statsClose( void )
 {
+#ifndef house_HOST_OS
     FILE *sf = RtsFlags.GcFlags.statsFile;
     if (sf != NULL) {
 	fclose(sf);
     }
+#endif
 }
