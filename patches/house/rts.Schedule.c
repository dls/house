diff -u ghc-xen/rts/Schedule.c ghc-house/rts/Schedule.c
--- ghc-xen/rts/Schedule.c	2008-05-29 21:34:10.000000000 +0000
+++ ghc-house/rts/Schedule.c	2008-05-15 06:33:59.000000000 +0000
@@ -3173,7 +3173,8 @@
 	next = tso->global_link;
 	tso->global_link = all_threads;
 	all_threads = tso;
-	debugTrace(DEBUG_sched, "resurrecting thread %lu", (unsigned long)tso->id);
+	//debugTrace(DEBUG_sched, "resurrecting thread %lu", (unsigned long)tso->id);
+	debugBelch("resurrecting thread %lu", (unsigned long)tso->id);
 	
 	// Wake up the thread on the Capability it was last on
 	cap = tso->cap;
