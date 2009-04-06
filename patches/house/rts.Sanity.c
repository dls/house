diff -u ghc-xen/rts/Sanity.c ghc-house/rts/Sanity.c
--- ghc-xen/rts/Sanity.c	2008-05-29 21:34:10.000000000 +0000
+++ ghc-house/rts/Sanity.c	2008-05-15 06:29:34.000000000 +0000
@@ -324,6 +324,7 @@
 	    return sizeW_fromITBL(info);
 	}
 
+#ifndef house_HOST_OS
     case BCO: {
 	StgBCO *bco = (StgBCO *)p;
 	ASSERT(LOOKS_LIKE_CLOSURE_PTR(bco->instrs));
@@ -331,6 +332,7 @@
 	ASSERT(LOOKS_LIKE_CLOSURE_PTR(bco->ptrs));
 	return bco_sizeW(bco);
     }
+#endif
 
     case IND_STATIC: /* (1, 0) closure */
       ASSERT(LOOKS_LIKE_CLOSURE_PTR(((StgIndStatic*)p)->indirectee));
