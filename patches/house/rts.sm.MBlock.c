diff -u ghc-xen/rts/sm/MBlock.c ghc-house/rts/sm/MBlock.c
--- ghc-xen/rts/sm/MBlock.c	2007-12-10 18:11:32.000000000 +0000
+++ ghc-house/rts/sm/MBlock.c	2008-02-13 12:26:46.000000000 +0000
@@ -133,6 +133,35 @@
   return getMBlocks(1);
 }
 
+#if defined(house_HOST_OS)
+static void * next_mblock;
+static void * end_of_memory;
+void MBlock_init(void * heap, void * limit) {
+  memset(mblock_map, 0, MBLOCK_MAP_SIZE);
+  next_mblock = (void*)(((StgWord32)heap + MBLOCK_SIZE - 1) & ~(MBLOCK_SIZE - 1));
+  end_of_memory = limit;
+}
+
+void *
+getMBlocks(unsigned n) {
+  void * p = next_mblock;
+  unsigned i = 0;
+  next_mblock += MBLOCK_SIZE * n;
+  if (next_mblock > end_of_memory)
+    perror("Haskell heap exhausted\n");
+  mblocks_allocated += n;
+  for (i = 0; i < n; i++) {
+      markHeapAlloced(p + i * MBLOCK_SIZE);
+  }
+  return p;
+}
+
+void
+freeAllMBlocks(void)
+{
+  /* XXX Do something here */
+}
+
 /* -----------------------------------------------------------------------------
    The mmap() method
 
@@ -160,7 +189,7 @@
 
    -------------------------------------------------------------------------- */
 
-#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS)
+#elif !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS)
 
 // A wrapper around mmap(), to abstract away from OS differences in
 // the mmap() interface.
