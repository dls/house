diff -u ghc-xen/rts/Makefile ghc-house/rts/Makefile
--- ghc-xen/rts/Makefile	2008-05-29 21:34:10.000000000 +0000
+++ ghc-house/rts/Makefile	2008-02-13 08:01:00.000000000 +0000
@@ -63,8 +63,9 @@
 ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
 ALL_DIRS += win32
 else
-ifeq "$(HOSTPLATFORM)" "i386-unknown-xen"
-ALL_DIRS += xen xen/libc xen/libc/math/i387 xen/libc/math/c xen/libc/math
+ifeq "$(HOSTPLATFORM)" "i386-unknown-house"
+#ALL_DIRS += xen xen/libc xen/libc/math/i387 xen/libc/math/c xen/libc/math
+ALL_DIRS += house
 else
 ALL_DIRS += posix
 endif
@@ -86,7 +87,7 @@
 EXCLUDED_SRCS += RtsDllMain.c
 endif
 
-ifeq "$(HOSTPLATFORM)" "i386-unknown-xen"
+ifeq "$(HOSTPLATFORM)" "i386-unknown-house"
 EXCLUDED_SRCS += Linker.c Hpc.c
 endif
 
@@ -151,9 +152,9 @@
 SRC_CC_OPTS += $(GhcRtsCcOpts)
 SRC_HC_OPTS += $(GhcRtsHcOpts) -package-name rts
 
-ifeq "$(HOSTPLATFORM)" "i386-unknown-xen"
-SRC_CC_OPTS += -Ixen/include -Ixen/include/sys
-endif
+#ifeq "$(HOSTPLATFORM)" "i386-unknown-xen"
+#SRC_CC_OPTS += -Ixen/include -Ixen/include/sys
+#endif
 
 ifneq "$(GhcWithSMP)" "YES"
 SRC_CC_OPTS += -DNOSMP
@@ -285,7 +286,7 @@
 # a superset of the dependencies.  To do this properly, we should generate
 # a different set of dependencies for each way.  Further hack: PROFILING and
 # TICKY_TICKY can't be used together, so we omit TICKY_TICKY for now.
-ifeq "$(HOSTPLATFORM)" "i386-unknown-xen"
+ifeq "$(HOSTPLATFORM)" "i386-unknown-house"
 SRC_MKDEPENDC_OPTS += -DPROFILING -DDEBUG
 else
 SRC_MKDEPENDC_OPTS += -DPROFILING -DTHREADED_RTS -DDEBUG
