diff -u ghc-xen/libraries/Makefile ghc-house/libraries/Makefile
--- ghc-xen/libraries/Makefile	2008-05-29 21:34:09.000000000 +0000
+++ ghc-house/libraries/Makefile	2008-02-13 12:35:42.000000000 +0000
@@ -46,10 +46,10 @@
 ifeq "$(Windows)" "YES"
 SUBDIRS += $(wildcard Win32)
 endif
-SUBDIRS += pretty hpc template-haskell Cabal random haskell98
+SUBDIRS += pretty hpc template-haskell random haskell98
 
-ifneq "$(HOSTPLATFORM)" "i386-unknown-xen"
-SUBDIRS += readline filepath directory process
+ifneq "$(HOSTPLATFORM)" "i386-unknown-house"
+SUBDIRS += Cabal readline filepath directory process
 endif
 
 # Set GhcBootLibs=YES from the command line to work with just the libraries
@@ -61,7 +61,7 @@
 SUBDIRS += $(wildcard parsec)
 SUBDIRS += $(wildcard haskell-src)
 SUBDIRS += $(wildcard html)
-ifneq "$(HOSTPLATFORM)" "i386-unknown-xen"
+ifneq "$(HOSTPLATFORM)" "i386-unknown-house"
 SUBDIRS += $(wildcard network)
 SUBDIRS += $(wildcard QuickCheck)
 SUBDIRS += $(wildcard HUnit)
@@ -69,7 +69,7 @@
 SUBDIRS += $(wildcard mtl)
 SUBDIRS += $(wildcard fgl)
 SUBDIRS += $(wildcard time)
-ifneq "$(HOSTPLATFORM)" "i386-unknown-xen"
+ifneq "$(HOSTPLATFORM)" "i386-unknown-house"
 SUBDIRS += $(wildcard OpenGL)
 SUBDIRS += $(wildcard GLUT)
 SUBDIRS += $(wildcard OpenAL)
@@ -77,7 +77,7 @@
 endif
 SUBDIRS += $(wildcard stm)
 SUBDIRS += $(wildcard xhtml)
-ifneq "$(HOSTPLATFORM)" "i386-unknown-xen"
+ifneq "$(HOSTPLATFORM)" "i386-unknown-house"
 SUBDIRS += $(wildcard cgi)
 ifeq "$(GhcLibsWithObjectIO)" "YES"
 SUBDIRS += $(wildcard ObjectIO)
