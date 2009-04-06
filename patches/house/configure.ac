diff -u ghc-xen/configure.ac ghc-house/configure.ac
--- ghc-xen/configure.ac	2008-05-29 21:34:09.000000000 +0000
+++ ghc-house/configure.ac	2008-02-13 09:42:48.000000000 +0000
@@ -233,14 +233,14 @@
         HostVendor_CPP='unknown'
         HostOS_CPP='linux'
         ;;
-i[[3456]]86-*-xen)
-	HostPlatform=i386-unknown-xen # vaguely true
-	TargetPlatform=i386-unknown-xen
+i[[3456]]86-*-house)
+	HostPlatform=i386-unknown-house # vaguely true
+	TargetPlatform=i386-unknown-house
 	BuildPlatform=i386-unknown-linux
 	HostPlatform_CPP='i386_unknown_linux'
 	HostArch_CPP='i386'
 	HostVendor_CPP='unknown'
-	HostOS_CPP='xen'
+	HostOS_CPP='house'
 	;;
 i[[3456]]86-*-kfreebsd*-gnu)
         HostPlatform=i386-unknown-kfreebsdgnu # hack again
@@ -845,7 +845,7 @@
 if test x"$TargetPlatform" = x"i386-unknown-mingw32"; then
    GhcLibsWithUnix=NO
 else 
-  if test x"$TargetPlatform" = x"i386-unknown-xen"; then
+  if test x"$TargetPlatform" = x"i386-unknown-house"; then
      GhcLibsWithUnix=NO
   else
      GhcLibsWithUnix=YES
@@ -1000,14 +1000,14 @@
 AC_SYS_LARGEFILE
 
 dnl ** check for specific header (.h) files that we are interested in
-if test x"$TargetPlatform" = x"i386-unknown-xen"; then
+if test x"$TargetPlatform" = x"i386-unknown-house"; then
 AC_CHECK_HEADERS([bfd.h ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pwd.h sys/mman.h sys/resource.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h time.h utime.h windows.h winsock.h])
 else
 AC_CHECK_HEADERS([bfd.h ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pthread.h pwd.h signal.h sys/mman.h sys/resource.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h termios.h time.h utime.h windows.h winsock.h])
 fi
 
 
-if test x"$TargetPlatform" = x"i386-unknown-xen"; then
+if test x"$TargetPlatform" = x"i386-unknown-house"; then
 HaveReadlineReadlineH=NO
 HaveReadlineHistoyH=NO
 else
@@ -1112,7 +1112,7 @@
 AC_CHECK_FUNCS([getpagesize])
 
 dnl ** check whether this machine has gmp3 installed
-if test "$HostOS_CPP" = "xen"; then
+if test "$HostOS_CPP" = "house"; then
 HaveLibGmp=NO
 LibGmp=not-installed
 else
@@ -1175,7 +1175,7 @@
 
 dnl ** check whether we need -ldl to get dlopen()
 
-if test "$HostOS_CPP" = "xen"; then
+if test "$HostOS_CPP" = "house"; then
 HaveLibDL=NO
 else
 AC_CHECK_LIB(dl, dlopen,
@@ -1236,7 +1236,7 @@
     [AC_MSG_RESULT(no)])
 
 dnl ** check for librt
-if test x"$TargetPlatform" = x"i386-unknown-xen"; then
+if test x"$TargetPlatform" = x"i386-unknown-house"; then
 AC_CHECK_LIB(rt, clock_gettime)
 AC_CHECK_FUNCS(clock_gettime timer_create timer_settime)
 FP_CHECK_TIMER_CREATE
