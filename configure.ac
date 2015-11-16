#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.60])
AC_INIT([ihc], [0.1], [BUG-REPORT-ADDRESS])
AM_INIT_AUTOMAKE([0.1 foreign])

AC_CHECK_HEADERS([assert.h float.h immintrin.h limits.h locale.h malloc.h math.h nmmintrin.h signal.h stdarg.h stdio.h stdlib.h string.h time.h sys/time.h sys/timeb.h termios.h])

PKG_CHECK_MODULES([IFLC_LIB], [iflc-lib])

AM_CONDITIONAL([WINDOWS], [test "x$OS" = "xWindows_NT"])
AM_CONDITIONAL([FAST], [test "x$fast" = "xy"])

AC_CONFIG_FILES([Makefile runtime/Makefile])
AC_OUTPUT
