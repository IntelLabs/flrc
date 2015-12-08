#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.60])
AC_INIT([hrc], [0.1], [BUG-REPORT-ADDRESS])

AC_CHECK_HEADERS([assert.h float.h immintrin.h limits.h locale.h malloc.h math.h nmmintrin.h signal.h stdarg.h stdio.h stdlib.h string.h time.h sys/time.h sys/timeb.h termios.h])

PKG_CHECK_MODULES([IFLC_LIB], [iflc-lib])

PLATFORM_CPPFLAGS=
AC_CANONICAL_HOST
case "$host" in
    *-mingw*|*-*-cygwin*)
        ;;
    *-*-linux*)
        PLATFORM_CPPFLAGS="-DPLSR_LINUX"
        ;;
    *)
        AC_MSG_ERROR([Unsupported host: $host])
        ;;
esac

AC_SUBST([PLATFORM_CPPFLAGS])

AM_SUBST_NOTMAKE
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
