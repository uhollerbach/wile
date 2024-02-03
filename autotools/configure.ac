AC_INIT([wile], [1.2.7], [uhollerbach@gmail.com], [],
    [https://github.com/uhollerbach/wile])

AM_INIT_AUTOMAKE
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_RANLIB

# check for Boehm garbage collector

AC_ARG_ENABLE([gc],
    [AS_HELP_STRING([--disable-gc],
	[disable gc @<:@default: no@:>@])],
    [use_gc=${enableval}], [use_gc=yes])

if test "x${use_gc}" = xyes; then
    have_gc=no
    AC_SEARCH_LIBS([GC_malloc], [gc], [have_gc=yes])

    if test "x${have_gc}" = xyes; then
	AC_CHECK_HEADERS([gc.h], [], [have_gc=no])
    fi

    if test "x${have_gc}" = xno; then
	AC_MSG_WARN([
    ------------------------------------------
    Unable to find gc on this system
    THIS WILL LIMIT YOU TO SMALL PROGRAMS!
    ------------------------------------------])
	use_gc=no
    fi
else
    AC_MSG_WARN([
    ------------------------------------------
    Skipping gc on this system
    THIS WILL LIMIT YOU TO SMALL PROGRAMS!
    ------------------------------------------])
    use_gc=no
fi

# check for sqlite3

AC_ARG_ENABLE([sqlite3],
    [AS_HELP_STRING([--disable-sqlite3],
	[disable sqlite3 @<:@default: no@:>@])],
    [use_sqlite3=${enableval}], [use_sqlite3=yes])

if test "x${use_sqlite3}" = xyes; then
    have_sqlite3=no
    AC_SEARCH_LIBS([sqlite3_libversion], [sqlite3], [have_sqlite3=yes])

    if test "x${have_sqlite3}" = xyes; then
	AC_CHECK_HEADERS([sqlite3.h], [], [have_sqlite3=no])
    fi

    if test "x${have_sqlite3}" = xno; then
	AC_MSG_WARN([
    ------------------------------------------
    Unable to find sqlite3 on this system
    ------------------------------------------])
	use_sqlite3=no
    fi
else
    AC_MSG_WARN([
    ------------------------------------------
    Skipping sqlite3 on this system
    ------------------------------------------])
    use_sqlite3=no
fi

# check for quadmath; use long double (which may not be different from
# double) if quadmath is not found or is deselected

AC_ARG_ENABLE([quadmath],
    [AS_HELP_STRING([--disable-quadmath],
	[disable quadmath @<:@default: no@:>@])],
    [use_quadmath=${enableval}], [use_quadmath=yes])

if test "x${use_quadmath}" = xyes; then
    have_quadmath=no
    AC_SEARCH_LIBS([quadmath_snprintf], [quadmath], [have_quadmath=yes])

    if test "x${have_quadmath}" = xyes; then
	AC_CHECK_HEADERS([quadmath.h], [], [have_quadmath=no])
    fi

    if test "x${have_quadmath}" = xno; then
	AC_MSG_WARN([
    ------------------------------------------
    Unable to find quadmath on this system
    ------------------------------------------])
	use_quadmath=no
    fi
else
    AC_MSG_WARN([
    ------------------------------------------
    Skipping quadmath on this system
    ------------------------------------------])
    use_quadmath=no
fi

# check for int128; use long int if int128 is not found or is deselected

AC_ARG_ENABLE([int128],
    [AS_HELP_STRING([--disable-int128],
	[disable int128 @<:@default: no@:>@])],
    [use_int128=${enableval}], [use_int128=yes])

if test "x${use_int128}" = xyes; then

# Check for int128 copied & modified from AX_HAVE_INT128 macro
# https://www.gnu.org/software/autoconf-archive/ax_int128.html
# Copyright 2019 Reuben Thomas <rrt@sc3d.org>

    AC_CACHE_CHECK([for __int128], [ax_cv_int128],
	[ax_cv_int128=
	AC_EGREP_CPP([le big int exists], [
#ifdef __SIZEOF_INT128__
le big int exists
#endif
], [ax_cv_int128=yes]) ])

    if test "x$ax_cv_int128" = xyes; then
	AC_DEFINE([HAVE_INT128], [1],
	    [Define to 1 if you have the type __int128.])
    else
	AC_MSG_WARN([
    ------------------------------------------
    Unable to find int128 on this system
    ------------------------------------------])
	use_int128=no
    fi
else
    AC_MSG_WARN([
    ------------------------------------------
    Skipping int128 on this system
    ------------------------------------------])
    use_int128=no
fi

# check for a couple of additional required libraries

AC_CHECK_LIB([pthread], [pthread_join])
AC_CHECK_LIB([m], [sqrt])

# write config.h

AC_CONFIG_HEADERS([config.h])

AC_CONFIG_FILES([Makefile
		 include/Makefile
		 libwrtl/Makefile
		 src/Makefile])

AC_OUTPUT
