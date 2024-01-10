bin_PROGRAMS = wile
wile_SOURCES = wilec.c
wile_CPPFLAGS = -I$(top_srcdir)/include -DWILE_CONFIG_FILE=$(sysconfdir)/wile-config.dat
wile_LDADD = ../libwrtl/libwrtl.a

sysconf_DATA = wile-config.dat

my_datadir = $(datadir)/wile
dist_my_data_DATA = arg-parse.scm deque.scm gthread.scm hash.scm monad-list.scm monad.scm queue.scm stack.scm stats.scm struct.scm test.scm

# filter out -g and -O[0123] flags if present; wile will handle those itself
# TODO: check on how this works out at some later point

wile-config.dat:	wile
	echo ';;; -*- mode: scheme; -*-' > wile-config.dat
	echo >> wile-config.dat
	echo "(c-compiler \"$(CC)\")" >> wile-config.dat
	echo "(c-compiler-flags \"$(CFLAGS)\")" |\
		sed -e 's/ *-g *//' |\
		sed -e 's/ *-O[0123] *//' >> wile-config.dat
	echo "(c-include-directories (\"$(includedir)\"))" >> wile-config.dat
	echo "(c-link-directories (\"$(libdir)\"))" >> wile-config.dat
	echo "(scheme-include-directories (\".\" \"$(datadir)/wile\"))" >> wile-config.dat
	./wile -write-config >> wile-config.dat
