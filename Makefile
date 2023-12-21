# Wile -- the extremely stable scheming genius compiler
# Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
# License: GPLv3 or later, see file 'LICENSE' for details

SHELL = /bin/sh

## -DWILE_USES_RC4_RAND

CC = gcc
#### CC = clang

CFDEF =	-O3 -ansi -std=c11 -Wall -Werror -Wstrict-prototypes \
	-Wmissing-prototypes -Winline -Wpointer-arith -Wshadow \
	-Wnested-externs -Wformat-security -Wunused -Wsign-compare \
	-Wno-error=unused-parameter -Wno-error=unused-but-set-variable \
	-D_DEFAULT_SOURCE -I.

## CFDEF += -Wextra

## CGCOV =	-fprofile-arcs -ftest-coverage
CGCOV =

CFLAGS = $(CFDEF) $(CGCOV) $(WILE_CONFIG)

ASRC =	wile-lex.[ch] wile-parse.[ch] wile-parse.txt
HDRS =	wile.h alloc.h wile-lex.h wile-parse.h lib-macros.h

WRSRC1 = wile-sql.c alloc.c print.c location.c wile-parse.c wile-lex.c \
	swll-cfft.c continuations.c fsi_set.c nfa.c regex.c ulexlib.c \
	sha256.c

WRSRC2 = wile-rtl1.c wile-rtl2.scm math-funcs.c

wrtl.sch:	wile-rtl2.scm
	wile -v -c wile-rtl2.scm wile-rtl2.c

# in an emergency, it also can work to build wile-rtl2.[co] as one unified
# object file; that does mean that every link pulls in everything

libwrtl.a:	wrtl.sch $(WRSRC1) $(WRSRC2)
	rm -rf bld-rtl-dir
	build-rtl libwrtl.a $(WRSRC1) -s $(WRSRC2)
	nm -a libwrtl.a | grep wile_config

libwrtl-dbg.a:	wrtl.sch $(WRSRC1) $(WRSRC2)
	rm -rf bld-rtl-dir
	build-rtl -g libwrtl-dbg.a $(WRSRC1) -s $(WRSRC2)
	nm -a libwrtl-dbg.a | grep wile_config

boot-files:	wrtl.sch $(WRSRC1) $(WRSRC2)
	rm -rf bld-rtl-dir
	build-rtl libfake.a $(WRSRC1) -s $(WRSRC2) -k
	rm -f libfake.a bld-rtl-dir/*.o bld-rtl-dir/*.scm bld-rtl-dir/wrtl.sch
	cp bld-rtl-dir/*.c bootstrap/rtl/
	cp wrtl.sch wile-rtl2.h bootstrap/
	rm -rf bld-rtl-dir
	wile -c wile-main.scm bootstrap/wilec.c

wilec:	wile-main.scm wile-comp.scm wile-prims.scm libwrtl.a
	wile -x -v wile-main.scm wilec

repl:	repl.scm libwrtl.a
	wile -x -v repl.scm repl

repl-dbg:	repl.scm libwrtl-dbg.a
	wile -x -v -g repl.scm repl-dbg

twp:	test-wile-progs.scm
	wile -x -v test-wile-progs.scm twp

build-rtl:	build-rtl.scm
	wile -x -v build-rtl.scm build-rtl

build-stages:	build-stages.scm
	wile -x -v build-stages.scm build-stages

test:	libwrtl.a twp
	test-wile.scm
	twp wtest

tarball:
	tar cvf dist.tar `git ls-tree -r HEAD --name-only`
	gzip -9 dist.tar

# don't delete build-rtl, otherwise there is a bootstrap problem!

realclean:	clean
	rm -f libwrtl.a libwrtl-dbg.a wrtl.sch wile-rtl2.h twp repl repl-dbg

clean:	semiclean
	rm -f wile-rtl2.c wtest/test_*.tst
	rm -rf bld-rtl-dir

semiclean:
	rm -f *.o *.gcno *.gcda *.gcov test.txt test-cont.txt \
	coyote coywolf coy.log woycolf wile-out.c wile-profile.out

.PHONY:	realclean clean semiclean test tarball

# dependencies

wile-parse.o:	wile-parse.c $(HDRS)

wile-lex.o:	wile-lex.c $(HDRS)

print.o:	print.c $(HDRS)

location.o:	location.c $(HDRS)

fsi_set.o:	fsi_set.c

nfa.o:		nfa.c

regex.o:	regex.c

ulexlib.o:	ulexlib.c

continuations.o:	continuations.c $(HDRS) wile-rtl1.h

wile-rtl1.o:	wile-rtl1.c $(HDRS) wile-rtl1.h wile-rtl2.h

wile-sql.o:	wile-sql.c $(HDRS) wile-rtl1.h wile-rtl2.h

wile-rtl2.o:	wile-rtl2.c $(HDRS)

wile-rtl2.h:	wile-rtl2.c

## wile-parse.c wile-parse.h:	wile.yucc
## 	yucc wile.yucc

## wile-lex.c wile-lex.h:	wile.ulex
## 	ulex wile.ulex

.c.o:
	wile -o -v $< $@
