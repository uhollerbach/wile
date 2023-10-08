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

WRSRC = wile-sql.c alloc.c print.c location.c wile-parse.c wile-lex.c \
	swll-cfft.c continuations.c fsi_set.c nfa.c regex.c ulexlib.c

# in an emergency, it also can work to build wile-rtl2.[co] as one unified
# object file; that does mean that every link pulls in everything

libwrtl.a:	$(WRSRC) wile-rtl1.c wile-rtl2.scm math-funcs.c
	wile -v wile-rtl2.scm wile-rtl2.c
	build-rtl libwrtl.a $(WRSRC) -s wile-rtl1.c wile-rtl2.scm math-funcs.c
	nm -a libwrtl.a | grep wile_config

libwrtl-dbg.a:	$(WRSRC) wile-rtl1.c wile-rtl2.scm math-funcs.c
	wile -v wile-rtl2.scm wile-rtl2.c
	build-rtl -g libwrtl-dbg.a $(WRSRC) -s \
		wile-rtl1.c wile-rtl2.scm math-funcs.c
	nm -a libwrtl-dbg.a | grep wile_config

wilec:	wile-main.scm wile-comp.scm wile-prims.scm libwrtl.a
	wile -x -v wile-main.scm wilec

twp:	test-wile-progs.scm
	wile -x -v test-wile-progs.scm twp

build-rtl:	build-rtl.scm
	wile -x -v build-rtl.scm build-rtl

test:	libwrtl.a twp
	test-wile.scm
	twp wtest

# don't delete build-rtl, otherwise there is a bootstrap problem!

realclean:	clean
	rm -f libwrtl.a libwrtl-dbg.a wrtl.sch wile-rtl2.h twp

clean:	semiclean
	rm -f wile-rtl2.c wtest/test_*.tst
	rm -rf bld-rtl-dir

semiclean:
	rm -f *.o *.gcno *.gcda *.gcov test.txt test-cont.txt \
	coyote coywolf coy.log woycolf wile-out.c wile-profile.out

# show all the files in the repo
tracked:
	git ls-tree -r HEAD --name-only

.PHONY:	realclean clean semiclean test

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

wile-rtl2.c:	wile-rtl2.scm
	./wile -v wile-rtl2.scm wile-rtl2.c

wile-rtl2.h:	wile-rtl2.c

wrtl.sch:	wile-rtl2.c

## wile-parse.c wile-parse.h:	wile.yucc
## 	yucc wile.yucc

## wile-lex.c wile-lex.h:	wile.ulex
## 	ulex wile.ulex

.c.o:
	wile -o -v $< $@
