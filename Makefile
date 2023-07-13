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

WROBJS = wile-sql.o alloc.o print.o location.o wile-parse.o wile-lex.o \
	swll-cfft.o continuations.o fsi_set.o nfa.o regex.o ulexlib.o

# in an emergency, it also can work to build wile-rtl2.[co] as one unified
# object file; that does mean that every link pulls in everything

libwrtl.a:	$(WROBJS) wile-rtl1.c wile-rtl2.h math-funcs.c
	rm -rf build-rtl
	mkdir build-rtl
	buspl wile-rtl1.c build-rtl
	buspl wile-rtl2.scm build-rtl
	buspl math-funcs.c build-rtl
	rm -f libwrtl.a
	ar rcs $@ $(WROBJS) build-rtl/*.o
	echo "sanity check: numbers of c and o files must match"
	ls build-rtl/*.c | wc -l
	ls build-rtl/*.o | wc -l

twp:	test-wile-progs.scm
	wile -x -v test-wile-progs.scm twp

buspl:	build-split.scm
	wile -x -v build-split.scm buspl

test:	libwrtl.a twp
	test-wile.scm
	twp wtest

# don't delete buspl, otherwise there is a bootstrap problem!

realclean:	clean
	rm -f libwrtl.a wrtl.sch wile-rtl2.h twp

clean:	semiclean
	rm -f wile-rtl2.c wtest/test_*.tst
	rm -rf build-rtl

semiclean:
	rm -f *.o *.gcno *.gcda *.gcov test.txt test-cont.txt \
	coyote coywolf coy.log woycolf wile-out.c wile-profile.out

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
