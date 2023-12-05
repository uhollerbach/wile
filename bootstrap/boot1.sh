#!/bin/sh

# Wile -- the extremely stable scheming genius compiler
# Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
# License: GPLv3 or later, see file 'LICENSE' for details

# bootstrap the compiler and RTL

AR="ar rcs"

# minimal config to get things going; no garbage collector is
# bad long-term, but hopefully in the next stage we have one

WCONFIG="-DWILE_USES_LONG_INT -DWILE_USES_DOUBLE"

################ don't touch anything below here ###############

# remove -Werror as there are unused functions in wilec.c

CCOMMON="-ansi -std=c11 -Wall -Wstrict-prototypes -Wmissing-prototypes -Winline -Wpointer-arith -Wshadow -Wnested-externs -Wformat-security -Wunused -Wsign-compare -Wno-error=unused-parameter -Wno-error=unused-but-set-variable -D_DEFAULT_SOURCE -I. -I.. $WCONFIG"

CFOPT="-O3"

rm -f *.o libwrtl.a
echo '################################'
echo build stage0
echo '################################'
echo build library
$CC $CCOMMON $CFOPT -c ../wile-sql.c ../alloc.c ../print.c ../location.c \
    ../wile-parse.c ../wile-lex.c ../swll-cfft.c ../continuations.c \
    ../fsi_set.c ../nfa.c ../regex.c ../ulexlib.c ../sha256.c rtl/*.c
$AR libwrtl.a *.o
echo '################################'
echo compile main program to create wilec
$CC $CCOMMON $CFOPT -c wilec.c
$CC $CCOMMON $CFOPT -L. -o wilec wilec.o -lwrtl -lpthread -lm

echo '################################'
echo build auxiliary build-rtl program
$CC $CCOMMON $CFOPT -c build-rtl.c
$CC $CCOMMON $CFOPT -L. -o build-rtl build-rtl.o -lwrtl -lpthread -lm

#### clean up

rm -f *.o
echo '################################'
echo stage0 build complete!
echo '################################'
