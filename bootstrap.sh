#!/bin/sh

# Wile -- the extremely stable scheming genius compiler
# Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
# License: GPLv3 or later, see file 'LICENSE' for details

(cd bootstrap && ./boot1.sh)

# get the configuration

## apparently not every /bin/sh understands this?
## it fails on openbsd... so inline the contents
## source setup.env

export WILE_CONFIG="-DWILE_USES_SQLITE -DWILE_USES_LONG_INT -DWILE_USES_GC -DWILE_USES_DOUBLE"
export WILE_LINK_LIBRARIES=sqlite3:gc

WHOME=$HOME/github/wile

# $HOME/tools/{include,lib} is where I have sqlite3 and gc installed;
# you will need to adjust this for your system

export WILE_INCLUDE_DIRECTORIES=.:$WHOME:$HOME/tools/include
export WILE_LINK_DIRECTORIES=.:$WHOME:$HOME/tools/lib
export WILE_LIBRARY_PATH=.:$WHOME:$WHOME/library

echo "################################"
echo "build stage1"

rm -f wile libwrtl.a build-rtl

ln -s bootstrap/wilec ./wile
ln -s bootstrap/build-rtl ./

make realclean libwrtl.a
bootstrap/wilec -c -v wile-main.scm wilec1.c
ln -fs wilec1.c wilecxx.c
bootstrap/wilec -x -v wilecxx.c wilecxx
rm -f wilecxx.c
mv wilecxx wilec1

ln -fs ./wilec1 ./wile

echo "################################"
echo "build stage2"

rm -f build-rtl
make build-rtl
make realclean libwrtl.a libwrtl-dbg.a twp

./wilec1 -c -v wile-main.scm wilec2.c
ln -fs wilec2.c wilecxx.c
./wilec1 -x -v wilecxx.c wilecxx
rm -f wilecxx.c
mv wilecxx wilec2

ln -fs ./wilec2 ./wile

./twp wtest

# a little bit of cleanup, we know these will fail
rm -f test_2[26]-int.c

echo "################################"
echo diffs between wilec1.c and wilec2.c
diff wilec[12].c
echo "################################"
echo MD5sums of wilec1 and wilec2
md5sum wilec[12]
echo "################################"

echo still TODO: move everything into final locations:
echo wilec3 into some bin directory
echo libwrtl*.a and wrtl.sch into some lib directory
echo all the header files into some include directory
echo then set WILE_INCLUDE_DIRECTORIES and WILE_LINK_DIRECTORIES properly
