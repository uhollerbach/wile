#!/bin/sh

# Wile -- the extremely stable scheming genius compiler
# Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
# License: GPLv3 or later, see file 'LICENSE' for details

echo "################################"
echo "build stage1"

rm -f wile libwrtl.a build-rtl

ln -s bootstrap/wilec ./wile
ln -s bootstrap/build-rtl ./

make realclean libwrtl.a
bootstrap/wilec -c -v -rm-uv wile-main.scm wilec1.c
ln -fs wilec1.c wilecxx.c
bootstrap/wilec -x -v wilecxx.c wilecxx
rm -f wilecxx.c
mv wilecxx wilec1

ln -fs ./wilec1 ./wile
mv libwrtl.a libwrtl.stage1.a
ln -s  libwrtl.stage1.a libwrtl.a

echo "################################"
echo "build stage2"

rm -f build-rtl
make build-rtl
make realclean libwrtl.a

./wilec1 -c -v wile-main.scm wilec2.c
ln -fs wilec2.c wilecxx.c
./wilec1 -x -v wilecxx.c wilecxx
rm -f wilecxx.c
mv wilecxx wilec2

ln -fs ./wilec2 ./wile
mv libwrtl.a libwrtl.stage2.a
ln -s  libwrtl.stage2.a libwrtl.a

echo "################################"
echo "build stage3"

rm -f build-rtl
make build-rtl
make realclean libwrtl.a twp

./wilec2 -c -v wile-main.scm wilec3.c
ln -fs wilec3.c wilecxx.c
./wilec2 -x -v wilecxx.c wilecxx
rm -f wilecxx.c
mv wilecxx wilec3

ln -fs ./wilec3 ./wile
mv libwrtl.a libwrtl.stage3.a
ln -s libwrtl.stage3.a libwrtl.a

./twp wtest

echo "################################"
echo diffs between wilec2.c and wilec3.c
diff wilec[23].c
echo "################################"
echo MD5sums of wilec?
md5sum wilec?
echo "################################"
echo MD5sums of libwrtl.stage?.a
md5sum libwrtl.stage?.a
echo "################################"

echo still TODO: move everything into final locations:
echo wilec3 into some bin directory
echo libwrtl*.a and wrtl.sch into some lib directory
echo all the header files into some include directory
echo then set WILE_INCLUDE_DIRECTORIES and WILE_LINK_DIRECTORIES properly
