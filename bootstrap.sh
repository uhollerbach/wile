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

WHOME=`pwd`

# $HOME/tools/{include,lib} is where I have sqlite3 and gc installed;
# you will need to adjust this for your system

export WILE_INCLUDE_DIRECTORIES=.:$WHOME:$HOME/tools/include
export WILE_LINK_DIRECTORIES=.:$WHOME:$HOME/tools/lib
export WILE_LIBRARY_PATH=.:$WHOME:$WHOME/library

./build-stage123.sh
