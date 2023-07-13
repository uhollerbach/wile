// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_getdomainname(lptr*, lptr)
{
    char buf[HOST_NAME_MAX+1];
    if (getdomainname(buf, sizeof(buf)) < 0) {
	// TODO: DO_ERRNO();
	return LVI_BOOL(false);
    } else {
	return LVI_STRING(buf);
    }
}

