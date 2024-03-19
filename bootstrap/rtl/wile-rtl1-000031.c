// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX		1024
#endif // HOST_NAME_MAX

lval wile_gethostname(lptr* clos, lptr args, const char* loc)
{
    char buf[HOST_NAME_MAX+1];
    if (gethostname(buf, sizeof(buf)) < 0) {
	return LVI_BOOL(false);
    } else {
	return LVI_STRING(buf);
    }
}

