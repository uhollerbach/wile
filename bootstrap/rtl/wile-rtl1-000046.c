// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_setfilepos2(lptr* clos, lptr args, const char* loc)
{
    if (args[0].vt != LV_FILE_PORT ||
	args[1].vt != LV_INT) {
	wile_exception("set-file-position", loc,
		       "expects a file port and an offset");
    }
    return LVI_BOOL(fseek(args[0].v.fp, args[1].v.iv, SEEK_SET) == 0);
}

