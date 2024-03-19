// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_setfilepos3(lptr* clos, lptr args, const char* loc)
{
    if (args[0].vt != LV_FILE_PORT ||
	args[1].vt != LV_INT ||
	args[2].vt != LV_SYMBOL) {
	wile_exception("set-file-position", loc,
		       "expects a file port, an offset, and a location symbol");
    }
    int whence;
    if (strcmp(args[2].v.str, "start") == 0) {
	whence = SEEK_SET;
    } else if (strcmp(args[2].v.str, "cur") == 0) {
	whence = SEEK_CUR;
    } else if (strcmp(args[2].v.str, "end") == 0) {
	whence = SEEK_END;
    } else {
	wile_exception("set-file-position", loc,
		       "got an unknown location symbol");
    }
    return LVI_BOOL(fseek(args[0].v.fp, args[1].v.iv, whence) == 0);
}

