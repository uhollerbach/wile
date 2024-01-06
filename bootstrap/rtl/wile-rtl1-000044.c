// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_setlinebuffering(lptr* clos, lptr args, const char* loc)
{
    if (args[0].vt == LV_FILE_PORT ||
	args[0].vt == LV_PIPE_PORT ||
	args[0].vt == LV_SOCK_PORT) {
	return LVI_BOOL(setvbuf(args[0].v.fp, NULL, _IOLBF, 0) == 0);
    } else if (args[0].vt == LV_STR_PORT) {
	return LVI_BOOL(true);
#ifdef WILE_USES_SQLITE
    } else if (args[0].vt == LV_SQLITE_PORT) {
	return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
    } else {
	wile_exception("set-line-buffering!", loc,
		       "expects one port argument");
    }
}

