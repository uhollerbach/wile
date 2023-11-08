// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_flushport(lptr*, lptr args)
{
    if (args[0].vt == LV_FILE_PORT ||
	args[0].vt == LV_PIPE_PORT ||
	args[0].vt == LV_SOCK_PORT) {
	return LVI_BOOL(fflush(args[0].v.fp) == 0);
    } else if (args[0].vt == LV_STR_PORT) {
	return LVI_BOOL(true);
#ifdef WILE_USES_SQLITE
    } else if (args[0].vt == LV_SQLITE_PORT) {
	// TODO: issue some kind of commit command?
	return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
    } else {
	wile_exception("flush-port", "expects one port argument");
    }
}

