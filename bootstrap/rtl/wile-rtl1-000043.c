// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_closeport(lptr* clos, lptr args, const char* loc)
{
    if (args[0].vt == LV_FILE_PORT ||
	args[0].vt == LV_SOCK_PORT) {
	return LVI_BOOL(fclose(args[0].v.fp) == 0);
    } else if (args[0].vt == LV_PIPE_PORT) {
	return LVI_BOOL(pclose(args[0].v.fp) == 0);
    } else if (args[0].vt == LV_STR_PORT) {
	return LVI_BOOL(false);
#ifdef WILE_USES_SQLITE
    } else if (args[0].vt == LV_SQLITE_PORT) {
	return LVI_BOOL(sqlite3_close_v2(args[0].v.sqlite_conn) == SQLITE_OK);
#endif // WILE_USES_SQLITE
    } else {
	wile_exception("close-port", loc, "expects one port argument");
    }
}
	   
