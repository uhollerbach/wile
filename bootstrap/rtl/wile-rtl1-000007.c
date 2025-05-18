// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_run_pipe_command(lval cmd, const char* rw, const char* loc)
{
    if (cmd.vt != LV_STRING ||
	(strcmp(rw, "r") != 0 && strcmp(rw, "w") != 0)) {
	wile_exception("run-read/write-command", loc,
		       "got bad input type!");
    }
    if (strcmp(rw, "r") != 0 && strcmp(rw, "w") != 0) {
	wile_exception("run-read/write-command", loc,
		       "got bad read/write mode %s", rw);
    }
    FILE* fp = popen(cmd.v.str, rw);
    if (fp) {
	return LVI_PPORT(fp);
    } else {
	return LVI_BOOL(false);
    }
}

