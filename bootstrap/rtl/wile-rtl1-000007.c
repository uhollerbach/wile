// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_run_pipe_command(lval cmd, const char* rw,
			   const char* fname, int lno)
{
    if (cmd.vt != LV_STRING ||
	(strcmp(rw, "r") != 0 && strcmp(rw, "w") != 0)) {
	wile_exception2("run-read/write-command", fname, lno,
			"got bad input type!");
    }
    if (strcmp(rw, "r") != 0 && strcmp(rw, "w") != 0) {
	wile_exception2("run-read/write-command", fname, lno,
			"got bad read/write mode %s", rw);
    }
    FILE* fp = popen(cmd.v.str, rw);
    if (fp) {
	return LVI_PPORT(fp);
    } else {
	return LVI_BOOL(false);
    }
}

