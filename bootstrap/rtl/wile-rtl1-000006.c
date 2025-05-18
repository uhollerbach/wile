// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_run_system_command(lval cmd, const char* loc)
{
    if (cmd.vt != LV_STRING) {
	wile_exception("run-command", loc, "got bad input type!");
    }
    int status = system(cmd.v.str);
    if (status < 0) {
	return LVI_BOOL(false);
    } else {
	if (WIFEXITED(status)) {
	    status = WEXITSTATUS(status);
	} else if (WIFSIGNALED(status)) {
	    status = -WTERMSIG(status);
	} else {
	    status = INT_MIN;
	}
	return LVI_INT(status);
    }
}

