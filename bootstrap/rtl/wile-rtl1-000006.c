// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval run_system_command(lval cmd, const char* fname, int lno)
{
    if (cmd.vt != LV_STRING) {
	wile_exception2("run-command", fname, lno, "got bad input type!");
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

