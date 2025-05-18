// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_waitpid(int pid, int opts)
{
    int status;

    pid = waitpid(pid, &status, opts);
    if (pid < 0) {
	return LVI_BOOL(false);
    } else {
	lval vs[2];
	vs[0] = LVI_INT(pid);
	vs[1] = LVI_INT(status);
	return wile_gen_list(2, vs, NULL);
    }
}

