// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


// This uses Daan Leijen's ic_readline library, which can be found at
// https://github.com/daanx/isocline

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

#include "isocline.h"

static bool init_read_line_interactive = true;

lval wile_read_line_interactive(lptr* clos, lptr args, const char* loc)
{
    char* str;

    if (init_read_line_interactive) {
	str = getenv("WILE_READLINE_HISTORY");
	if (str != NULL && strcmp(str, "") == 0) {
	    str = NULL;
	}
	ic_set_history(str, -1);	// -1 = default entries (200)
	init_read_line_interactive = false;
    }

    if (args[0].vt != LV_STRING) {
	wile_exception("read-line-interactive", loc,
		       "expects a prompt string");
    }

    str = ic_readline(args[0].v.str);
    if (str) {
	lval ret = LVI_STRING(str);
	free(str);
	return ret;
    } else {
	return LVI_BOOL(false);
    }
}

