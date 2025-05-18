// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


// need to protect this as if it were a header file because we need it
// in several places; if this file gets split, it needs to be included
// more than once

#ifndef WILE_NEEDS_ULEX
#define WILE_NEEDS_ULEX
#include "wile-parse.h"
#include "wile-lex.h"

void set_start_state(struct ulex_context* context);
#endif // WILE_NEEDS_ULEX

lval wile_parse_string(lptr* clos, lptr args, const char* loc)
{
    lval res;

    if (!IS_STRING(args)) {
	wile_exception("parse-string", loc, "expects one string argument");
    }

    lptr lp = NULL;
    wile_set_lisp_loc_file(NULL);
    struct ulex_context* context = ulex_init(ulex_TEXT, args[0].v.str);

    if (context == NULL) {
	wile_exception("parse-string", loc, "unable to set up lexer");
    } else {
	set_start_state(context);
	if (wile_parse(context, &lp, 1)) {
	    res = LVI_BOOL(false);
	} else {
	    res = *lp;
	}
    }
    ulex_cleanup(context);
    return res;
}

