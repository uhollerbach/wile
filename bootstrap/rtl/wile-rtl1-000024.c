// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
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

lval wile_parse_file(lptr* clos, lptr args, const char* loc)
{
    lval res;

    if (!IS_STRING(args) && !IS_FPORT(args) &&
	!IS_PPORT(args) && !IS_SOCKPORT(args)) {
	wile_exception("read-all", loc,
		       "expects one string or file/pipe/socket port argument");
    }

    lptr lp = NULL;
    wile_set_lisp_loc_file(IS_STRING(args) ? args->v.str : NULL);
    struct ulex_context* context =
	IS_STRING(args)
	? ulex_init(ulex_FILE, args->v.str)
	: ulex_init(ulex_STREAM, args->v.fp);

    if (context == NULL) {
	wile_exception("read-all", loc, "unable to set up lexer");
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

