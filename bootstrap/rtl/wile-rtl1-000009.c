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

lval wile_string2num(lval str, const char* loc)
{
    lval val1, val2;
    unsigned char* text;

    wile_set_lisp_loc_file(NULL);
    struct ulex_context* context = ulex_init(ulex_TEXT, str.v.str);
    if (context == NULL) {
	wile_exception("string->number", loc, "was unable to set up lexer");
    }
    set_start_state(context);
    (void) wile_lex(context, &val1, NULL, &text);

    if (IS_NUMERIC(&val1) && wile_lex(context, &val2, NULL, &text) == 0) {
	if (val1.vt == LV_INT ||
	    val1.vt == LV_RAT ||
	    val1.vt == LV_REAL || val1.vt == LV_CMPLX) {
	    return val1;
	} else {
	    wile_exception("string->number", loc,
			   "got bad type %d!", val1.vt);
	}
	ulex_cleanup(context);
    } else {
	ulex_cleanup(context);
	wile_exception("string->number", loc,
		       "got bad input string '%s'", str.v.str);
    }
}

