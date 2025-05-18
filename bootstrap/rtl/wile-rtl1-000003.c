// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


extern const unsigned int wile_nnames;
extern const struct wile_name_map wile_names[];

lval wile_translate_fn_name(const char* c_fn, lisp_loc_t origin)
{
    unsigned int i;
    lval ret;
    char buf[16];	// space for 12 digits... should be enough

    // remove the ".constprop.N" that gcc adds in some cases
    for (i = 0; i < sizeof(buf); ++i) {
	if (c_fn[i] == '\0' || c_fn[i] == '.') {
	    buf[i] = '\0';
	    break;
	}
	buf[i] = c_fn[i];
    }
    buf[sizeof(buf)-1] = '\0';

    for (i = 0; i < wile_nnames; ++i) {
	if (strcmp(buf, wile_names[i].c_name) == 0) {
	    ret.vt = LV_STRING;
	    ret.origin = origin;
	    ret.v.str = LISP_STRDUP(wile_names[i].s_name);
	    return ret;
	}
    }
    ret.vt = LV_NIL;
    return ret;
}

