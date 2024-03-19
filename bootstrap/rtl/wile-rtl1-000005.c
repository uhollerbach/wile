// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_get_gensym(lisp_loc_t origin)
{
    static unsigned long count = 0;
    char buf[64];
    lval ret;

    snprintf(buf, sizeof(buf), " symbol.%lu", ++count);
    ret.vt = LV_SYMBOL;
    ret.origin = origin;
    ret.v.str = LISP_STRDUP(buf);
    return ret;
}

