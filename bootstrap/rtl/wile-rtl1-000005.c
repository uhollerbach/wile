// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval get_gensym(void)
{
    static unsigned long count = 0;
    char buf[64];
    lval res;

    snprintf(buf, sizeof(buf), " symbol.%lu", ++count);
    res.vt = LV_SYMBOL;
    res.v.str = LISP_STRDUP(buf);
    return res;
}

