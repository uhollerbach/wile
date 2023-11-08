// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_string_reverse(lptr*, lptr args)
{
    size_t i, j;
    char c;
    if (args[0].vt != LV_STRING) {
	wile_exception("string-reverse", "expects a string argument");
    }
    lval ret = LVI_STRING(args[0].v.str);
    i = 0;
    j = strlen(ret.v.str);
    while (i < j) {
	c = ret.v.str[--j];
	ret.v.str[j] = ret.v.str[i];
	ret.v.str[i++] = c;
    }
    return ret;
}

