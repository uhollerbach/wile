// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_temp_file(lptr*, lptr args)
{
    char *template, *tp;
    size_t tlen;
    int i, nt;
    FILE* fp;

    if (args->vt != LV_STRING) {
	wile_exception("open-temporary-file", "expects one string argument");
    }
    tlen = strlen(args->v.str);
    template = LISP_ALLOC(char, tlen + 8);
    LISP_ASSERT(template != NULL);
    strcpy(template, args->v.str);
    tp = template + tlen;
    nt = (tlen < 6) ? tlen : 6;
    for (i = 0; i < nt; ++i) {
	if (*(--tp) != 'X') {
	    break;
	}
    }
    tp = template + tlen;
    while (i++ < 6) {
	*tp++ = 'X';
    }
    *tp++ = '\0';
    nt = mkstemp(template);
    if (nt < 0) {
	LISP_FREE_STR(template);
	wile_exception("open-temporary-file", "could not create temporary file");
    }
    lval vs[2];
    vs[1] = LVI_STRING(template);
    LISP_FREE_STR(template);
    fp = fdopen(nt, "w+");
    if (fp == NULL) {
	wile_exception("open-temporary-file", "could not create temporary file");
    }
    vs[0] = LVI_FPORT(fp);
    return gen_list(2, vs, NULL);
}

