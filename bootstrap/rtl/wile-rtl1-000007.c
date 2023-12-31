// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


#include <sys/types.h>
#include <sys/stat.h>

lval wile_temp_file(lptr* clos, lptr args, const char* loc)
{
    char *template, *tp;
    size_t tlen;
    int i, nt;
    FILE* fp;

    if (args->vt != LV_STRING) {
	wile_exception("open-temporary-file", loc,
		       "expects one string argument");
    }
    tlen = strlen(args->v.str);
    template = LISP_ALLOC(char, tlen + 8);
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
    mode_t mask = umask(0);
    umask(077);
    nt = mkstemp(template);
    umask(mask);
    if (nt < 0) {
	LISP_FREE(template);
	wile_exception("open-temporary-file", loc,
		       "could not create temporary file");
    }
    lval vs[2];
    vs[1] = LVI_STRING(template);
    LISP_FREE(template);
    fp = fdopen(nt, "w+");
    if (fp == NULL) {
	wile_exception("open-temporary-file", loc,
		       "could not create temporary file");
    }
    vs[0] = LVI_FPORT(fp);
    return wile_gen_list(2, vs, NULL);
}

