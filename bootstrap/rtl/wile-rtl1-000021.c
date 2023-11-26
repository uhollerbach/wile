// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


#define AB_ATYPE	char
#define AB_STYPE	ab_char
#define AB_STATIC
#define AB_ALLOC	LISP_ALLOC
#define AB_REALLOC	LISP_REALLOC
#define AB_FREE		LISP_FREE

#include "array_builder.h"

lval wile_read_line(lptr*, lptr args, const char* loc)
{
    FILE* fp;

    if (args[0].vt == LV_FILE_PORT ||
	args[0].vt == LV_PIPE_PORT ||
	args[0].vt == LV_SOCK_PORT) {
	fp = args[0].v.fp;
    } else {
	wile_exception("read-line", loc, "expects one port argument");
    }

    ab_char mya_str = ab_char_setup(64);
    char buf[128], *bp;
    while (fgets(buf, sizeof(buf), fp)) {
	bp = buf;
	while (*bp) {
	    if (*bp == '\r' || *bp == '\n') {
		break;
	    }
	    ab_char_append(&mya_str, *bp++);
	}
	if (*bp) {
	    break;
	}
    }

    lval res;
    if (ab_char_get_size(&mya_str) == 0 && feof(fp)) {
	res = LVI_BOOL(false);
    } else {
	ab_char_append(&mya_str, '\0');
	res = LVI_STRING(ab_char_get_array(&mya_str));
    }
    ab_char_destroy(&mya_str);
    return res;
}

