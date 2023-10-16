// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_char2string(lptr*, lptr args)
{
    lval lv, ac;
    lisp_int_t i, len;

    if (args == NULL || args->vt == LV_NIL) {
	return LVI_STRING("");
    }
    lv = wile_list_length(NULL, args);
    if (lv.vt != LV_INT || lv.v.iv < 0) {
	wile_exception("char->string", "got a bad list length!?!");
    }
    len = lv.v.iv;

    if (len == 1) {
	ac = CAR(args) ? *(CAR(args)) : LVI_NIL();
	if (ac.vt == LV_PAIR || ac.vt == LV_NIL) {
	    args = CAR(args);
	    if (args == NULL) {
		len = 0;
	    } else {
		lv = wile_list_length(NULL, args);
		if (lv.vt != LV_INT || lv.v.iv < 0) {
		    wile_exception("char->string", "got a bad list length!?!");
		}
		len = lv.v.iv;
	    }
	}
    }

    LISP_ASSERT(len >= 0);

    lv.vt = LV_STRING;
    lv.v.str = LISP_ALLOC(char, len + 1);
    LISP_ASSERT(lv.v.str != NULL);

    for (i = 0; i < len; ++i) {
	LISP_ASSERT(args != NULL && args->vt == LV_PAIR);
	ac = CAR(args) ? *(CAR(args)) : LVI_NIL();
	if (ac.vt != LV_CHAR) {
	    wile_exception("char->string", "got a non-character argument");
	}
	lv.v.str[i] = ac.v.chr;
	args = CDR(args);
    }
    LISP_ASSERT(args == NULL || args->vt == LV_NIL);
    lv.v.str[len] = '\0';
    return lv;
}

