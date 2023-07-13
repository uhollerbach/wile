// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


bool do_eqv(lptr arg1, lptr arg2)
{
    size_t i;

 top:
    if (arg1 == NULL && arg2 == NULL) {
	return true;
    } else if (arg1 == NULL || arg2 == NULL) {
	return false;
    } else if (arg1->vt != arg2->vt) {
	return false;
    } else {
	switch (arg1->vt) {
	case LV_NIL:		return true;
	case LV_SYMBOL:		return (strcmp(arg1->v.str, arg2->v.str) == 0);
	case LV_BOOL:		return (arg1->v.bv == arg2->v.bv);
	case LV_CHAR:		return (arg1->v.chr == arg2->v.chr);
	case LV_STRING:		return (strcmp(arg1->v.str, arg2->v.str) == 0);
	case LV_INT:		return (arg1->v.iv == arg2->v.iv);

	case LV_RAT:
	    return (arg1->v.irv.num == arg2->v.irv.num &&
		    arg1->v.irv.den == arg2->v.irv.den);

	case LV_REAL:		return (arg1->v.rv == arg2->v.rv ||
					(ISNAN(arg1->v.rv) &&
					 ISNAN(arg2->v.rv)));

	case LV_CMPLX:
	    return (arg1->v.cv == arg2->v.cv ||
		    ((ISNAN(CREAL(arg1->v.cv)) || ISNAN(CIMAG(arg1->v.cv))) &&
		     (ISNAN(CREAL(arg2->v.cv)) || ISNAN(CIMAG(arg2->v.cv)))));

	case LV_PAIR:
	    if (!do_eqv(CAR(arg1), CAR(arg2))) {
		return false;
	    }
	    arg1 = CDR(arg1);
	    arg2 = CDR(arg2);
	    goto top;

	case LV_FILE_PORT:
	case LV_PIPE_PORT:
	case LV_SOCK_PORT:	return (arg1->v.fp == arg2->v.fp);

#ifdef WILE_USES_SQLITE
	case LV_SQLITE_PORT:	return (arg1->v.sqlite_conn ==
					arg2->v.sqlite_conn);
	case LV_SQLITE_STMT:	return (arg1->v.sqlite_stmt ==
					arg2->v.sqlite_stmt);
#endif // WILE_USES_SQLITE

	case LV_VECTOR:
	    if (arg1->v.vec.capa != arg2->v.vec.capa) {
		return false;
	    }
	    for (i = 0; i < arg1->v.vec.capa; ++i) {
		if (!do_eqv(arg1->v.vec.arr[i], arg2->v.vec.arr[i])) {
		    return false;
		}
	    }
	    return true;

	case LV_BVECTOR:
	    if (arg1->v.bvec.capa != arg2->v.bvec.capa) {
		return false;
	    }
	    for (i = 0; i < arg1->v.bvec.capa; ++i) {
		if (arg1->v.bvec.arr[i] != arg2->v.bvec.arr[i]) {
		    return false;
		}
	    }
	    return true;

	// TODO: implement these
//////	case LV_STR_PORT:
//	case LV_PROMISE:
//	case LV_LAMBDA:

	default:		return false;
	}
    }
}

