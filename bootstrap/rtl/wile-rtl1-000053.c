// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_cfft_good_n(lptr* clos, lptr args, const char* loc)
{
    if (args[0].vt != LV_INT) {
	wile_exception("cfft-good-n?", loc, "expects an integer argument");
    }
    return LVI_BOOL(wilec_cfft_good_n(args[0].v.iv));
}

lval wile_cfft(lptr* clos, lptr args, const char* loc)
{
    if (args[0].vt != LV_INT || args[1].vt != LV_VECTOR) {
	wile_exception("vector-cfft!", loc,
		       "expects an integer transform direction and a vector of complex values");
    }

    lptr *arr;
    int si;
    size_t n = 0, i;
    lisp_cmplx_t *a1, *a2, *ap;

    wilec_cfft_init();

    if (args[0].v.iv > 0) {
	si = 1;
    } else if (args[0].v.iv < 0) {
	si = -1;
    } else {
	wile_exception("vector-cfft!", loc,
		       "transform direction was not specified");
    }

    n = args[1].v.vec.capa;
    if (!wilec_cfft_good_n(n)) {
	wile_exception("vector-cfft!", loc,
		       "%zu is not a multiple of (2,3,5,7,11)", n);
    }
    arr = args[1].v.vec.arr;
    a1 = LISP_ALLOC(lisp_cmplx_t, n);
    for (i = 0; i < n; ++i) {
	switch (arr[i]->vt) {
	case LV_INT:
	    a1[i] = CMPLX((lisp_real_t) arr[i]->v.iv, 0.0);
	    break;
	case LV_RAT:
	    a1[i] = CMPLX(LV_RAT2REAL(*(arr[i])), 0.0);
	    break;
	case LV_REAL:
	    a1[i] = CMPLX(arr[i]->v.rv, 0.0);
	    break;
	case LV_CMPLX:
	    a1[i] = arr[i]->v.cv;
	    break;
	default:
	    LISP_FREE(a1);
	    wile_exception("vector-cfft!", loc,
			   "input contains non-numeric value at index %zu", i);
	    break;
	}
    }
    a2 = LISP_ALLOC(lisp_cmplx_t, n);

    ap = wilec_cfft(si, n, n, a1, a2);

    for (i = 0; i < n; ++i) {
	arr[i]->vt = LV_CMPLX;
	arr[i]->v.cv = ap[i];
    }

    LISP_FREE(a1);
    LISP_FREE(a2);

    return args[1];
}

