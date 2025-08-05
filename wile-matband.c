// Wile -- the extremely stable scheming genius compiler
// Copyright 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <complex.h>
#include <math.h>
#include <float.h>
#include <sys/types.h>
#include <unistd.h>

#include "wile-rtl1.h"

static lisp_rat_t* promote_rat(lisp_rat_t* vout, lptr* vin,
			       size_t* index, size_t n);
static lisp_real_t* promote_real(lisp_real_t* vout, lptr* vin,
				 size_t* index, size_t n);
static lisp_cmplx_t* promote_cmplx(lisp_cmplx_t* vout, lptr* vin,
				   size_t* index, size_t n);

#define DETECT_TYPE(vv, nn, tt, fname)				\
    do {							\
	tt = 0;							\
	for (i = 0; i < nn; ++i) {				\
	    if (vv.v.vec.arr[i]) {				\
		switch (vv.v.vec.arr[i]->vt) {			\
		case LV_NIL:					\
		    break;					\
		case LV_INT:					\
		    if (tt < 1) {				\
			tt = 1;					\
		    }						\
		    break;					\
		case LV_RAT:					\
		    if (tt < 2) {				\
			tt = 2;					\
		    }						\
		    break;					\
		case LV_REAL:					\
		    if (tt < 3) {				\
			tt = 3;					\
		    }						\
		    break;					\
		case LV_CMPLX:					\
		    if (tt < 4) {				\
			tt = 4;					\
		    }						\
		    break;					\
		default:					\
		    wile_exception(fname, loc,			\
				   "expects a vector of numeric values"); \
		}						\
	    }							\
	}							\
    } while (0)

lval wile_mat_banded_solve(lptr* clos, lptr args, const char* loc)
{
    if (args[0].vt != LV_VECTOR || args[1].vt != LV_INT ||
	args[2].vt != LV_INT || args[3].vt != LV_INT ||
	args[4].vt != LV_VECTOR) {
	wile_exception("matrix-banded-solve", loc,
		       "expects a vector of numbers, three integers, and another vector of numbers");
    }
    if (args[1].v.iv < 1 ||
	args[2].v.iv < 0 || args[2].v.iv >= args[1].v.iv ||
	args[3].v.iv < 0 || args[3].v.iv >= args[1].v.iv) {
	wile_exception("matrix-banded-solve", loc,
		       "bad banded-matrix sizes %d, %d, %d",
		       args[1].v.iv, args[2].v.iv, args[3].v.iv);
    }

    size_t nrc, kl, ku, klu;

    nrc = args[1].v.iv;
    kl = args[2].v.iv;
    ku = args[3].v.iv;
    klu = kl + ku;

    return LVI_BOOL(true);
}
