#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <complex.h>
#include <math.h>
#include <time.h>
#include <float.h>
#include <sys/types.h>
#include <unistd.h>

#include "wile.h"
#include "alloc.h"
#include "lib-macros.h"

#include "wile-rtl1.h"


// compute a^b, (hopefully) taking into account most of the special cases

lval wile_expt(lval* a, lval* b)
{
    lval res;

    switch (TYPE_COMBO(a->vt, b->vt)) {

    case TYPE_COMBO(LV_INT, LV_INT):
	if (b->v.iv >= 0) {
	    res = LVI_INT(powi(a->v.iv, b->v.iv));
	} else {
	    res = LVI_RAT(1, powi(a->v.iv, -b->v.iv));
	}
	break;

    case TYPE_COMBO(LV_RAT, LV_INT):
	if (b->v.iv >= 0) {
	    res = LVI_RAT(powi(a->v.irv.num, b->v.iv),
			  powi(a->v.irv.den, b->v.iv));
	} else {
	    res = LVI_RAT(powi(a->v.irv.den, -b->v.iv),
			  powi(a->v.irv.num, -b->v.iv));
	}
	break;

    case TYPE_COMBO(LV_REAL, LV_INT):
	res = LVI_REAL(POW(a->v.rv, b->v.iv));
	break;

    case TYPE_COMBO(LV_CMPLX, LV_INT):
	res = LVI_CMPLX1(CPOW(a->v.cv, b->v.iv));
	break;

    case TYPE_COMBO(LV_INT, LV_RAT):
	res = LVI_CMPLX1(CPOW(a->v.iv, LV_RAT2REAL(*b)));
	break;
    case TYPE_COMBO(LV_RAT, LV_RAT):
	res = LVI_CMPLX1(CPOW(LV_RAT2REAL(*a), LV_RAT2REAL(*b)));
	break;
    case TYPE_COMBO(LV_REAL, LV_RAT):
	res = LVI_CMPLX1(CPOW(a->v.rv, LV_RAT2REAL(*b)));
	break;
    case TYPE_COMBO(LV_CMPLX, LV_RAT):
	res = LVI_CMPLX1(CPOW(a->v.cv, LV_RAT2REAL(*b)));
	break;

    case TYPE_COMBO(LV_INT, LV_REAL):
	res = LVI_CMPLX1(CPOW(a->v.iv, b->v.rv));
	break;
    case TYPE_COMBO(LV_RAT, LV_REAL):
	res = LVI_CMPLX1(CPOW(LV_RAT2REAL(*a), b->v.rv));
	break;
    case TYPE_COMBO(LV_REAL, LV_REAL):
	res = LVI_CMPLX1(CPOW(a->v.rv, b->v.rv));
	break;
    case TYPE_COMBO(LV_CMPLX, LV_REAL):
	res = LVI_CMPLX1(CPOW(a->v.cv, b->v.rv));
	break;

    case TYPE_COMBO(LV_INT, LV_CMPLX):
	res = LVI_CMPLX1(CPOW(a->v.iv, b->v.cv));
	break;
    case TYPE_COMBO(LV_RAT, LV_CMPLX):
	res = LVI_CMPLX1(CPOW(LV_RAT2REAL(*a), b->v.cv));
	break;
    case TYPE_COMBO(LV_REAL, LV_CMPLX):
	res = LVI_CMPLX1(CPOW(a->v.rv, b->v.cv));
	break;
    case TYPE_COMBO(LV_CMPLX, LV_CMPLX):
	res = LVI_CMPLX1(CPOW(a->v.cv, b->v.cv));
	break;

    default:
	WILE_EX("expt", "expects two numeric arguments");
	break;
    }

    if (res.vt == LV_CMPLX && CIMAG(res.v.cv) == 0.0) {
	res = LVI_REAL(CREAL(res.v.cv));
    }
    return res;
}
