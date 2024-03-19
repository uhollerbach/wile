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

#include "wile-rtl1.h"


#define TYPE_CHECK(vector, name)					\
    do {								\
	for (i = 0; i < vector->v.vec.capa; ++i) {			\
	    if (vector->v.vec.arr[i]) {					\
		switch (vector->v.vec.arr[i]->vt) {			\
		case LV_INT:						\
		    break;						\
		case LV_RAT:						\
		    if (ty < 1) {					\
			ty = 1;						\
		    }							\
		    break;						\
		case LV_REAL:						\
		    if (ty < 2) {					\
			ty = 2;						\
		    }							\
		    break;						\
		case LV_CMPLX:						\
		    if (ty < 3) {					\
			ty = 3;						\
		    }							\
		    break;						\
		default:						\
		    wile_exception(fname, loc,				\
				   name " contains non-numeric values");\
		}							\
	    } else {							\
		wile_exception(fname, loc, name " contains nil values");\
	    }								\
	}								\
    } while (0)

lval wile_gauss_elim(lval* a, lval* rhs, lval* tr, const char* loc)
{
    static const char* fname = "gauss-elim";

    if (a->vt != LV_VECTOR || rhs->vt != LV_VECTOR || tr->vt != LV_BOOL) {
	wile_exception(fname, loc,
		       "expects two vectors of numbers and a boolean");
    }

    size_t i, n = rhs->v.vec.capa;

    if (a->v.vec.capa != n*n) {
	wile_exception(fname, loc, "array and rhs sizes are incompatible");
    }

    int ty = 0;
    TYPE_CHECK(a, "matrix");
    TYPE_CHECK(rhs, "rhs vector");

    // allocate direct homogeneous arrays for a and rhs
    // copy values over, promoting if needed
    // call the gauss_elim_foo routines
    // put the result back into a proper scheme vector and return it
    
    if (ty == 3) {
	// complex path

	for (i = 0; i < a->v.vec.capa; ++i) {
	    if (a->v.vec.arr[i]->vt == LV_INT) {
		*(a->v.vec.arr[i]) = LVI_CMPLX2((lisp_real_t) (a->v.vec.arr[i]->v.iv), 0.0);
	    } else if (a->v.vec.arr[i]->vt == LV_RAT) {
		*(a->v.vec.arr[i]) = LVI_CMPLX2(LV_RAT2REAL(*(a->v.vec.arr[i])), 0.0);
	    } else if (a->v.vec.arr[i]->vt == LV_REAL) {
		*(a->v.vec.arr[i]) = LVI_CMPLX2(a->v.vec.arr[i]->v.rv, 0.0);
	    }
	}

    } else {
	// real path

	for (i = 0; i < a->v.vec.capa; ++i) {
	    if (a->v.vec.arr[i]->vt == LV_INT) {
		*(a->v.vec.arr[i]) = LVI_REAL((lisp_real_t) (a->v.vec.arr[i]->v.iv));
	    } else if (a->v.vec.arr[i]->vt == LV_RAT) {
		*(a->v.vec.arr[i]) = LVI_REAL(LV_RAT2REAL(*(a->v.vec.arr[i])));
	    }
	}
    }
    return LVI_BOOL(true);
}
