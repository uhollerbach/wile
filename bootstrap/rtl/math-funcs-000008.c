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


#ifdef TYPE_CHECK
#undef TYPE_CHECK
#endif

#define TYPE_CHECK(vector, name)					\
    do {								\
	for (k = 0; k < vector.v.vec.capa; ++k) {			\
	    if (vector.v.vec.arr[k]) {					\
		switch (vector.v.vec.arr[k]->vt) {			\
		case LV_INT:						\
		    ty = (ty > 1) ? ty : 1;				\
		    break;						\
		case LV_RAT:						\
		    ty = (ty > 2) ? ty : 2;				\
		    break;						\
		case LV_REAL:						\
		    ty = (ty > 3) ? ty : 3;				\
		    break;						\
		    break;						\
		case LV_CMPLX:						\
		    ty = 4;						\
		    break;						\
		default:						\
		    wile_exception(fname, loc,				\
				   name " contains non-numeric values");\
		}							\
	    }								\
	}								\
    } while (0)

#define COPY_INT(inmat,outmat,msize)					\
    do {								\
	for (k = 0; k < msize; ++k) {					\
	    lptr val = inmat.v.vec.arr[k];				\
	    if (val) {							\
		if (val->vt == LV_INT) {				\
		    outmat[k] = val->v.iv;				\
		} else {						\
		    wile_exception(fname, loc, "internal error 1");	\
		}							\
	    } else {							\
		outmat[k] = 0;						\
	    }								\
	}								\
    } while (0)

#define COPY_RAT(inmat,outmat,msize)					\
    do {								\
	for (k = 0; k < msize; ++k) {					\
	    lptr val = inmat.v.vec.arr[k];				\
	    if (val) {							\
		if (val->vt == LV_INT) {				\
		    outmat[k].num = val->v.iv;				\
		    outmat[k].den = 1;					\
		} else if (val->vt == LV_RAT) {				\
		    outmat[k] = val->v.irv;				\
		} else {						\
		    wile_exception(fname, loc, "internal error 2");	\
		}							\
	    } else {							\
		outmat[k].num = 0;					\
		outmat[k].den = 1;					\
	    }								\
	}								\
    } while (0)

#define COPY_REAL(inmat,outmat,msize)					\
    do {								\
	for (k = 0; k < msize; ++k) {					\
	    lptr val = inmat.v.vec.arr[k];				\
	    if (val) {							\
		if (val->vt == LV_INT) {				\
		    outmat[k] = (lisp_real_t) val->v.iv;		\
		} else if (val->vt == LV_RAT) {				\
		    outmat[k] = LV_RAT2REAL(*(val));			\
		} else if (val->vt == LV_REAL) {			\
		    outmat[k] = val->v.rv;				\
		} else {						\
		    wile_exception(fname, loc, "internal error 3");	\
		}							\
	    } else {							\
		outmat[k] = 0.0;					\
	    }								\
	}								\
    } while (0)

#define COPY_CMPLX(inmat,outmat,msize)					\
    do {								\
	for (k = 0; k < msize; ++k) {					\
	    lptr val = inmat.v.vec.arr[k];				\
	    if (val) {							\
		if (val->vt == LV_INT) {				\
		    outmat[k] = CMPLX((lisp_real_t) val->v.iv, 0);	\
		} else if (val->vt == LV_RAT) {				\
		    outmat[k] = CMPLX(LV_RAT2REAL(*(val)), 0);		\
		} else if (val->vt == LV_REAL) {			\
		    outmat[k] = CMPLX(val->v.rv, 0);			\
		} else if (val->vt == LV_CMPLX) {			\
		    outmat[k] = val->v.cv;				\
		} else {						\
		    wile_exception(fname, loc, "internal error 4");	\
		}							\
	    } else {							\
		outmat[k] = CMPLX(0.0, 0.0);				\
	    }								\
	}								\
    } while (0)

lval wile_mat_mat_mul(lval mat1, lval nr1, lval nc1, lval tr1,
		      lval mat2, lval nr2, lval nc2, lval tr2,
		      lval tr3, const char* loc)
{
    static const char* fname = "matrix-matrix-multiply";
    size_t cnr1, cnc1, cnr2, cnc2, sr1, sc1, sr2, sc2, sr3, sc3,
	ir, ic, k;

    if (mat1.vt != LV_VECTOR || nr1.vt != LV_INT ||
	nc1.vt != LV_INT || tr1.vt != LV_BOOL ||
	mat2.vt != LV_VECTOR || nr2.vt != LV_INT ||
	nc2.vt != LV_INT || tr2.vt != LV_BOOL ||
	tr3.vt != LV_BOOL ||
	nr1.v.iv <= 0 || nc1.v.iv <= 0 ||
	nr2.v.iv <= 0 || nc2.v.iv <= 0) {
	wile_exception(fname, loc, "got bad arguments");
    }

    if (nc1.v.iv != nr2.v.iv) {
	wile_exception(fname, loc, "got incompatible matrices");
    }

    int ty = 0;
    TYPE_CHECK(mat1, "matrix 1");
    TYPE_CHECK(mat2, "matrix 2");

    cnr1 = nr1.v.iv;
    cnc1 = nc1.v.iv;
    cnr2 = nr2.v.iv;
    cnc2 = nc2.v.iv;

    if (tr1.v.bv) {
	sr1 = 1;
	sc1 = cnr1;
    } else {
	sr1 = cnc1;
	sc1 = 1;
    }
    if (tr2.v.bv) {
	sr2 = 1;
	sc2 = cnr2;
    } else {
	sr2 = cnc2;
	sc2 = 1;
    }
    if (tr3.v.bv) {
	sr3 = 1;
	sc3 = cnr1;
    } else {
	sr3 = cnc2;
	sc3 = 1;
    }

    lval mat3;

    mat3.vt = LV_VECTOR;
    mat3.origin = tr3.origin;
    mat3.v.vec.capa = cnr1*cnc2;
    mat3.v.vec.arr = LISP_ALLOC(lptr, mat3.v.vec.capa);

    if (ty <= 1) {
	lisp_int_t *imat1, *imat2;

	imat1 = LISP_ALLOC(lisp_int_t, cnr1*cnc1);
	imat2 = LISP_ALLOC(lisp_int_t, cnr2*cnc2);
	COPY_INT(mat1, imat1, cnr1*cnc1);
	COPY_INT(mat2, imat2, cnr2*cnc2);
	for (ir = 0; ir < cnr1; ++ir) {
	    for (ic = 0; ic < cnc2; ++ic) {
		lisp_int_t isum = 0;
		for (k = 0; k < cnc1; ++k) {
		    isum += imat1[sr1*ir + sc1*k] * imat2[sr2*k + sc2*ic];
		}
		mat3.v.vec.arr[sr3*ir + sc3*ic] = new_int(isum);
	    }
	}
	LISP_FREE(imat1);
	LISP_FREE(imat2);
    } else if (ty == 2) {
	lisp_rat_t *rmat1, *rmat2;

	rmat1 = LISP_ALLOC(lisp_rat_t, cnr1*cnc1);
	rmat2 = LISP_ALLOC(lisp_rat_t, cnr2*cnc2);
	COPY_RAT(mat1, rmat1, cnr1*cnc1);
	COPY_RAT(mat2, rmat2, cnr2*cnc2);
	for (ir = 0; ir < cnr1; ++ir) {
	    for (ic = 0; ic < cnc2; ++ic) {
		lisp_int_t ns, ds, nt, dt, g;
		ns = 0;
		ds = 1;
		for (k = 0; k < cnc1; ++k) {
		    nt = rmat1[sr1*ir + sc1*k].num * rmat2[sr2*k + sc2*ic].num;
		    dt = rmat1[sr1*ir + sc1*k].den * rmat2[sr2*k + sc2*ic].den;
		    g = lgcd(nt, dt);
		    nt /= g;
		    dt /= g;
		    ns = ns*dt + nt*ds;
		    ds = ds*dt;
		    g = lgcd(ns, ds);
		    ns /= g;
		    ds /= g;
		}
		mat3.v.vec.arr[sr3*ir + sc3*ic] = new_rat2(ns, ds);
	    }
	}
	LISP_FREE(rmat1);
	LISP_FREE(rmat2);
    } else if (ty == 3) {
	lisp_real_t *rmat1, *rmat2;

	rmat1 = LISP_ALLOC(lisp_real_t, cnr1*cnc1);
	rmat2 = LISP_ALLOC(lisp_real_t, cnr2*cnc2);
	COPY_REAL(mat1, rmat1, cnr1*cnc1);
	COPY_REAL(mat2, rmat2, cnr2*cnc2);
	for (ir = 0; ir < cnr1; ++ir) {
	    for (ic = 0; ic < cnc2; ++ic) {
		lisp_real_t rsum = 0.0;
		for (k = 0; k < cnc1; ++k) {
		    rsum += rmat1[sr1*ir + sc1*k] * rmat2[sr2*k + sc2*ic];
		}
		mat3.v.vec.arr[sr3*ir + sc3*ic] = new_real(rsum);
	    }
	}
	LISP_FREE(rmat1);
	LISP_FREE(rmat2);
    } else if (ty == 4) {
	lisp_cmplx_t *cmat1, *cmat2;

	cmat1 = LISP_ALLOC(lisp_cmplx_t, cnr1*cnc1);
	cmat2 = LISP_ALLOC(lisp_cmplx_t, cnr2*cnc2);
	COPY_CMPLX(mat1, cmat1, cnr1*cnc1);
	COPY_CMPLX(mat2, cmat2, cnr2*cnc2);
	for (ir = 0; ir < cnr1; ++ir) {
	    for (ic = 0; ic < cnc2; ++ic) {
		lisp_cmplx_t csum = CMPLX(0.0, 0.0);
		for (k = 0; k < cnc1; ++k) {
		    csum += cmat1[sr1*ir + sc1*k] * cmat2[sr2*k + sc2*ic];
		}
		mat3.v.vec.arr[sr3*ir + sc3*ic] = new_cmplx1(csum);
	    }
	}
	LISP_FREE(cmat1);
	LISP_FREE(cmat2);
    } else {
	wile_exception(fname, loc, "internal error 5");
    }

    return mat3;
}

