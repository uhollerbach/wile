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

static bool do_init = true;
static lval sym_rat, sym_real, sym_cmplx;

static void do_matlu_init(void)
{
    if (do_init) {
	sym_rat = LVI_SYMBOL(" mat-lu-rat ");
	sym_real = LVI_SYMBOL(" mat-lu-real ");
	sym_cmplx = LVI_SYMBOL(" mat-lu-cmplx ");
	do_init = false;
    }
}

static lisp_rat_t* promote_rat(lisp_rat_t* vout, lptr* vin,
			       size_t* index, size_t n);
static lisp_real_t* promote_real(lisp_real_t* vout, lptr* vin,
				 size_t* index, size_t n);
static lisp_cmplx_t* promote_cmplx(lisp_cmplx_t* vout, lptr* vin,
				   size_t* index, size_t n);

static lval lud_rat(size_t n, lisp_vector_t* mat, bool transpose);
static lval lud_real(size_t n, lisp_vector_t* mat, bool transpose);
static lval lud_cmplx(size_t n, lisp_vector_t* mat, bool transpose);

static void lus_rat_rat(lisp_rat_t* mat, size_t* index,
			lptr* rhs, size_t n, lptr* ret);
static void lus_rat_real(lisp_rat_t* mat, size_t* index,
			 lptr* rhs, size_t n, lptr* ret);
static void lus_rat_cmplx(lisp_rat_t* mat, size_t* index,
			  lptr* rhs, size_t n, lptr* ret);

static void lus_real_real(lisp_real_t* mat, size_t* index,
			  lptr* rhs, size_t n, lptr* ret);
static void lus_real_cmplx(lisp_real_t* mat, size_t* index,
			   lptr* rhs, size_t n, lptr* ret);
static void lus_cmplx_cmplx(lisp_cmplx_t* mat, size_t* index,
			    lptr* rhs, size_t n, lptr* ret);

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

// return a 5-vector containing: #(type-symbol n LU-matrix index s)

lval wile_mat_lud(lval lmat, lval transpose, bool allow_rat, const char* loc)
{
    do_matlu_init();

    if (lmat.vt != LV_VECTOR || transpose.vt != LV_BOOL) {
	wile_exception("matrix-lu-decompose", loc,
		       "expects a vector of numbers and a boolean");
    }

    size_t n, n2, i;

    n = floor(0.5 + sqrt(lmat.v.vec.capa));
    n2 = n*n;
    if (n2 != lmat.v.vec.capa) {
	wile_exception("matrix-lu-decompose", loc, "expects a square matrix");
    }

    int type;
    DETECT_TYPE(lmat, n2, type, "matrix-lu-decompose");

    switch (type) {
    case 0:
	// a matrix of all () is automatically singular
	return LVI_BOOL(false);
    case 1:
    case 2:
	if (allow_rat) {
	    return lud_rat(n, &(lmat.v.vec), transpose.v.bv);
	}
	// otherwise fall-through desired
    case 3:
	return lud_real(n, &(lmat.v.vec), transpose.v.bv);
    case 4:
	return lud_cmplx(n, &(lmat.v.vec), transpose.v.bv);
    default:
	wile_exception("matrix-lu-decompose", loc,
		       "internal error, should never happen!");
    }
}

// expect a 5-vector containing: #(type-symbol n LU-matrix index s)

lval wile_mat_lus(lptr* clos, lptr args, const char* loc)
{
    do_matlu_init();

    if (args[0].vt != LV_VECTOR ||
	args[1].vt != LV_VECTOR ||
	args[0].v.vec.capa != 5 ||
	args[0].v.vec.arr[0]->vt != LV_SYMBOL ||
	(strcmp(args[0].v.vec.arr[0]->v.str, sym_rat.v.str) != 0 &&
	 strcmp(args[0].v.vec.arr[0]->v.str, sym_real.v.str) != 0 &&
	 strcmp(args[0].v.vec.arr[0]->v.str, sym_cmplx.v.str) != 0) ||
	args[0].v.vec.arr[1]->vt != LV_INT ||
	args[0].v.vec.arr[1]->v.iv <= 0) {
	wile_exception("matrix-lu-solve", loc,
		       "expects an LU-decompose object and a vector");
    }

    size_t i, n = args[0].v.vec.arr[1]->v.iv;
    if (args[1].v.vec.capa != n) {
	wile_exception("matrix-lu-solve", loc,
		       "matrix and vector sizes are different");
    }

    int mtype;
    if (strcmp(args[0].v.vec.arr[0]->v.str, sym_rat.v.str) == 0) {
	mtype = 2;
    } else if (strcmp(args[0].v.vec.arr[0]->v.str, sym_real.v.str) == 0) {
	mtype = 3;
    } else if (strcmp(args[0].v.vec.arr[0]->v.str, sym_cmplx.v.str) == 0) {
	mtype = 4;
    } else {
	wile_exception("matrix-lu-solve", loc,
		       "internal error, should never happen!");
    }

    int rtype;
    DETECT_TYPE(args[1], n, rtype, "matrix-lu-solve");

    lval ret;
    ret.vt = LV_VECTOR;
    ret.origin = 0;		// TODO: better than this!
    ret.v.vec.capa = n;
    ret.v.vec.arr = LISP_ALLOC(lptr, n);
    if (rtype == 0) {		// if the input is all 0, so is the output
	for (i = 0; i < n; ++i) {
	    ret.v.vec.arr[i] = new_int(0);
	}
	return ret;
    }

    if (rtype < mtype) {
	rtype = mtype;
    }

#define MRC(m,r)	(r + 2*m)

    switch (MRC(mtype, rtype)) {
    case MRC(2,2):
	lus_rat_rat((lisp_rat_t*) args[0].v.vec.arr[2]->v.bvec.arr,
		    (size_t*) args[0].v.vec.arr[3]->v.bvec.arr,
		    args[1].v.vec.arr, n, ret.v.vec.arr);
	break;
    case MRC(2,3):
	lus_rat_real((lisp_rat_t*) args[0].v.vec.arr[2]->v.bvec.arr,
		    (size_t*) args[0].v.vec.arr[3]->v.bvec.arr,
		    args[1].v.vec.arr, n, ret.v.vec.arr);
	break;
    case MRC(2,4):
	lus_rat_cmplx((lisp_rat_t*) args[0].v.vec.arr[2]->v.bvec.arr,
		      (size_t*) args[0].v.vec.arr[3]->v.bvec.arr,
		      args[1].v.vec.arr, n, ret.v.vec.arr);
	break;
    case MRC(3,3):
	lus_real_real((lisp_real_t*) args[0].v.vec.arr[2]->v.bvec.arr,
		      (size_t*) args[0].v.vec.arr[3]->v.bvec.arr,
		      args[1].v.vec.arr, n, ret.v.vec.arr);
	break;
    case MRC(3,4):
	lus_real_cmplx((lisp_real_t*) args[0].v.vec.arr[2]->v.bvec.arr,
		       (size_t*) args[0].v.vec.arr[3]->v.bvec.arr,
		       args[1].v.vec.arr, n, ret.v.vec.arr);
	break;
    case MRC(4,4):
	lus_cmplx_cmplx((lisp_cmplx_t*) args[0].v.vec.arr[2]->v.bvec.arr,
			(size_t*) args[0].v.vec.arr[3]->v.bvec.arr,
			args[1].v.vec.arr, n, ret.v.vec.arr);
	break;
    default:
	wile_exception("matrix-lu-solve", loc,
		       "internal error, should never happen!");
    }
    return ret;
}

static lval mk_vec(lptr symp, size_t n, size_t isize)
{
    lval ret;

    ret.vt = LV_VECTOR;
    ret.origin = 0;		// TODO: better than this!
    ret.v.vec.capa = 5;
    ret.v.vec.arr = LISP_ALLOC(lptr, 5);

    ret.v.vec.arr[0] = symp;
    ret.v.vec.arr[1] = new_int(n);
    ret.v.vec.arr[2] = new_bvec(n*n*isize);
    ret.v.vec.arr[3] = new_bvec(n*sizeof(size_t));
    ret.v.vec.arr[4] = new_int(0);

    return ret;
}

static lisp_rat_t* promote_rat(lisp_rat_t* vout, lptr* vin,
			       size_t* index, size_t n)
{
    if (vout == NULL) {
	vout = LISP_ALLOC(lisp_rat_t, n);
    }

    size_t i;
    for (i = 0; i < n; ++i) {
	if (vin[i]) {
	    switch (vin[i]->vt) {
	    case LV_NIL:
		vout[i].num = 0;
		vout[i].den = 1;
		break;
	    case LV_INT:
		vout[i].num = vin[i]->v.iv;
		vout[i].den = 1;
		break;
	    case LV_RAT:
		vout[i] = vin[i]->v.irv;
		break;
	    default:
		wile_exception("matrix-lu-solve", "promote-rat",
			       "internal error! should never happen");
	    }
	} else {
	    vout[i].num = 0;
	    vout[i].den = 1;
	}
    }

    if (index) {
	for (i = 0; i < n; ++i) {
	    size_t j = *index++;
	    if (j != i) {
		lisp_rat_t temp = vout[j];
		vout[j] = vout[i];
		vout[i] = temp;
	    }
	}
    }

    return vout;
}

static lisp_real_t* promote_real(lisp_real_t* vout, lptr* vin,
				 size_t* index, size_t n)
{
    if (vout == NULL) {
	vout = LISP_ALLOC(lisp_real_t, n);
    }

    size_t i;
    for (i = 0; i < n; ++i) {
	if (vin[i]) {
	    switch (vin[i]->vt) {
	    case LV_NIL:
		vout[i] = 0.0;
		break;
	    case LV_INT:
		vout[i] = (lisp_real_t) vin[i]->v.iv;
		break;
	    case LV_RAT:
		vout[i] = LV_RAT2REAL(*vin[i]);
		break;
	    case LV_REAL:
		vout[i] = vin[i]->v.rv;
		break;
	    default:
		wile_exception("matrix-lu-solve", "promote-real",
			       "internal error! should never happen");
	    }
	} else {
	    vout[i] = 0.0;
	}
    }

    if (index) {
	for (i = 0; i < n; ++i) {
	    size_t j = *index++;
	    if (j != i) {
		lisp_real_t temp = vout[j];
		vout[j] = vout[i];
		vout[i] = temp;
	    }
	}
    }

    return vout;
}

static lisp_cmplx_t* promote_cmplx(lisp_cmplx_t* vout, lptr* vin,
				   size_t* index, size_t n)
{
    if (vout == NULL) {
	vout = LISP_ALLOC(lisp_cmplx_t, n);
    }

    size_t i;
    for (i = 0; i < n; ++i) {
	if (vin[i]) {
	    switch (vin[i]->vt) {
	    case LV_NIL:
		vout[i] = CMPLX(0.0, 0.0);
		break;
	    case LV_INT:
		vout[i] = CMPLX((lisp_real_t) vin[i]->v.iv, 0.0);
		break;
	    case LV_RAT:
		vout[i] = CMPLX(LV_RAT2REAL(*vin[i]), 0.0);
		break;
	    case LV_REAL:
		vout[i] = CMPLX(vin[i]->v.rv, 0.0);
		break;
	    case LV_CMPLX:
		vout[i] = vin[i]->v.cv;
		break;
	    default:
		wile_exception("matrix-lu-solve", "promote-cmplx",
			       "internal error! should never happen");
	    }
	} else {
	    vout[i] = CMPLX(0.0, 0.0);
	}
    }

    if (index) {
	for (i = 0; i < n; ++i) {
	    size_t j = *index++;
	    if (j != i) {
		lisp_cmplx_t temp = vout[j];
		vout[j] = vout[i];
		vout[i] = temp;
	    }
	}
    }

    return vout;
}

// "native" format is row-major, but this algorithm is formulated
// in terms of column-major; so we need to swap only if !transpose

#define MAYBE_TRANSPOSE()					\
    do {							\
	if (!transpose) {					\
	    size_t k1, k2;					\
	    for (j = 0; j < n; ++j) {				\
		for (i = j + 1; i < n; ++i) {			\
		    k1 = i + n*j;				\
		    k2 = j + n*i;				\
		    temp = mat[k1];				\
		    mat[k1] = mat[k2];				\
		    mat[k2] = temp;				\
		}						\
	    }							\
	}							\
    } while (0)

#define FIND_BANDWIDTH(ZMAC)					\
    do {							\
	w = 0;							\
	for (j = 0; j < n; ++j) {				\
	    for (i = 0; i < n; ++i) {				\
		ix1 = i + j*n;					\
		if (!ZMAC(mat[ix1])) {				\
		    k = (j > i) ? (j - i) : (i - j);		\
		    if (w < k) {				\
			w = k;					\
		    }						\
		}						\
	    }							\
	}							\
	w2 = 2*w;						\
    } while (0)

#define ZMAC_RAT(v)		(v.num == 0)
#define ZMAC_REAL(v)		(v == 0.0)
#define ZMAC_CMPLX(v)		(CREAL(v) == 0.0 && CIMAG(v) == 0.0)

static inline lisp_rat_t rat_sub(lisp_rat_t a, lisp_rat_t b)
{
    lisp_rat_t c;
    lisp_int_t g;

    c.num = a.num*b.den - b.num*a.den;
    c.den = a.den*b.den;
    g = lgcd(c.num, c.den);
    c.num /= g;
    c.den /= g;

    return c;
}

static inline lisp_rat_t rat_mul(lisp_rat_t a, lisp_rat_t b)
{
    lisp_rat_t c;
    lisp_int_t g;

    c.num = a.num*b.num;
    c.den = a.den*b.den;
    g = lgcd(c.num, c.den);
    c.num /= g;
    c.den /= g;

    return c;
}

static lval lud_rat(size_t n, lisp_vector_t* mat1, bool transpose)
{
    size_t i, j, k, ix1, w, w2, jmw2, jpw, jpw2;
    lisp_rat_t sum, tmax, temp, *mp1, *mp2, *mp3;
    int sign;

    lval ret = mk_vec(&sym_rat, n, sizeof(lisp_rat_t));
    lisp_rat_t* mat = (lisp_rat_t*) ret.v.vec.arr[2]->v.bvec.arr;
    size_t* index = (size_t*) ret.v.vec.arr[3]->v.bvec.arr;

    (void) promote_rat(mat, mat1->arr, NULL, n*n);
    MAYBE_TRANSPOSE();
    FIND_BANDWIDTH(ZMAC_RAT);

    sign = 1;
    for (j = 0; j < n; ++j) {
	jmw2 = (j > w2) ? (j - w2) : 0;
	jpw = j + w;
	if (jpw >= n) {
	    jpw = n - 1;
	}
	jpw2 = j + w2;
	if (jpw2 >= n) {
	    jpw2 = n - 1;
	}
	ix1 = jmw2 + n*j;

	// calculate supradiagonal U_ij

	mp1 = mat + jmw2 + n*j;
	for (i = jmw2; i < j; ++i) {
	    mp2 = mat + i + n*jmw2;
	    mp3 = mat + jmw2 + n*j;
	    sum = *mp1;
	    for (k = jmw2; k < i; ++k) {
		sum = rat_sub(sum, rat_mul(*mp2, *mp3++));
		mp2 += n;
	    }
	    *mp1++ = sum;
	}

	// calculate candidates for diagonal U_ij and all L_ij

	for (i = j; i <= jpw; ++i) {
	    mp2 = mat + i + n*jmw2;
	    mp3 = mat + jmw2 + n*j;
	    sum = *mp1;
	    for (k = jmw2; k < j; ++k) {
		sum = rat_sub(sum, rat_mul(*mp2, *mp3++));
		mp2 += n;
	    }
	    *mp1++ = sum;
	}

	// find largest candidate

	mp1 = mat + j*(n + 1);
	tmax = *mp1++;
	if (tmax.num < 0) {
	    tmax.num = -tmax.num;
	}
	k = j;
	for (i = j + 1; i <= jpw; ++i) {
	    lisp_rat_t tcur = *mp1++;
	    if (tcur.num < 0) {
		tcur.num = -tcur.num;
	    }
	    if (tmax.num*tcur.den < tcur.num*tmax.den) {
		tmax = tcur;
		k = i;
	    }
	}

	// if largest candidate is zero, the problem is singular

	if (tmax.num == 0) {
	    return LVI_BOOL(false);
	}

	// put largest candidate onto diagonal

	if (k != j) {
	    mp1 = mat + j;
	    mp2 = mat + k;
	    for (i = 0; i <= jpw2; ++i) {
		temp = *mp1;
		*mp1 = *mp2;
		*mp2 = temp;
		mp1 += n;
		mp2 += n;
	    }
	    sign = -sign;
	}
	index[j] = k;

	// calculate the L_ij from the remaining candidates

	mp1 = mat + j*(n + 1);
	temp.num = mp1->den;
	temp.den = mp1->num;
	if (temp.den < 0) {
	    temp.num = -temp.num;
	    temp.den = -temp.den;
	}
	++mp1;
	for (i = j; i < jpw; ++i) {
	    *mp1 = rat_mul(*mp1, temp);
	    ++mp1;
	}
    }
    *(ret.v.vec.arr[4]) = LVI_INT(sign);

    return ret;
}

// generic real or complex LU-decomposition

#define GENERIC_DECOMPOSE(NABS)					\
    int sign;							\
    do {							\
	sign = 1;						\
	for (j = 0; j < n; ++j) {				\
	    jmw2 = (j > w2) ? (j - w2) : 0;			\
	    jpw = j + w;					\
	    if (jpw >= n) {					\
		jpw = n - 1;					\
	    }							\
	    jpw2 = j + w2;					\
	    if (jpw2 >= n) {					\
		jpw2 = n - 1;					\
	    }							\
	    ix1 = jmw2 + n*j;					\
	    /* calculate supradiagonal U_ij */			\
	    mp1 = mat + jmw2 + n*j;				\
	    for (i = jmw2; i < j; ++i) {			\
		mp2 = mat + i + n*jmw2;				\
		mp3 = mat + jmw2 + n*j;				\
		sum = *mp1;					\
		for (k = jmw2; k < i; ++k) {			\
		    sum -= (*mp2)*(*mp3++);			\
		    mp2 += n;					\
		}						\
		*mp1++ = sum;					\
	    }							\
	    /* calculate candidates for diagonal U_ij and all L_ij */	\
	    for (i = j; i <= jpw; ++i) {			\
		mp2 = mat + i + n*jmw2;				\
		mp3 = mat + jmw2 + n*j;				\
		sum = *mp1;					\
		for (k = jmw2; k < j; ++k) {			\
		    sum -= (*mp2)*(*mp3++);			\
		    mp2 += n;					\
		}						\
		*mp1++ = sum;					\
	    }							\
	    /* find largest candidate */			\
	    mp1 = mat + j*(n + 1);				\
	    tmax = NABS(*mp1++);				\
	    k = j;						\
	    for (i = j + 1; i <= jpw; ++i) {			\
		lisp_real_t tcur = NABS(*mp1++);		\
		if (tmax < tcur) {				\
		    tmax = tcur;				\
		    k = i;					\
		}						\
	    }							\
	    /* if largest candidate is zero, the problem is singular */ \
	    if (tmax == 0.0) {					\
		return LVI_BOOL(false);				\
	    }							\
	    /* put largest candidate onto diagonal */		\
	    if (k != j) {					\
		mp1 = mat + j;					\
		mp2 = mat + k;					\
		for (i = 0; i <= jpw2; ++i) {			\
		    temp = *mp1;				\
		    *mp1 = *mp2;				\
		    *mp2 = temp;				\
		    mp1 += n;					\
		    mp2 += n;					\
		}						\
		sign = -sign;					\
	    }							\
	    index[j] = k;					\
	    /* calculate the L_ij from the remaining candidates */ \
	    mp1 = mat + j*(n + 1);				\
	    temp = 1.0/(*mp1++);				\
	    for (i = j; i < jpw; ++i) {				\
		*mp1++ *= temp;					\
	    }							\
	}							\
	*(ret.v.vec.arr[4]) = LVI_INT(sign);			\
	return ret;						\
    } while (0)

static lval lud_real(size_t n, lisp_vector_t* mat1, bool transpose)
{
    size_t i, j, k, ix1, w, w2, jmw2, jpw, jpw2;
    lisp_real_t sum, tmax, temp, *mp1, *mp2, *mp3;

    lval ret = mk_vec(&sym_real, n, sizeof(lisp_real_t));
    lisp_real_t* mat = (lisp_real_t*) ret.v.vec.arr[2]->v.bvec.arr;
    size_t* index = (size_t*) ret.v.vec.arr[3]->v.bvec.arr;

    (void) promote_real(mat, mat1->arr, NULL, n*n);
    MAYBE_TRANSPOSE();
    FIND_BANDWIDTH(ZMAC_REAL);
    GENERIC_DECOMPOSE(FABS);
}

static lval lud_cmplx(size_t n, lisp_vector_t* mat1, bool transpose)
{
    size_t i, j, k, ix1, w, w2, jmw2, jpw, jpw2;
    lisp_cmplx_t sum, temp, *mp1, *mp2, *mp3;
    lisp_real_t tmax;

    lval ret = mk_vec(&sym_cmplx, n, sizeof(lisp_cmplx_t));
    lisp_cmplx_t* mat = (lisp_cmplx_t*) ret.v.vec.arr[2]->v.bvec.arr;
    size_t* index = (size_t*) ret.v.vec.arr[3]->v.bvec.arr;

    (void) promote_cmplx(mat, mat1->arr, NULL, n*n);
    MAYBE_TRANSPOSE();
    FIND_BANDWIDTH(ZMAC_CMPLX);
    GENERIC_DECOMPOSE(CABS);
}

#define RAT_GEN_SOLVE(wrap)					\
    do {							\
	size_t i, j;						\
	/* forward solution of L.Y = B */			\
	for (i = 0; i < n; ++i) {				\
	    fp1 = sol + i;					\
	    fp2 = mat + 1 + (n + 1)*i;				\
	    temp = *fp1++;					\
	    for (j = i + 1; j < n; ++j) {			\
		*fp1++ -= (fp2->num)*temp/(fp2->den);		\
		++fp2;						\
	    }							\
	}							\
	/* backward solution of U.X = Y */			\
	i = n - 1;						\
	while (1) {						\
	    fp1 = sol + i;					\
	    fp2 = mat + i*(n + 1);				\
	    *fp1 *= fp2->den;					\
	    *fp1 /= fp2->num;					\
	    --fp2;						\
	    temp = *fp1--;					\
	    for (j = 0; j < i; ++j) {				\
		*fp1-- -= (fp2->num)*temp/(fp2->den);		\
		--fp2;						\
	    }							\
	    if (i == 0) {					\
		break;						\
	    }							\
	    --i;						\
	}							\
	for (i = 0; i < n; ++i) {				\
	    ret[i] = wrap(sol[i]);				\
	}							\
    } while (0)

#define GENERIC_SOLVE(wrap)					\
    do {							\
	size_t i, j;						\
	/* forward solution of L.Y = B */			\
	for (i = 0; i < n; ++i) {				\
	    fp1 = sol + i;					\
	    fp2 = mat + 1 + (n + 1)*i;				\
	    temp = *fp1++;					\
	    for (j = i + 1; j < n; ++j) {			\
		*fp1++ -= (*fp2++)*temp;			\
	    }							\
	}							\
	/* backward solution of U.X = Y */			\
	i = n - 1;						\
	while (1) {						\
	    fp1 = sol + i;					\
	    fp2 = mat + i*(n + 1);				\
	    *fp1 /= *fp2--;					\
	    temp = *fp1--;					\
	    for (j = 0; j < i; ++j) {				\
		*fp1-- -= (*fp2--)*temp;			\
	    }							\
	    if (i == 0) {					\
		break;						\
	    }							\
	    --i;						\
	}							\
	for (i = 0; i < n; ++i) {				\
	    ret[i] = wrap(sol[i]);				\
	}							\
    } while (0)

static void lus_rat_rat(lisp_rat_t* mat, size_t* index,
			lptr* rhs, size_t n, lptr* ret)
{
    lisp_rat_t *fp2;
    lisp_rat_t *sol, *fp1, temp;

    sol = promote_rat(NULL, rhs, index, n);

    size_t i, j;
    /* forward solution of L.Y = B */
    for (i = 0; i < n; ++i) {
	fp1 = sol + i;
	fp2 = mat + 1 + (n + 1)*i;
	temp = *fp1++;
	for (j = i + 1; j < n; ++j) {
	    *fp1 = rat_sub(*fp1, rat_mul(*fp2, temp));
	    ++fp1;
	    ++fp2;
	}
    }
    /* backward solution of U.X = Y */
    i = n - 1;
    while (1) {
	fp1 = sol + i;
	fp2 = mat + i*(n + 1);
	temp.num = fp2->den;
	temp.den = fp2->num;
	*fp1 = rat_mul(*fp1, temp);
	--fp2;
	temp = *fp1--;
	for (j = 0; j < i; ++j) {
	    *fp1 = rat_sub(*fp1, rat_mul(*fp2, temp));
	    --fp1;
	    --fp2;
	}
	if (i == 0) {
	    break;
	}
	--i;
    }
    for (i = 0; i < n; ++i) {
	ret[i] = new_rat1(sol[i]);
    }
}

static void lus_rat_real(lisp_rat_t* mat, size_t* index,
			 lptr* rhs, size_t n, lptr* ret)
{
    lisp_rat_t *fp2;
    lisp_real_t *sol, *fp1, temp;

    sol = promote_real(NULL, rhs, index, n);
    RAT_GEN_SOLVE(new_real);
}

static void lus_rat_cmplx(lisp_rat_t* mat, size_t* index,
			  lptr* rhs, size_t n, lptr* ret)
{
    lisp_rat_t *fp2;
    lisp_cmplx_t *sol, *fp1, temp;

    sol = promote_cmplx(NULL, rhs, index, n);
    RAT_GEN_SOLVE(new_cmplx1);
}

static void lus_real_real(lisp_real_t* mat, size_t* index,
			  lptr* rhs, size_t n, lptr* ret)
{
    lisp_real_t *fp2;
    lisp_real_t *sol, *fp1, temp;

    sol = promote_real(NULL, rhs, index, n);
    GENERIC_SOLVE(new_real);
}

static void lus_real_cmplx(lisp_real_t* mat, size_t* index,
			   lptr* rhs, size_t n, lptr* ret)
{
    lisp_real_t *fp2;
    lisp_cmplx_t *sol, *fp1, temp;

    sol = promote_cmplx(NULL, rhs, index, n);
    GENERIC_SOLVE(new_cmplx1);
}

static void lus_cmplx_cmplx(lisp_cmplx_t* mat, size_t* index,
			    lptr* rhs, size_t n, lptr* ret)
{
    lisp_cmplx_t *fp2;
    lisp_cmplx_t *sol, *fp1, temp;

    sol = promote_cmplx(NULL, rhs, index, n);
    GENERIC_SOLVE(new_cmplx1);
}
