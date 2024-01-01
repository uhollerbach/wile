// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <limits.h>
#include <sys/types.h>

#include "wile-rtl1.h"

// compute the GCD of two integers; this is here because of CAN_RAT below

lisp_int_t lgcd(lisp_int_t p, lisp_int_t q)
{
    lisp_int_t m, n, r;

    m = (p >= 0) ? p : -p;
    n = (q >= 0) ? q : -q;
    if (m < n) {
	r = m;
	m = n;
	n = r;
    }
    if (n == 0) {
	return (m > 1) ? m : 1;
    }
    while ((r = m%n) != 0) {
	m = n;
	n = r;
    }
    return n;
}

// allocation-related stuff begins here

#ifdef WILE_USES_GC
#include <gc.h>
#endif // WILE_USES_GC

void* lisp_alloc(size_t nb, const char* loc)
{
    void* ret;

    if (nb == 0) {
	nb = 1;
    }
#ifdef WILE_USES_GC
    ret = GC_malloc(nb);
#else
    ret = malloc(nb);
#endif // WILE_USES_GC
    if (ret == NULL) {
	wile_exception("<alloc>", loc, "failed to allocate %zu bytes", nb);
    }
    return ret;
}

void* lisp_realloc(void* op, size_t nb, const char* loc)
{
    void* ret;

    if (nb == 0) {
	nb = 1;
    }
#ifdef WILE_USES_GC
    ret = GC_realloc(op, nb);
#else
    ret = realloc(op, nb);
#endif // WILE_USES_GC
    if (ret == NULL) {
	wile_exception("<alloc>", loc, "failed to reallocate %zu bytes", nb);
    }
    return ret;
}

char* lisp_strdup(const char* str)
{
    char* ret = NULL;
    if (str) {
	size_t l = 1 + strlen(str);
	ret = LISP_ALLOC(char, l);
	memcpy(ret, str, l);
    }
    return ret;
}

void lisp_free(void* ip)
{
#ifdef WILE_USES_GC
    GC_free(ip);
#else
    free(ip);
#endif // WILE_USES_GC
}

lptr new_lv(enum val_type vt)
{
    lptr ret;

    ret = LISP_ALLOC(lval, 1);
    memset(ret, 0, sizeof(lval));
    ret->vt = vt;

    return ret;
}

lptr new_pair(lptr car, lptr cdr)
{
    lptr ret = new_lv(LV_PAIR);
    CAR(ret) = car;
    CDR(ret) = cdr;
    return ret;
}

lptr new_bool(bool val)
{
    lptr ret = new_lv(LV_BOOL);
    ret->v.bv = val;
    return ret;
}

lptr new_char(char val)
{
    lptr ret = new_lv(LV_CHAR);
    ret->v.chr = val;
    return ret;
}

lptr new_int(lisp_int_t val)
{
    lptr ret = new_lv(LV_INT);
    ret->v.iv = val;
    return ret;
}

lptr new_rat1(lisp_rat_t val)
{
    lptr ret = new_lv(LV_RAT);
    CAN_RAT(val.num, val.den);
    ret->v.irv = val;
    if (ret->v.irv.den == 1) {
	lisp_int_t aux = ret->v.irv.num;
	ret->v.iv = aux;
	ret->vt = LV_INT;
    }
    return ret;
}

lptr new_rat2(lisp_int_t num, lisp_int_t den)
{
    lptr ret = new_lv(VT_UNINIT);
    CAN_RAT(num, den);
    if (den == 1) {
	lisp_int_t aux = num;
	ret->v.iv = aux;
	ret->vt = LV_INT;
    } else {
	ret->vt = LV_RAT;
	ret->v.irv.num = num;
	ret->v.irv.den = den;
    }
    return ret;
}

lptr new_real(lisp_real_t val)
{
    lptr ret = new_lv(LV_REAL);
    ret->v.rv = val;
    return ret;
}

lptr new_cmplx1(lisp_cmplx_t cv)
{
    lptr ret = new_lv(LV_CMPLX);
    ret->v.cv = cv;
    return ret;
}

lptr new_cmplx2(lisp_real_t rv, lisp_real_t iv)
{
    lptr ret = new_lv(LV_CMPLX);
    ret->v.cv = CMPLX(rv, iv);
    return ret;
}

lptr new_string_empty(size_t len)
{
    lptr ret = new_lv(LV_STRING);
    ret->v.str = LISP_ALLOC(char, 1 + len);
    ret->v.str[len] = '\0';
    return ret;
}

lptr new_string(const char* str)
{
    if (str == NULL) {
	str = "<NULL>";
    }
    size_t len = strlen(str);
    lptr ret = new_string_empty(len);
    memcpy(ret->v.str, str, len);
    return ret;
}

lptr new_vec(size_t capa)
{
    size_t i;
    lptr ret = new_lv(LV_VECTOR);
    ret->v.vec.capa = capa;
    if (capa < 1) {
	capa = 1;
    }
    ret->v.vec.arr = LISP_ALLOC(lptr, capa);
    for (i = 0; i < capa; ++i) {
	ret->v.vec.arr[i] = NULL;
    }
    return ret;
}

lptr new_bvec(size_t capa)
{
    lptr ret = new_lv(LV_BVECTOR);
    ret->v.bvec.capa = capa;
    if (capa < 1) {
	capa = 1;
    }
    ret->v.bvec.arr = LISP_ALLOC(unsigned char, capa);
    memset(ret->v.bvec.arr, '\0', capa);
    return ret;
}

lptr new_spine(lptr lp)
{
    return IS_PAIR(lp) ? new_pair(CAR(lp), new_spine(CDR(lp))) : lp;
}

lptr lst2vec(lptr lst)
{
    size_t len = 0;
    lptr ret = lst, lt;

    while (ret) {
	++len;
	ret = CDR(ret);
    }
    ret = new_vec(len);
    len = 0;
    while (lst) {
	ret->v.vec.arr[len++] = CAR(lst);
	lt = CDR(lst);
	LISP_FREE(lst);
	lst = lt;
    }
    return ret;
}

lptr lst2bvec(lptr lst)
{
    size_t len = 0;
    lptr ret = lst, lt;

    while (ret) {
	++len;
	lt = CAR(ret);
	if (!IS_INT(lt) || lt->v.iv < 0 || lt->v.iv > UCHAR_MAX) {
	    wile_exception("list->bytevector",
			   wile_decode_line_loc(wile_get_lisp_loc(ret)),
			   "got bad byte value");
	}
	ret = CDR(ret);
    }
    ret = new_bvec(len);
    len = 0;
    while (lst) {
	ret->v.bvec.arr[len++] = (unsigned char) CAR(lst)->v.iv;
	lt = CDR(lst);
	LISP_FREE(lst);
	lst = lt;
    }
    return ret;
}
