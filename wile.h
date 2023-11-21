#ifndef WILE_H
#define WILE_H

// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// This file is part of the wile RTL and is licensed under LGPL
// version 3 or later; see file 'LICENSE-LGPL' for details.

#include <stdio.h>
#include <stdint.h>
#include <setjmp.h>
#include <stdbool.h>
#include <complex.h>
#include <math.h>
#include <limits.h>
#include <float.h>

#ifdef __GNUC__
#define WILE_ATTR(x)	__attribute__(x)
#else
#define WILE_ATTR(x)
#endif

#ifdef WILE_USES_SQLITE
#include "sqlite3.h"
#endif // WILE_USES_SQLITE

enum val_type {
    VT_UNINIT,
    LV_NIL,
    LV_SYMBOL,
    LV_BOOL,
    LV_CHAR,
    LV_STRING,

    LV_INT,			// LV_INT, LV_RAT, LV_REAL, LV_CMPLX
    LV_RAT,			// must come in that order, since
    LV_REAL,			// numeric type-promoting functions
    LV_CMPLX,			// rely on it

    LV_PAIR,
    LV_VECTOR,
    LV_BVECTOR,

    LV_FILE_PORT,
    LV_PIPE_PORT,
    LV_STR_PORT,
    LV_SOCK_PORT,
    LV_SQLITE_PORT,
    LV_SQLITE_STMT,
    LV_PROMISE,

    LV_CLAMBDA,
    LV_ILAMBDA,
    LV_CONT,

    LV_TYPES_COUNT		// must be last
};

#define TYPE_COMBO(t1,t2)	((t1) + LV_TYPES_COUNT*(t2))

#undef WILE_GOT_INT

//////////////// plain old long int ////////////////

#ifdef WILE_USES_LONG_INT

#ifdef WILE_GOT_INT
#error "wile int format was already specified!"
#endif
#define WILE_GOT_INT

typedef long int lisp_int_t;
typedef unsigned long int lisp_uint_t;

#endif // WILE_USES_LONG_INT

//////////////// __int128_t ////////////////

#ifdef WILE_USES_INT128

#ifdef WILE_GOT_INT
#error "wile int format was already specified!"
#endif
#define WILE_GOT_INT

typedef __int128 lisp_int_t;
typedef unsigned __int128 lisp_uint_t;

#endif // WILE_USES_INT128

//////////////// ginormous int ////////////////

#ifdef WILE_USES_BIGINT

#ifdef WILE_GOT_INT
#error "wile int format was already specified!"
#endif
#define WILE_GOT_INT

typedef struct bigint {
    uint64_t bdits[16];
} lisp_int_t;

#endif // WILE_USES_BIGINT

#ifndef WILE_GOT_INT
#error "wile int format was not specified!"
#endif

typedef struct {
    lisp_int_t num;
    lisp_int_t den;
} lisp_rat_t;

#undef WILE_GOT_REAL

//////////////// plain old double ////////////////

#ifdef WILE_USES_DOUBLE

#ifdef WILE_GOT_REAL
#error "wile real format was already specified!"
#endif
#define WILE_GOT_REAL

typedef double lisp_real_t;
typedef double complex lisp_cmplx_t;

#define REAL_INF		(1.0/0.0)
#define REAL_NAN		nan("")
#define REAL_SCAN_FMT		"%lf"
#define REAL_PRINT_FMT_P	"%+.17e"
#define REAL_PRINT_FMT		"%.17e"
#define REAL_EPSILON		DBL_EPSILON
#define REAL_LIT(v)		v

#define ISINF(v)		isinf(v)
#define ISNAN(v)		isnan(v)
#define COPYSIGN(a,b)		copysign((a),(b))
#define HYPOT(a,b)		hypot((a),(b))
#define LDEXP(a,b)		ldexp((a),(b))
#define FREXP(a,b)		frexp((a),(b))
#define CREAL(v)		creal(v)
#define CIMAG(v)		cimag(v)
#define CABS(v)			cabs(v)
////#define CMPLX(x,y)		CMPLX(x,y)
#define CONJ(v)			conj(v)

#define FLOOR(v)		floor(v)
#define CEIL(v)			ceil(v)
#define SQRT(v)			sqrt(v)
#define CBRT(v)			cbrt(v)
#define EXP(v)			exp(v)
#define LOG(v)			log(v)
#define COS(v)			cos(v)
#define SIN(v)			sin(v)
#define TAN(v)			tan(v)
#define COSH(v)			cosh(v)
#define SINH(v)			sinh(v)
#define TANH(v)			tanh(v)
#define ACOS(v)			acos(v)
#define ASIN(v)			asin(v)
#define ATAN(v)			atan(v)
#define ATAN2(a,b)		atan2(a,b)
#define ACOSH(v)		acosh(v)
#define ASINH(v)		asinh(v)
#define ATANH(v)		atanh(v)
#define ERFC(v)			erfc(v)
#define J0(v)			j0(v)
#define J1(v)			j1(v)
#define JN(n,v)			jn(n,v)
#define Y0(v)			y0(v)
#define Y1(v)			y1(v)
#define YN(n,v)			yn(n,v)
#define POW(x,y)		pow(x,y)
#define FMOD(x,y)		fmod(x,y)
#define FABS(v)			fabs(v)

#define CSQRT(v)		csqrt(v)
#define CEXP(v)			cexp(v)
#define CLOG(v)			clog(v)
#define CCOS(v)			ccos(v)
#define CSIN(v)			csin(v)
#define CTAN(v)			ctan(v)
#define CCOSH(v)		ccosh(v)
#define CSINH(v)		csinh(v)
#define CTANH(v)		ctanh(v)
#define CACOS(v)		cacos(v)
#define CASIN(v)		casin(v)
#define CATAN(v)		catan(v)
#define CACOSH(v)		cacosh(v)
#define CASINH(v)		casinh(v)
#define CATANH(v)		catanh(v)
#define CPOW(x,y)		cpow(x,y)

#define PI_L		3.14159265358979323846264338327950288419716939937511
#define E_L		2.71828182845904523536028747135266249775724709369996
#define EINV_L		0.36787944117144232159552377016146087
#define EGAMMA_L	0.57721566490153286060651209008240243104215933593992

#endif // WILE_USES_DOUBLE

//////////////// long double ////////////////

#ifdef WILE_USES_LONG_DOUBLE

#ifdef WILE_GOT_REAL
#error "wile real format was already specified!"
#endif
#define WILE_GOT_REAL

typedef long double lisp_real_t;
typedef long double complex lisp_cmplx_t;

#define REAL_INF		(1.0L/0.0L)
#define REAL_NAN		nanl("")
#define REAL_SCAN_FMT		"%Lf"
#define REAL_PRINT_FMT_P	"%+.21Le"
#define REAL_PRINT_FMT		"%.21Le"
#define REAL_EPSILON		LDBL_EPSILON
#define REAL_LIT(v)		v ## L

#define ISINF(v)		isinf(v)
#define ISNAN(v)		isnan(v)
#define COPYSIGN(a,b)		copysignl((a),(b))
#define HYPOT(a,b)		hypotl((a),(b))
#define LDEXP(a,b)		ldexpl((a),(b))
#define FREXP(a,b)		frexpl((a),(b))
#define CREAL(v)		creall(v)
#define CIMAG(v)		cimagl(v)
#define CABS(v)			cabsl(v)
#undef CMPLX
#define CMPLX(x,y)		CMPLXL(x,y)
#define CONJ(v)			conjl(v)

#define FLOOR(v)		floorl(v)
#define CEIL(v)			ceill(v)
#define SQRT(v)			sqrtl(v)
#define CBRT(v)			cbrtl(v)
#define EXP(v)			expl(v)
#define LOG(v)			logl(v)
#define COS(v)			cosl(v)
#define SIN(v)			sinl(v)
#define TAN(v)			tanl(v)
#define COSH(v)			coshl(v)
#define SINH(v)			sinhl(v)
#define TANH(v)			tanhl(v)
#define ACOS(v)			acosl(v)
#define ASIN(v)			asinl(v)
#define ATAN(v)			atanl(v)
#define ATAN2(a,b)		atan2l(a,b)
#define ACOSH(v)		acoshl(v)
#define ASINH(v)		asinhl(v)
#define ATANH(v)		atanhl(v)
#define ERFC(v)			erfcl(v)
#define J0(v)			j0l(v)
#define J1(v)			j1l(v)
#define JN(n,v)			jnl(n,v)
#define Y0(v)			y0l(v)
#define Y1(v)			y1l(v)
#define YN(n,v)			ynl(n,v)
#define POW(x,y)		powl(x,y)
#define FMOD(x,y)		fmodl(x,y)
#define FABS(v)			fabsl(v)

#define CSQRT(v)		csqrtl(v)
#define CEXP(v)			cexpl(v)
#define CLOG(v)			clogl(v)
#define CCOS(v)			ccosl(v)
#define CSIN(v)			csinl(v)
#define CTAN(v)			ctanl(v)
#define CCOSH(v)		ccoshl(v)
#define CSINH(v)		csinhl(v)
#define CTANH(v)		ctanhl(v)
#define CACOS(v)		cacosl(v)
#define CASIN(v)		casinl(v)
#define CATAN(v)		catanl(v)
#define CACOSH(v)		cacoshl(v)
#define CASINH(v)		casinhl(v)
#define CATANH(v)		catanhl(v)
#define CPOW(x,y)		cpowl(x,y)

#define PI_L		3.14159265358979323846264338327950288419716939937511L
// #define E_L		2.71828182845904523536028747135266249775724709369996L
#define EINV_L		0.36787944117144232159552377016146087L
#define EGAMMA_L	0.57721566490153286060651209008240243104215933593992L

#endif // WILE_USES_LONG_DOUBLE

//////////////// __float128 ////////////////

#ifdef WILE_USES_QUAD_DOUBLE

#ifdef WILE_GOT_REAL
#error "wile real format was already specified!"
#endif
#define WILE_GOT_REAL

#include <quadmath.h>

typedef __float128 lisp_real_t;
typedef __complex128 lisp_cmplx_t;

#define REAL_INF		(1.0Q/0.0Q)
#define REAL_NAN		nanq("")
// #define REAL_SCAN_FMT
// #define REAL_PRINT_FMT_P
#define REAL_PRINT_FMT		"%.35Qe"
#define REAL_EPSILON		FLT128_EPSILON
#define REAL_LIT(v)		v ## Q

#define ISINF(v)		isinfq(v)
#define ISNAN(v)		isnanq(v)
#define COPYSIGN(a,b)		copysignq((a),(b))
#define HYPOT(a,b)		hypotq((a),(b))
#define LDEXP(a,b)		ldexpq((a),(b))
#define FREXP(a,b)		frexpq((a),(b))
#define CREAL(v)		crealq(v)
#define CIMAG(v)		cimagq(v)
#define CABS(v)			cabsq(v)
////#define CMPLX(x,y)		CMPLXQ(x,y)
// TODO: if either x or y is NaN, this makes the complex number be NaN+NaN*I,
// unlike the versions for the other sizes... try to fix that.
#undef CMPLX
#define CMPLX(x,y)		((x) + (y)*I)
#define CONJ(v)			conjq(v)

#define FLOOR(v)		floorq(v)
#define CEIL(v)			ceilq(v)
#define SQRT(v)			sqrtq(v)
#define CBRT(v)			cbrtq(v)
#define EXP(v)			expq(v)
#define LOG(v)			logq(v)
#define COS(v)			cosq(v)
#define SIN(v)			sinq(v)
#define TAN(v)			tanq(v)
#define COSH(v)			coshq(v)
#define SINH(v)			sinhq(v)
#define TANH(v)			tanhq(v)
#define ACOS(v)			acosq(v)
#define ASIN(v)			asinq(v)
#define ATAN(v)			atanq(v)
#define ATAN2(a,b)		atan2q(a,b)
#define ACOSH(v)		acoshq(v)
#define ASINH(v)		asinhq(v)
#define ATANH(v)		atanhq(v)
#define ERFC(v)			erfcq(v)
#define J0(v)			j0q(v)
#define J1(v)			j1q(v)
#define JN(n,v)			jnq(n,v)
#define Y0(v)			y0q(v)
#define Y1(v)			y1q(v)
#define YN(n,v)			ynq(n,v)
#define POW(x,y)		powq(x,y)
#define FMOD(x,y)		fmodq(x,y)
#define FABS(v)			fabsq(v)

#define CSQRT(v)		csqrtq(v)
#define CEXP(v)			cexpq(v)
#define CLOG(v)			clogq(v)
#define CCOS(v)			ccosq(v)
#define CSIN(v)			csinq(v)
#define CTAN(v)			ctanq(v)
#define CCOSH(v)		ccoshq(v)
#define CSINH(v)		csinhq(v)
#define CTANH(v)		ctanhq(v)
#define CACOS(v)		cacosq(v)
#define CASIN(v)		casinq(v)
#define CATAN(v)		catanq(v)
#define CACOSH(v)		cacoshq(v)
#define CASINH(v)		casinhq(v)
#define CATANH(v)		catanhq(v)
#define CPOW(x,y)		cpowq(x,y)

#define PI_L		3.14159265358979323846264338327950288419716939937511Q
#define E_L		2.71828182845904523536028747135266249775724709369996Q
#define EINV_L		0.36787944117144232159552377016146087Q
#define EGAMMA_L	0.57721566490153286060651209008240243104215933593992Q

#endif // WILE_USES_QUAD_DOUBLE

#ifndef WILE_GOT_REAL
#error "wile real format was not specified!"
#endif

// Need to override a few things here
#ifdef __OpenBSD__
#undef CMPLX
#undef JN
#undef YN
#define CMPLX(x,y)		((x) + I*(y))
#define JN(n,v)			jn(n,v)
#define YN(n,v)			yn(n,v)
#endif // __OpenBSD__

#ifdef __clang__
#ifndef CMPLX
#define CMPLX(x,y)		((x) + I*(y))
#endif // CMPLX
#endif // __clang__

#define ISFINITE(v)	((!ISINF(v)) && (!ISNAN(v)))

// forward declaration and some pointers
struct lisp_val;
typedef struct lisp_val* lptr;

typedef struct {
    lptr car;
    lptr cdr;
} lisp_pair_t;

// wile compiled functions
typedef struct lisp_val (wile_cfunc_t)(lptr* clos, lptr args);

typedef struct {
    lptr* arr;
    size_t capa;
} lisp_vector_t;

typedef struct {
    unsigned char* arr;
    size_t size;
    size_t capa;
} lisp_bytevector_t;

typedef struct {
    jmp_buf registers;
    size_t st_size;
    unsigned char* st_save;
    unsigned char* st_wk;
    lptr ret;
} lisp_cont_t;

typedef uint32_t lisp_loc_t;

typedef struct {
    wile_cfunc_t* fn;
    lptr* closure;
    int arity;
} lisp_cfunc_t;

struct lisp_ifunc;

typedef struct lisp_val {
    union {
	bool bv;
	unsigned char chr;
	char* str;
	lisp_int_t iv;
	lisp_rat_t irv;
	lisp_real_t rv;
	lisp_cmplx_t cv;
	lisp_pair_t pair;
	lisp_cfunc_t clambda;
	struct lisp_ifunc* ilambda;
	FILE* fp;
	lisp_vector_t vec;
	lisp_bytevector_t bvec;
	lisp_cont_t* cont;
#ifdef WILE_USES_SQLITE
	sqlite3* sqlite_conn;
	sqlite3_stmt* sqlite_stmt;
#endif // WILE_USES_SQLITE
    } v;
    lisp_loc_t origin;
    enum val_type vt;
} lval;

typedef struct lisp_ifunc {
    lval args;
    int arity;
    lval body;
    lval env;
    bool macro;
} lisp_ifunc_t;

#define YYSTYPE		lval

#define CAR(lvp)	((lvp)->v.pair.car)
#define CDR(lvp)	((lvp)->v.pair.cdr)

#define CAAR(lvp)	CAR(CAR(lvp))
#define CADR(lvp)	CAR(CDR(lvp))
#define CDAR(lvp)	CDR(CAR(lvp))
#define CDDR(lvp)	CDR(CDR(lvp))

// The full set of combinations would be overkill, but these are useful
#define CADDR(lvp)	CAR(CDR(CDR(lvp)))
#define CADDDR(lvp)	CAR(CDR(CDR(CDR(lvp))))
#define CADDDDR(lvp)	CAR(CDR(CDR(CDR(CDR(lvp)))))

#define BSIZE1	(8+sizeof(lisp_int_t)*CHAR_BIT)
#define BSIZE2	2048
#define BSIZE	((BSIZE1 > BSIZE2) ? BSIZE1 : BSIZE2)

struct lisp_escape_info {
    jmp_buf cenv;
    lptr errval;
    lisp_loc_t l_whence;
    const char* c_whence;
    struct lisp_escape_info* next;
};

typedef struct lisp_escape_info* lisp_escape_t;

extern lisp_escape_t cachalot;

lisp_int_t lgcd(lisp_int_t p, lisp_int_t q);

void show_kv_pair(const void* vp, FILE* fp, lisp_bytevector_t* bvp);

lptr clear_display_hook(const char* sym);
lptr get_display_hook(const char* sym);
lptr set_display_hook(const char* sym, lptr hook);

void wile_print_lisp_val(lptr vp, FILE* fp);
void wile_sprint_lisp_num(char* buf, size_t bsize, lptr num,
			  int base, int prec, bool psign);

lisp_loc_t wile_encode_line_loc(size_t lineno);
char* wile_decode_line_loc(lisp_loc_t lloc);
void wile_set_lisp_loc_file(const char* fname);
lisp_loc_t wile_get_lisp_loc(lptr vp);

void err_print(const char* fname, lisp_loc_t l_whence,
	       const char* c_whence, const char* fmt, ...)
    WILE_ATTR((noreturn,format(printf,4,5)));

const char* typename(enum val_type vt);

void bv_putc(unsigned char c, lisp_bytevector_t* bvp);
void bv_puts(const unsigned char* s, lisp_bytevector_t* bvp);
void bv_putbytes(size_t sl, const unsigned char* s, lisp_bytevector_t* bvp);

bool wile_do_eqv(lptr arg1, lptr arg2);

// continuation stuff

int stack_check(int verbose);
void stack_base(void* st_bptr);
void wile_invoke_continuation(lptr cc, lptr args)
    WILE_ATTR((noreturn));

// wile low-level CFFT routines

void wilec_cfft_init(void);
bool wilec_cfft_good_n(lisp_int_t n);
lisp_cmplx_t* wilec_cfft(int si, size_t n, size_t nc,
			 lisp_cmplx_t* a1, lisp_cmplx_t* a2);

#define LISP_ASSERT(expr)						\
    do {								\
	if (!(expr)) {							\
	    fprintf(stderr, "wile assertion '%s' failed at %s:%d\n",	\
		    #expr, __FILE__, __LINE__);				\
	    exit(1);							\
	}								\
    } while (0)

// these are only for comparing pointers

#define LISP_PMIN(a,b)	(((void*) (a) < (void*) (b)) ? (void*) (a) : (void*) (b))
#define LISP_PMAX(a,b)	(((void*) (a) > (void*) (b)) ? (void*) (a) : (void*) (b))

#endif /* WILE_H */
