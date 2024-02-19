#ifndef WILE_RUNTIME_H
#define WILE_RUNTIME_H

// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <setjmp.h>
#include <complex.h>
#include <float.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <math.h>
#include <unistd.h>
#include <signal.h>
#include <limits.h>
#include <dirent.h>
#include <errno.h>
#include <netdb.h>
#include <pwd.h>
#include <grp.h>
#include <ctype.h>
#include <getopt.h>
#include <setjmp.h>

#ifdef __linux__
#include <linux/limits.h>
#endif

#include "config.h"

// prepare for autotools-based build

#ifndef WILE_USES_GC
#if HAVE_GC_H
#define WILE_USES_GC
#endif // HAVE_GC_H
#endif // WILE_USES_GC

#ifndef WILE_USES_SQLITE
#if HAVE_SQLITE3_H
#define WILE_USES_SQLITE
#endif // HAVE_SQLITE3_H
#endif // WILE_USES_SQLITE

#if !defined(WILE_USES_QUAD_DOUBLE) && !defined(WILE_USES_LONG_DOUBLE) && !defined(WILE_USES_DOUBLE)
#if HAVE_QUADMATH_H
#define WILE_USES_QUAD_DOUBLE
#else
#define WILE_USES_LONG_DOUBLE
#endif // HAVE_QUADMATH_H
#endif // !defined(double stuff) etc

#if !defined(WILE_USES_LONG_INT) && !defined(WILE_USES_INT128)
#if HAVE_INT128
#define WILE_USES_INT128
#else
#define WILE_USES_LONG_INT
#endif // HAVE_INT128
#endif // !defined(int stuff) etc

// end of prep for autotools

////////////////////////////////////////////////////////////////
// inlined wile.h

#ifdef WILE_USES_SQLITE
#include "sqlite3.h"
#endif // WILE_USES_SQLITE

#include "sha256.h"

#ifdef __GNUC__
#define WILE_ATTR(x)	__attribute__(x)
#else
#define WILE_ATTR(x)
#endif

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
    LV_SHA256_DATA,

    LV_CLAMBDA,
    LV_ILAMBDA,
    LV_CONT,

    LV_PROMISE,

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

#ifdef __APPLE__
#undef JN
#undef YN
#define JN(n,v)			jn(n,v)
#define YN(n,v)			yn(n,v)
#endif // __APPLE__

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
typedef struct lisp_val (wile_cfunc_t)(lptr* clos, lptr args, const char* cloc);

typedef struct {
    lptr* arr;
    size_t capa;
} lisp_vector_t;

typedef struct {
    unsigned char* arr;
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
	SHA256_info* sha256_info;
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
    const char* whence;
    struct lisp_escape_info* next;
};

typedef struct lisp_escape_info* lisp_escape_t;

extern lisp_escape_t cachalot;

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

////////////////////////////////////////////////////////////////
// inlined nfa.h

enum nfa_state_type
{
    TR_NONE,		/* no transitions out of this state */
    TR_CHAR,		/* real char transition, in next1 */
    TR_CC,		/* real char-class transition, in next1 */
    TR_EPS1,		/* one epsilon-transition, in next1 */
    TR_EPS2		/* two epsilon-transitions, in next1 and next2 */
};

enum nfa_anchor_type
{
    NA_NONE = 0,
    NA_BEGIN = (1 << 0),
    NA_END = (1 << 1),
    NA_BOTH = (NA_BEGIN | NA_END)
};

struct nfa_state
{
    unsigned int id;		/* the state's label */
    enum nfa_state_type type;	/* the state's type: outgoing transitions */
    enum nfa_anchor_type anchor;/* the state's anchored-ness: used by regex */
    char tr_char;		/* if a single-character outgoing
				   transition, store it here */
    struct fsi_set* tr_cc;	/* if a character-class outgoing
				   transition, store it here */
    struct fsi_set* eps_closure;/* the state's epsilon-closure */

    unsigned int tag;		/* tag for the end state: this is for use with
				   multiple regexes, such as ugrep -f and ulex */

    /* the following is quasi-private, for traversal of NFAs */
    struct nfa_state* next1;
    struct nfa_state* next2;
    int mark;
};

/* Allocate & free a single NFA state */

struct nfa_state* nfa_state_alloc(void);
void nfa_state_free(struct nfa_state* ns);
void nfa_state_shallow_free(struct nfa_state* ns);

/* Operations on an entire NFA graph */

/* Apply a user-specified function to each node of an NFA. The user function
   may not alter the structure of the graph, and neither may it call another
   instance of nfa_foreach (on the same graph): this does not nest.

   The traversal_order argument specifies the order in which to do the work.
   This is important if information propagates from node to node: if it
   propagates from a node to its next[12] neighbors, then traversal_order
   should be positive; if it propagates in the other direction, then it
   should be negative; and if no information propagates, then it can be 0.
   For example, in computing the epsilon-closure, information travels
   against the pointers: a node's epsilon-closure is the union of its own
   and its next[12] neighbors' epsilon-closures. With traversal_order -1,
   the regex "(((... (a?) ...)?)?)?' takes 2 iterations regardless of the
   number of parentheses, whereas with traversal_order 0 or 1, it takes
   a number of iterations that's proportional to the number of parentheses. */

void nfa_foreach(struct nfa_state* ns,
		 void (*fn)(struct nfa_state*, void*),
		 void* data,
		 int traversal_order);

/* Find the end state. This is currently done in an efficient but non-general
   way that depends on the specific construction of the NFA graph in the
   parse-regex routine. It should perhaps be replaced with a slower but more
   general call to nfa_foreach(). */

struct nfa_state* nfa_find_end(struct nfa_state* ns);

/* Utility routine to print out an NFA graph in a vaguely-readable fashion */

void nfa_print(struct nfa_state* ns);

/* Free an entire NFA graph */

void nfa_free(struct nfa_state* ns);

/* Add an integer label to each node. This is needed for & used in the
   calculation of the epsilon-closure and in the regex_match routine. */

unsigned int nfa_label(struct nfa_state* ns);

/* Compute the epsilon-closure of each node in the NFA graph,
   and return the number of states in the NFA. */

unsigned int nfa_gen_epsilon_closure(struct nfa_state* ns);

/* Add an NFA to the head of an existing list of NFAs, which may be NULL */

void chain_nfa(struct nfa_state** chain, struct nfa_state* link);

////////////////////////////////////////////////////////////////
// inlined fsi_set.h

/*
Routines to manipulate finite sets of generally-small integers.
Sets are represented as bit-sets stored in unsigned char, with
<n>-present-in-set represented as a 1-bit, and <n>-not-present
represented as a 0 bit, in the <n>th location in the bit-set.

These routines, where applicable, make the "closed-world"
assumption: the given bits are all there is; thus, for example,
inverting a set does not add an infinite number of 1-bits.
*/

struct fsi_set {
    unsigned int n_ints;
    unsigned char* bits;
};

struct fsi_set* fsi_set_alloc(unsigned int ni);
struct fsi_set* fsi_set_copy(struct fsi_set* set);
void fsi_set_free(struct fsi_set* set);
void fsi_set_clear(struct fsi_set* set);

void fsi_set_print(struct fsi_set* set);
void fsi_set_print_members(struct fsi_set* set);

void fsi_set_insert(struct fsi_set* set, unsigned int n);
void fsi_set_remove(struct fsi_set* set, unsigned int n);
void fsi_set_toggle(struct fsi_set* set, unsigned int n);
int fsi_set_ismember(struct fsi_set* set, unsigned int n);

/* These, like the above, are in-place:
   set1 |= set2, set1 &= set2, etc */

void fsi_set_or(struct fsi_set* set1, struct fsi_set* set2);
void fsi_set_xor(struct fsi_set* set1, struct fsi_set* set2);
void fsi_set_and(struct fsi_set* set1, struct fsi_set* set2);
void fsi_set_andnot(struct fsi_set* set1, struct fsi_set* set2);
void fsi_set_not(struct fsi_set* set);
void fsi_set_xfer(struct fsi_set* set1, struct fsi_set* set2);

/* Version 2 of or: compute the OR of two fsi_sets and return a count
   of diffs (of the stored bytes, not bits) */

unsigned int fsi_set_or2(struct fsi_set* set1, struct fsi_set* set2);

/* Compare two fsi_sets: return 1 if they are equal, 0 if unequal.
   They are unequal if they have different sizes, even if all the
   bits that are present in one are zero. */

int fsi_set_equal(struct fsi_set* set1, struct fsi_set* set2);

////////////////////////////////////////////////////////////////
// inlined regex.h

#define REGEX_OK	0
#define REGEX_ERR	1

/* These are flags, so they have to be powers of 2 */

enum regex_options
{
    REGEX_OPTION_8BIT = 1 << 0,
    REGEX_OPTION_CRLF_EQUIVALENT = 1 << 1
};

/* Wrapper around an NFA graph to organize & collect needed info for
   regex matching and for conversion to DFA. */

struct nfa_work {
    struct nfa_state* nfa;		/* the actual NFA graph */

    unsigned int n_states;		/* number of states in the graph */
    struct nfa_state** states;		/* alternate way to access states,
					   by id rather than by pointer */
    unsigned int n_chars;		/* number of characters in the input
					   character set */

    struct fsi_set* st_old;		/* container for the "from" group */
    struct fsi_set* st_new;		/* container for the "to" group */
};

/* A key/value list of names & regexes, for {FOO} substitution */

struct regex_defs {
    char* name;			/* the name by which we look this up */
    char* regex;		/* the stuff we substitute for {NAME} */

    struct regex_defs* next;	/* the next one in the list */
};

struct regex_defs* regex_defs_alloc(void);
void regex_defs_free(struct regex_defs* rd);

/* The regular-expression parser: returns a pure NFA */

struct nfa_state* regex_parse(const char* regex, struct regex_defs* rdefs,
			      unsigned int options, unsigned int end_tag);

/* A wrapper that turns a pure NFA into an nfa_work which gets passed
   to regex_match() below or processed in some other fashion. */

struct nfa_work* regex_wrap(struct nfa_state* nfa, unsigned int options);

/* Match routine: accept text, an NFA parsed by regex_parse() above,
   and return whether or not some portion of the text matches. Return
   true if there is a match, false if no match. Additionally, if the
   pointers-to-pointers ms and me are non-NULL, they will get set to
   the beginning and end of the matching area. */

int regex_match(unsigned char* text, struct nfa_work* wk,
		unsigned char** ms, unsigned char** me);

void regex_free(struct nfa_work* wk);

////////////////////////////////////////////////////////////////

#define _STRINGIFY(x)		#x
#define LISP_STRING(x)		_STRINGIFY(x)
#define LISP_WHENCE		(__FILE__ ":" LISP_STRING(__LINE__))

#define LISP_ALLOC(t,c)		((t*) lisp_alloc((c)*sizeof(t), LISP_WHENCE))
#define LISP_REALLOC(t,o,c)	((t*) lisp_realloc(o, (c)*sizeof(t), LISP_WHENCE))
#define LISP_FREE(p)		lisp_free(p)
#define LISP_STRDUP(s)		lisp_strdup(s)

void* lisp_alloc(size_t nb, const char* loc)	WILE_ATTR((malloc));
void* lisp_realloc(void* op, size_t nb, const char* loc);
char* lisp_strdup(const char* str)		WILE_ATTR((malloc));
void lisp_free(void* ip);

lptr new_lv(enum val_type vt)			WILE_ATTR((malloc));
lptr new_pair(lptr car, lptr cdr)		WILE_ATTR((malloc));
lptr new_bool(bool val)				WILE_ATTR((malloc));
lptr new_char(char val)				WILE_ATTR((malloc));
lptr new_int(lisp_int_t val)			WILE_ATTR((malloc));
lptr new_rat1(lisp_rat_t val)			WILE_ATTR((malloc));
lptr new_rat2(lisp_int_t num, lisp_int_t den)	WILE_ATTR((malloc));
lptr new_real(lisp_real_t val)			WILE_ATTR((malloc));
lptr new_cmplx1(lisp_cmplx_t cv)		WILE_ATTR((malloc));
lptr new_cmplx2(lisp_real_t rv, lisp_real_t iv)	WILE_ATTR((malloc));
lptr new_string_empty(size_t len)		WILE_ATTR((malloc));
lptr new_string(const char* str)		WILE_ATTR((malloc));
lptr new_vec(size_t capa)			WILE_ATTR((malloc));
lptr new_bvec(size_t capa)			WILE_ATTR((malloc));

#define FATAL(fname, ...)						\
    do {								\
	fprintf(stderr, "fatal error: '%s' ", fname);			\
	fprintf(stderr, __VA_ARGS__);					\
	fputc('\n', stderr);						\
	exit(1);							\
    } while (0)

#define WARN(fname, ...)						\
    do {								\
	fprintf(stderr, "warning: '%s' ", fname);			\
	fprintf(stderr, __VA_ARGS__);					\
	fputc('\n', stderr);						\
    } while (0)

#define IS_PAIR(a)		((a) != NULL && (a)->vt == LV_PAIR)
#define IS_SYMBOL(a)		((a) != NULL && (a)->vt == LV_SYMBOL)
#define IS_STRING(a)		((a) != NULL && (a)->vt == LV_STRING)
#define IS_INT(a)		((a) != NULL && (a)->vt == LV_INT)

#define IS_CLAMBDA(a)		((a) != NULL && (a)->vt == LV_CLAMBDA)
#define IS_ILAMBDA(a)		((a) != NULL && (a)->vt == LV_ILAMBDA)
#define IS_CONT(a)		((a) != NULL && (a)->vt == LV_CONT)
#define IS_FPORT(a)		((a) != NULL && (a)->vt == LV_FILE_PORT)
#define IS_PPORT(a)		((a) != NULL && (a)->vt == LV_PIPE_PORT)
#define IS_SOCKPORT(a)		((a) != NULL && (a)->vt == LV_SOCK_PORT)

#define IS_NUMERIC(a)						\
    ((a)->vt == LV_INT || (a)->vt == LV_RAT ||			\
     (a)->vt == LV_REAL || (a)->vt == LV_CMPLX)

#define CAN_RAT(n, d)							\
    do {								\
	lisp_int_t ag = lgcd(n, d);					\
	if (ag != 0) {							\
	    n /= ag;							\
	    d /= ag;							\
	}								\
	if (d < 0) {							\
	    d = -d;							\
	    n = -n;							\
	}								\
    } while (0)

#define REVERSE_LIST_INPLACE(lst)					\
    do {								\
	lptr RLIP_tmp1 = NULL;						\
	while (lst) {							\
	    lptr RLIP_tmp2 = CDR(lst);					\
	    CDR(lst) = RLIP_tmp1;					\
	    RLIP_tmp1 = (lst);						\
	    (lst) = RLIP_tmp2;						\
	}								\
	(lst) = RLIP_tmp1;						\
    } while (0)

#define LVI_NIL()		((lval) { .vt = LV_NIL})

#define LVI_BOOL(q)		((lval) { .v.bv = (q), .vt = LV_BOOL})

#define LVI_CHAR(q)		((lval) { .v.chr = (q), .vt = LV_CHAR})

#define LVI_INT(q)		((lval) { .v.iv = (q), .vt = LV_INT})

#define LVI_RAT(qn,qd)		((lval) { .v.irv.num = (qn),		\
					  .v.irv.den = (qd),		\
					  .vt = LV_RAT})

#define LVI_REAL(q)		((lval) { .v.rv = (q), .vt = LV_REAL})

#define LVI_CMPLX1(qc)		((lval) { .v.cv = (qc), .vt = LV_CMPLX})

#define LVI_CMPLX2(qr,qi)	((lval) { .v.cv = (qr) + (qi)*I,	\
					  .vt = LV_CMPLX})

#define LVI_STRING(q)		((lval) { .v.str = LISP_STRDUP(q),	\
					  .vt = LV_STRING})

#define LVI_SYMBOL(q)		((lval) { .v.str = LISP_STRDUP(q),	\
					  .vt = LV_SYMBOL})

#define LVI_STRING_NOCPY(q)	((lval) { .v.str = (q),			\
					  .vt = LV_STRING})

#define LVI_PAIR(qa,qd)		((lval) { .v.pair.car = (qa),		\
					  .v.pair.cdr = (qd),		\
					  .vt = LV_PAIR})

#define LVI_FPORT(q)		((lval) { .v.fp = (q), .vt = LV_FILE_PORT})

#define LVI_PPORT(q)		((lval) { .v.fp = (q), .vt = LV_PIPE_PORT})

#define LVI_SPORT(q)		((lval) { .v.fp = (q), .vt = LV_SOCK_PORT})

#define LVI_PROC(qf,qc,qa)	((lval) { .v.clambda.fn = (qf),		\
					  .v.clambda.closure = (qc),	\
					  .v.clambda.arity = (qa),	\
					  .vt = LV_CLAMBDA})

#define LVI_IPROC(qargs,qari,qb,qe,qf)					\
    ((lval) { .v.ilambda.args = (qargs),				\
	      .v.ilambda.arity = (qari),				\
	      .v.ilambda.body = (qb),					\
	      .v.ilambda.env = (qe),					\
	      .v.ilambda.macro = (qf),					\
	      .vt = LV_ILAMBDA})

#define LV_IS_FALSE(q)		((q).vt == LV_BOOL && (q).v.bv == false)

#define LV_RAT2REAL(q)	(((lisp_real_t) (q).v.irv.num)/((lisp_real_t) (q).v.irv.den))

#define WILE_ABS(v)		(((v) < 0) ? -(v) : (v))
#define WILE_SIGN(v)		((v) > 0 ? 1 : ((v) < 0 ? -1 : 0))

// clang recognizes attribute musttail, which enables TCO

#ifdef __clang__
#define TAIL_CALL		__attribute__((musttail)) return
#else
#define TAIL_CALL		return
#endif

#define MK_CLOS(n,s)	lptr* n = LISP_ALLOC(lptr, s); LISP_ASSERT(n != NULL)
#define P_CLOS(n,i)	(n[i])
#define V_CLOS(n,i)	(*((P_CLOS(n,i)->vt == VT_UNINIT) \
			   ? P_CLOS(n,i)->v.pair.car : P_CLOS(n,i)))

uint16_t wile_binfo(void);
lval wile_sql_version(lptr* clos, lptr args, const char* loc);
lval wile_gc_version(lptr*, lptr, const char*);
lval wile_os_name(void);
lval wile_arch_name(void);

void wile_rand_seed(long int seed);
double wile_rand_dbl(void);

void wile_stack_trace_minimal(int fd);

void wile_exception(const char* func_name, const char* loc,
		    const char* fmt, ...)
    WILE_ATTR((noreturn,format(printf,3,4)));

lval wile_register_display_proc(const char* sym, lval proc, const char* loc);
lval wile_get_gensym(lisp_loc_t);
lval wile_run_system_command(lval cmd, const char* loc);
lval wile_run_pipe_command(lval cmd, const char* rw, const char* loc);
lval wile_temp_file(lptr* clos, lptr args, const char* loc);
lval wile_string2num(lval str, const char* loc);
lval wile_num2string(lval num, int base, int prec, const char* loc);

lisp_int_t powi(lisp_int_t a, lisp_int_t b);

lisp_real_t pcheby1(int n, lisp_real_t x);
lisp_real_t pcheby2(int n, lisp_real_t x);
lisp_real_t plegendre(int n, lisp_real_t x);
lisp_real_t plaguerre(int n, lisp_real_t x);
lisp_real_t phermite1(int n, lisp_real_t x);
lisp_real_t phermite2(int n, lisp_real_t x);

void floor_qr(lisp_int_t n1, lisp_int_t n2,
	      lisp_int_t* nq, lisp_int_t* nr, const char* loc);
void trunc_qr(lisp_int_t n1, lisp_int_t n2,
	      lisp_int_t* nq, lisp_int_t* nr, const char* loc);
void ceil_qr(lisp_int_t n1, lisp_int_t n2,
	     lisp_int_t* nq, lisp_int_t* nr, const char* loc);

// Yes, an lval* is an lptr. Remember that this is an array of lvals,
// not a single lptr. This routine is not intended for zero-length lists;
// if 0 items are passed in, a NULL pointer dereference will result
// and the program will crash. The tail is for generating improper lists;
// passing NULL in makes a proper list.

lval wile_gen_list(size_t nitems, lval* items, lval* tail);

bool wile_do_eqv(lptr arg1, lptr arg2);

lval wile_read_line(lptr* clos, lptr args, const char* loc);
lval wile_read_line_interactive(lptr* clos, lptr args, const char* loc);

lval wile_parse_string(lptr* clos, lptr args, const char* loc);
lval wile_parse_file(lptr* clos, lptr args, const char* loc);

lval wile_regex_match(lptr* clos, lptr args, const char* loc);
lval wile_apply_function(lptr args, const char* loc);
lval wile_read_directory(lptr* clos, lptr args, const char* loc);
lval wile_char2string(lptr* clos, lptr args, const char* loc);
lval wile_listen_port(lptr* clos, lptr args, const char* loc);
lval wile_accept_connection(lptr* clos, lptr args, const char* loc);
lval wile_connect_to(lptr* clos, lptr args, const char* loc);
lval wile_gethostname(lptr* clos, lptr args, const char* loc);
lval wile_getdomainname(lptr* clos, lptr args, const char* loc);
lval wile_getcwd(lptr* clos, lptr args, const char* loc);
lval wile_cputime(lptr* clos, lptr args, const char* loc);
lval wile_filestat(lptr* clos, lptr args, const char* loc);
lval wile_symlinkstat(lptr* clos, lptr args, const char* loc);
lval wile_getuserinfo(lptr* clos, lptr args, const char* loc);
lval wile_getalluserinfo(lptr*, lptr args, const char* loc);
lval wile_getgroupinfo(lptr* clos, lptr args, const char* loc);
lval wile_getallgroupinfo(lptr*, lptr args, const char* loc);
lval wile_localtime(lptr* clos, lptr args, const char* loc);
lval wile_gmtime(lptr* clos, lptr args, const char* loc);
lval wile_closeport(lptr* clos, lptr args, const char* loc);
lval wile_flushport(lptr* clos, lptr args, const char* loc);
lval wile_setlinebuffering(lptr* clos, lptr args, const char* loc);
lval wile_setnobuffering(lptr* clos, lptr args, const char* loc);
lval wile_setfilepos2(lptr* clos, lptr args, const char* loc);
lval wile_setfilepos3(lptr* clos, lptr args, const char* loc);
lval wile_string_reverse(lptr* clos, lptr args, const char* loc);
lval wile_string_hash_32(lptr*, lptr, const char* loc);
lval wile_string_hash_64(lptr*, lptr, const char* loc);
lval wile_string_ci_hash_32(lptr*, lptr, const char* loc);
lval wile_string_ci_hash_64(lptr*, lptr, const char* loc);

// Log of Gamma function

lisp_cmplx_t logamma(lisp_cmplx_t z);

// Digamma function: logarithmic derivative of Gamma(x) = Gamma'(x)/Gamma(x)

lisp_cmplx_t digamma(lisp_cmplx_t z);

// Arithmetic-geometric mean

lisp_cmplx_t arith_geo_mean(lisp_cmplx_t a, lisp_cmplx_t g);

//  Compute the complete elliptic integrals
//
//  K(z) = integral_0^{pi/2} (1 - z*sin^2(t))^(-1/2) dt
//  E(z) = integral_0^{pi/2} (1 - z*sin^2(t))^(+1/2) dt
//
//  Note that the z is /not/ squared inside the integrand; this usage follows
//  Mathematica, but not the NIST Digital Library of Mathematical Functions.

lisp_cmplx_t elliptic_k(lisp_cmplx_t k);
lisp_cmplx_t elliptic_e(lisp_cmplx_t k);

// The two real branches of the Lambert W function: wp and wn.
// Initial guesses are from various expansions taken from Corless et al,
// "On the Lambert W Function"; the constants are wet-eyeball guesses.

// For non-negative x, the only real value of the Lambert W function;
// for -1/e < x < 0, the more-positive branch.

lisp_real_t lambert_wp_fn(lisp_real_t xl);

// For -1/e < x < 0, the more-negative branch of the Lambert W function

lisp_real_t lambert_wn_fn(lisp_real_t xl);

// All complex-valued branches of the Lambert W function, for all
// complex z; this includes as special values the two branches which
// happen to be real, above

lisp_cmplx_t lambert_wc_fn(int k, lisp_cmplx_t zl);

double sine_integral(double x);
double cosine_integral(double x);

lisp_int_t lgcd(lisp_int_t p, lisp_int_t q);

void show_kv_pair(const void* vp, FILE* fp, lisp_bytevector_t* bvp);

lptr clear_display_hook(const char* sym);
lptr get_display_hook(const char* sym);
lptr set_display_hook(const char* sym, lptr hook);

void wile_print_lisp_val(lptr vp, FILE* fp, const char* loc);
void wile_sprint_lisp_num(char* buf, size_t bsize, lptr num,
			  int base, int prec, bool psign);

lisp_loc_t wile_encode_line_loc(size_t lineno);
char* wile_decode_line_loc(lisp_loc_t lloc);
void wile_set_lisp_loc_file(const char* fname);
lisp_loc_t wile_get_lisp_loc(lptr vp);

const char* typename(enum val_type vt);

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

lval wile_expt(lval* a, lval* b, const char* loc);

lval wile_rand_normal_pair(lisp_real_t m, lisp_real_t s);

lval wile_cfft_good_n(lptr* clos, lptr args, const char* loc);
lval wile_cfft(lptr* clos, lptr args, const char* loc);

lval wile_sha256_wrap(bool is_256, lval input, const char* loc);
lval wile_sha256_init(bool is_256);
lval wile_sha256_update(lptr*, lptr, const char*);
lval wile_sha256_finish(lptr*, lptr args, const char* loc);

lval wile_waitpid(int pid, int opts);

// sqlite interface

lval wile_sql_open(const char* fname, int mode, const char* loc);
#ifdef WILE_USES_SQLITE
// have to hide this one because of the first argument:
// sqlite3* is unknown if we're not using sqlite
lval wile_sql_run(sqlite3* sqlite_conn, const char* cmd, const char* loc);
#endif // WILE_USES_SQLITE
lval wile_sql_stmt_prep(lptr* clos, lptr args, const char* loc);
lval wile_sql_stmt_clean(lptr* clos, lptr args, const char* loc);
lval wile_sql_stmt_info(lptr* clos, lptr args, const char* loc);
lval wile_sql_stmt_bind(lptr* clos, lptr args, const char* loc);
lval wile_sql_stmt_run(lptr* clos, lptr args, const char* loc);

// the infamous call-with-current-continuation

lval wile_call_cc(lptr* clos, lptr args, const char* loc);

lval wile_main(int argc, char** argv);

struct wile_profile_t {
    uint64_t count;
    char* name;
};

// This stuff is for checking that the configuration of the library is
// the same as the configuration of the main program: the library will
// contain a do-nothing routine WILE_CONFIG_SYM<N>, which is its
// configuration, and the main will call a routine WILE_CONFIG_SYM<N>
// which is its configuration. If they match, we're good

#define CAT(s1,s2)	s1 ## _ ## s2
#define XCAT(s1,s2)	CAT(s1,s2)

#if defined(WILE_USES_DOUBLE)
#define WILE_REAL_SYM		real_D
#elif  defined(WILE_USES_LONG_DOUBLE)
#define WILE_REAL_SYM		real_LD
#elif  defined(WILE_USES_QUAD_DOUBLE)
#define WILE_REAL_SYM		real_QD
#else
#error "wile real format was not specified!"
#endif // WILE_USES_DOUBLE

#define WILE_CONFIG_SYM1	XCAT(wile_config, WILE_REAL_SYM)

#if defined(WILE_USES_LONG_INT)
#define WILE_INT_SYM		int_LI
#elif  defined(WILE_USES_INT128)
#define WILE_INT_SYM		int_I128
#else
#error "wile int format was not specified!"
#endif // WILE_USES_LONG_INT

#define WILE_CONFIG_SYM2	XCAT(WILE_CONFIG_SYM1, WILE_INT_SYM)

#if defined(WILE_USES_GC)
#define WILE_GC_SYM		gc_Y
#else
#define WILE_GC_SYM		gc_N
#endif // WILE_USES_GC

#define WILE_CONFIG_SYM3	XCAT(WILE_CONFIG_SYM2, WILE_GC_SYM)

#if defined(WILE_USES_SQLITE)
#define WILE_SQLITE_SYM		sqlite_Y
#else
#define WILE_SQLITE_SYM		sqlite_N
#endif // WILE_USES_SQLITE

#define WILE_CONFIG_SYM4	XCAT(WILE_CONFIG_SYM3, WILE_SQLITE_SYM)

// Not used yet
#if 0
#if defined(WILE_USES_RC4)
#define WILE_RC4_SYM		rc4_Y
#else
#define WILE_RC4_SYM		rc4_N
#endif // WILE_USES_RC4

#define WILE_CONFIG_SYM5	XCAT(WILE_CONFIG_SYM4, WILE_RC4_SYM)
#endif // 0

void WILE_CONFIG_SYM4(void);

#endif // WILE_RUNTIME_H
