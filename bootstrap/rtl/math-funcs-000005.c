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


/* The two real branches of the Lambert W function: wp and wn.
   Initial guesses are from various expansions taken from Corless et al,
   "On the Lambert W Function"; the constants are wet-eyeball guesses. */

/* I have not seen more than 4 iterations required; 8 is probably a
   safe limit. Corless et al comment that Halley's method is faster, but
   for fixed precision the difference is very marginal: the initial
   guesses are more important. I also observed some extremely limited
   flakiness with Halley's method, only for very large arguments, so
   decided to stick with Newton's method. I did not try this with long
   doubles as in the current code, that might have worked better...
   in the complex version, that seems to be the case. */

// Halley update formula
// dw = (wew - xl)/(wew + ew - 0.5L*(wc + 2.0L)*(wew - xl)/(wc + 1.0L));

#define NEWTON_ITER()						\
    do {							\
	int i;							\
	lisp_real_t ew, dw, wew;				\
	for (i = 0; i < 8; ++i) {				\
	    ew = EXP(wc);					\
	    wew = wc*ew;					\
	    /* Newton update formula */				\
	    dw = (wew - xl)/(wew + ew);				\
	    wc -= dw;						\
	    if (FABS(dw) < REAL_EPSILON*FABS(wc)) {		\
		return wc;					\
	    }							\
	}							\
	return wc;	/* just in case */			\
    } while (0)

// For non-negative x, the only real value of the Lambert W function;
// for -1/e < x < 0, the more-positive branch.

lisp_real_t lambert_wp_fn(lisp_real_t xl)
{
    lisp_real_t wc, t;

    if (xl < -EINV_L) {
	return REAL_NAN;
    } else if (FABS(xl) < 1.0e-5L) {
    	return xl*(1.0L + xl*(-1.0L + xl*(1.5L + xl*(-8.0L/3.0L))));
    } else {
	if (xl > 3.5L) {
	    t = LOG(xl);
	    wc = t - LOG(t) + 0.25L;
	} else if (xl > -0.25L) {
	    // this is an extremely arbitrary guess; the only
	    // justification is that it works pretty well.
	    wc = 1.4L*CBRT(xl + EINV_L) - 1.0L;
	} else {
	    t = SQRT(2.0L*(1.0L + xl/EINV_L));
	    wc = -1.0L + t*(1.0L + t*(-1.0L/3.0L + t*(11.0L/72.0L)));
	}
	NEWTON_ITER();
    }
}

// For -1/e < x < 0, the more-negative branch of the Lambert W function

lisp_real_t lambert_wn_fn(lisp_real_t xl)
{
    lisp_real_t wc, t;

    if (xl < -EINV_L || xl >= 0.0) {
	return REAL_NAN;
    } else {
	if (xl > -0.25L) {
	    t = LOG(-xl);
	    wc = t - LOG(-t) - 0.4L;
	} else {
	    t = -SQRT(2.0L*(1.0L + xl/EINV_L));
	    wc = -1.0L + t*(1.0L + t*(-1.0L/3.0L + t*(11.0L/72.0L)));
	}
	NEWTON_ITER();
    }
}

// All complex-valued branches of the Lambert W function, for all
// complex z; this includes as special values the two branches which
// happen to be real, above

lisp_cmplx_t lambert_wc_fn(int k, lisp_cmplx_t zl)
{
    int i;
    lisp_cmplx_t wc, l1, l2, ew, dw, wew;

    if (CREAL(zl) == 0.0 && CIMAG(zl) == 0.0) {
	return (k == 0) ? 0.0 : REAL_NAN;
    }

    if (k == 0 &&
	CREAL(zl) > -0.5 &&
	CREAL(zl) < 3.0 &&
	FABS(CIMAG(zl)) < 1.5) {
	/* an extremely arbitrary guess... but it seems to work pretty well */
	wc = CABS(zl)*CEXP(I*ATAN2(CIMAG(zl), CREAL(zl))/PI_L);
    } else {
	l1 = CLOG(zl) + 2.0L*PI_L*k*I;
	l2 = CLOG(l1);
	wc = l1 - l2 + l2/l1 + (l2*(l2 - 2.0L))/(2.0L*l1*l1);
    }

    /* 20 is a very conservative limit, most of the time this
       should take 2 to 4 when using the Halley update formula */

    for (i = 0; i < 20; ++i) {
	ew = CEXP(wc);
	wew = wc*ew;
#if 0
	/* Newton update formula */
	dw = (wew - zl)/(wew + ew);
#else
	/* Halley update formula */
	dw = (wew - zl)/(wew + ew - 0.5L*(wc + 2.0L)*(wew - zl)/(wc + 1.0L));
#endif
	wc -= dw;
	if (CABS(dw) < REAL_EPSILON*CABS(wc)) {
	    return wc;		/* normal return */
	}
    }

    char buf1[64], buf2[64], buf3[64], buf4[64];

#if defined(WILE_USES_QUAD_DOUBLE)
    quadmath_snprintf(buf1, sizeof(buf1), "%.15Qe", CREAL(zl));
    quadmath_snprintf(buf2, sizeof(buf2), "%.15Qe", CIMAG(zl));
    quadmath_snprintf(buf3, sizeof(buf3), "%.15Qe", CREAL(dw));
    quadmath_snprintf(buf4, sizeof(buf4), "%.15Qe", CIMAG(dw));
#elif defined(WILE_USES_LONG_DOUBLE)
    snprintf(buf1, sizeof(buf1), "%.15Le", CREAL(zl));
    snprintf(buf2, sizeof(buf2), "%.15Le", CIMAG(zl));
    snprintf(buf3, sizeof(buf3), "%.15Le", CREAL(dw));
    snprintf(buf4, sizeof(buf4), "%.15Le", CIMAG(dw));
#else
    snprintf(buf1, sizeof(buf1), "%.15e", CREAL(zl));
    snprintf(buf2, sizeof(buf2), "%.15e", CIMAG(zl));
    snprintf(buf3, sizeof(buf3), "%.15e", CREAL(dw));
    snprintf(buf4, sizeof(buf4), "%.15e", CIMAG(dw));
#endif

    fprintf(stderr,
	    "lambert-W exhausted iterations! %d %s %s %s %s\n",
	    k, buf1, buf2, buf3, buf4);
    return wc;			/* just in case */
}

