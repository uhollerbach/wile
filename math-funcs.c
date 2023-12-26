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

// --8><----8><----8><--

// log of Gamma function

lisp_cmplx_t logamma(lisp_cmplx_t z)
{
    static const lisp_real_t c[] = {
	1.0L/12.0L,
	-1.0L/360.0L,
	1.0L/1260.0L,
	-1.0L/1680.0L,
	1.0L/1188.0L,
	-691.0L/360360.0L,
	1.0L/156.0L
    };

    lisp_cmplx_t ret, y;

    if (CREAL(z) < 0.5) {
	ret = CLOG(PI_L/CSIN(PI_L*z)) - logamma(1.0 - z);
    } else {
	ret = 0.0L;
	while (CREAL(z) < 15.0L) {
	    ret -= CLOG(z);
	    z += 1.0L;
	}
	y = 1.0/(z*z);
	ret += z*CLOG(z) - z - 0.5L*CLOG(z/(2.0L*PI_L))
	    + (c[0] + y*(c[1] + y*(c[2] + y*(c[3] + y*(c[4] + y*(c[5] + y*c[6]))))))/z;
    }
    return ret;
}

// --8><----8><----8><--

// Digamma function: logarithmic derivative of Gamma(x) = Gamma'(x)/Gamma(x)

lisp_cmplx_t digamma(lisp_cmplx_t z)
{
    lisp_cmplx_t ret, y;
    static const lisp_real_t c[] = {	// B_{2n}/(2*n) for n = 1 ...
	-1.0L/12.0L,
	1.0L/120.0L,
	-1.0L/252.0L,
	1.0L/240.0L,
	-1.0L/132.0L,
	691.0L/32760.0L,
	-1.0L/12.0L
    };

    ret = 0.0L;
    if (CREAL(z) < 0.5) {
	ret -= PI_L/CTAN(PI_L*z);
	z = 1.0L - z;
    }
    while (CREAL(z) < 15.0L) {
	ret -= 1.0L/z;
	z += 1.0L;
    }
    y = 1.0L/(z*z);
    ret += CLOG(z) - 0.5L/z
	+ y*(c[0] + y*(c[1] + y*(c[2] + y*(c[3] + y*(c[4] + y*(c[5] + y*c[6]))))));
    return ret;
}

// --8><----8><----8><--

// AGM is currently inlined in wile-prims, so this never gets used;
// but it's a separate object file, so it doesn't contribute to exe size.
// leave it for documentation and to keep it with the elliptic stuff

// Arithmetic-geometric mean

lisp_cmplx_t arith_geo_mean(lisp_cmplx_t a, lisp_cmplx_t g)
{
    lisp_cmplx_t an;

    while (CABS(a - g) > 0.5L*REAL_EPSILON*(CABS(a) + CABS(g))) {
	an = (a + g)*0.5L;
	g = CSQRT(a*g);
	a = an;
    }
    return a;
}

// --8><----8><----8><--

//  Compute the complete elliptic integrals
//
//  K(z) = integral_0^{pi/2} (1 - z*sin^2(t))^(-1/2) dt
//  E(z) = integral_0^{pi/2} (1 - z*sin^2(t))^(+1/2) dt
//
//  Note that the z is /not/ squared inside the integrand; this usage follows
//  Mathematica, but not the NIST Digital Library of Mathematical Functions.

lisp_cmplx_t elliptic_k(lisp_cmplx_t k)
{
    lisp_cmplx_t a, g, an;

    if (k == 1.0L) {
	return REAL_NAN;
    }

    a = 1.0L;
    g = CSQRT(1.0L - k);
    while (CABS(a - g) > 0.5L*REAL_EPSILON*(CABS(a) + CABS(g))) {
	an = (a + g)*0.5L;
	g = CSQRT(a*g);
	a = an;
    }
    return PI_L/(2.0*a);
}

// --8><----8><----8><--

// If required, this function can be trivially modified to compute both
// the elliptic-k and elliptic-e integrals: at the bottom, simply compute
// elliptic-k = PI_HALF/a, just as in the elliptic_k routine: the AGM
// calculations are exactly the same.

lisp_cmplx_t elliptic_e(lisp_cmplx_t k)
{
    lisp_cmplx_t a, g, an, sum, c;
    lisp_real_t p2;

    if (k == 1.0L) {
	return 1.0L;
    }

    a = 1.0L;
    g = CSQRT(1.0L - k);
    an = (a + g)*0.5L;
    g = CSQRT(a*g);
    a = an;
    sum = a*a;
    p2 = 2.0L;

    while (CABS(a - g) > 0.5L*REAL_EPSILON*(CABS(a) + CABS(g))) {
	c = 0.5L*(a - g);
	sum -= p2*c*c;
	p2 *= 2.0L;
	an = (a + g)*0.5L;
	g = CSQRT(a*g);
	a = an;
    }
    return PI_L*sum/(2.0L*a);
}

// --8><----8><----8><--

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

// --8><----8><----8><--

// Many thanks to the anonymous editor of the wikipedia page
//
//	http://en.wikipedia.org/wiki/Trigonometric_integral
//
// who posted these functions!

static double aux_f(double x)
{
    double y, v;

    y = 1.0/(x*x);
    v = (1.0 +
	 y*(7.44437068161936700618e2 +
	    y*(1.96396372895146869801e5 +
	       y*(2.37750310125431834034e7 +
		  y*(1.43073403821274636888e9 +
		     y*(4.33736238870432522765e10 +
			y*(6.40533830574022022911e11 +
			   y*(4.20968180571076940208e12 +
			      y*(1.00795182980368574617e13 +
				 y*(4.94816688199951963482e12 +
				    y*(-4.94701168645415959931e11)))))))))))
        / (x*(1.0 +
              y*(7.46437068161927678031e2 +
                 y*(1.97865247031583951450e5 +
                    y*(2.41535670165126845144e7 +
                       y*(1.47478952192985464958e9 +
                          y*(4.58595115847765779830e10 +
                             y*(7.08501308149515401563e11 +
                                y*(5.06084464593475076774e12 +
                                   y*(1.43468549171581016479e13 +
                                      y*(1.11535493509914254097e13)))))))))));
    return v;
}

static double aux_g(double x)
{
    double y, v;

    y = 1.0/(x*x);
    v = y*(1.0 +
	   y*(8.1359520115168615e2 +
	      y*(2.35239181626478200e5 +
		 y*(3.12557570795778731e7 +
		    y*(2.06297595146763354e9 +
		       y*(6.83052205423625007e10 +
			  y*(1.09049528450362786e12 +
			     y*(7.57664583257834349e12 +
				y*(1.81004487464664575e13 +
				   y*(6.43291613143049485e12 +
				      y*(-1.36517137670871689e12)))))))))))
	/ (1.0 +
	   y*(8.19595201151451564e2 +
	      y*(2.40036752835578777e5 +
		 y*(3.26026661647090822e7 +
		    y*(2.23355543278099360e9 +
		       y*(7.87465017341829930e10 +
			  y*(1.39866710696414565e12 +
			     y*(1.17164723371736605e13 +
				y*(4.01839087307656620e13 +
				   y*(3.99653257887490811e13))))))))));
    return v;
}

double sine_integral(double x)
{
    double x2, v;

    if (x <= 4.0) {
	x2 = x*x;
	v = x*(1.0 +
	       x2*(-4.54393409816329991e-2 +
		   x2*(1.15457225751016682e-3 +
		       x2*(-1.41018536821330254e-5 +
			   x2*(9.43280809438713025e-8 +
			       x2*(-3.53201978997168357e-10 +
				   x2*(7.08240282274875911e-13 +
				       x2*(-6.05338212010422477e-16))))))))
	    / (1.0 +
	       x2*(1.01162145739225565e-2 +
		   x2*(4.99175116169755106e-5 +
		       x2*(1.55654986308745614e-7 +
			   x2*(3.28067571055789734e-10 +
			       x2*(4.5049097575386581e-13 +
				   x2*(3.21107051193712168e-16)))))));
    } else {
	v = PI_L/2.0 - aux_f(x)*cos(x) - aux_g(x)*sin(x);
    }
    return v;
}

double cosine_integral(double x)
{
    double x2, v;

    if (x <= 4.0) {
	x2 = x*x;
	v = 0.577215664901532860606512 + log(x) +
	    x2*(-0.25 +
		x2*(7.51851524438898291e-3 +
		    x2*(-1.27528342240267686e-4 +
			x2*(1.05297363846239184e-6 +
			    x2*(-4.68889508144848019e-9 +
				x2*(1.06480802891189243e-11 +
				    x2*(-9.93728488857585407e-15)))))))
	    / (1.0 +
	       x2*(1.1592605689110735e-2 +
		   x2*(6.72126800814254432e-5 +
		       x2*(2.55533277086129636e-7 +
			   x2*(6.97071295760958946e-10 +
			       x2*(1.38536352772778619e-12 +
				   x2*(1.89106054713059759e-15 +
				       x2*(1.39759616731376855e-18))))))));
    } else {
	v = aux_f(x)*sin(x) - aux_g(x)*cos(x);
    }
    return v;
}

// --8><----8><----8><--

// compute a^b, (hopefully) taking into account most of the special cases

lval wile_expt(lval* a, lval* b, const char* loc)
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
	wile_exception("expt", loc, "expects two numeric arguments");
	break;
    }

    if (res.vt == LV_CMPLX && CIMAG(res.v.cv) == 0.0) {
	res = LVI_REAL(CREAL(res.v.cv));
    }
    return res;
}
