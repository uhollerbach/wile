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

// --8><----8><----8><--

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

// --8><----8><----8><--

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
