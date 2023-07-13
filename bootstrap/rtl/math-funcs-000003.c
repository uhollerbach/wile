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

