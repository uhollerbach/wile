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

