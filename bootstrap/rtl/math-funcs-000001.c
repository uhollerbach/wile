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

