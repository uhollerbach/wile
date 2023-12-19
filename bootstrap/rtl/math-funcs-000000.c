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

