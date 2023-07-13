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

