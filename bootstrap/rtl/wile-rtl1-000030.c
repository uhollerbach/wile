// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_rand_normal_pair(lisp_real_t m, lisp_real_t s)
{
    while (1) {
	lisp_real_t v1, v2, r2;
	v1 = 2.0*wile_rand_dbl() - 1.0;
	v2 = 2.0*wile_rand_dbl() - 1.0;
	r2 = v1*v1 + v2*v2;
	if (r2 > 0.0 && r2 < 1.0) {
	    lval vs[2];
	    s *= SQRT(-2.0*LOG(r2)/r2);
	    vs[0] = LVI_REAL(m + s*v1);
	    vs[1] = LVI_REAL(m + s*v2);
	    return wile_gen_list(2, vs, NULL);
	}
    }
}

