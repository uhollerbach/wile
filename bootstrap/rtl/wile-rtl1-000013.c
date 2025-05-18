// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lisp_real_t plegendre(int n, lisp_real_t x)
{
    int k;
    lisp_real_t a, pm, pc, pp;

    if (n == 0) {
	return 1.0;
    } else if (n == 1) {
	return x;
    } else {
	pm = 1.0;
	pc = x;
	for (k = 1; k < n; ++k) {
	    a = ((lisp_real_t) k)/((lisp_real_t) (k + 1));
	    pp = (1.0 + a)*x*pc - a*pm;
	    pm = pc;
	    pc = pp;
	}
	return pc;
    }
}

