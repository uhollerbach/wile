// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lisp_real_t pcheby1(int n, lisp_real_t x)
{
    int k;
    lisp_real_t pm, pc, pp;

    if (n == 0) {
	return 1.0;
    } else if (n == 1) {
	return x;
    } else {
	pm = 1.0;
	pc = x;
	x *= 2.0;
	for (k = 1; k < n; ++k) {
	    pp = x*pc - pm;
	    pm = pc;
	    pc = pp;
	}
	return pc;
    }
}

