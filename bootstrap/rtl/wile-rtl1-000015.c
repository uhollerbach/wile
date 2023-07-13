// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lisp_real_t phermite1(int n, lisp_real_t x)
{
    int k;
    lisp_real_t pm, pc, pp;

    if (n == 0) {
	return 1.0;
    } else if (n == 1) {
	return 2.0*x;
    } else {
	pm = 1.0;
	pc = 2.0*x;
	for (k = 1; k < n; ++k) {
	    pp = 2.0*x*pc - 2.0*k*pm;
	    pm = pc;
	    pc = pp;
	}
	return pc;
    }
}

