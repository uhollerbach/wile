// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lisp_int_t powi(lisp_int_t a, lisp_int_t b)
{
    lisp_int_t p;

    p = 1;
    while (b) {
	if (b%2) {
	    p *= a;
	}
	b /= 2;
	a = a*a;
    }
    return p;
}

