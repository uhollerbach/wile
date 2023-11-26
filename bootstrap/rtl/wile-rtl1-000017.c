// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


void trunc_qr(lisp_int_t n1, lisp_int_t n2, lisp_int_t* nq, lisp_int_t* nr)
{
    if (n2 == 0) {
	wile_exception("trunc_qr", LISP_WHENCE, "division by zero!");
    }
    *nq = n1/n2;
    *nr = n1 - *nq*n2;
}

