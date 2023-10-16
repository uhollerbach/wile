// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_cputime(lptr*, lptr)
{
    struct rusage usage;
    if (getrusage(RUSAGE_SELF, &usage) == 0) {
	lval vs[2];
	vs[0] = LVI_REAL(usage.ru_utime.tv_sec + 1.0e-6*usage.ru_utime.tv_usec);
	vs[1] = LVI_REAL(usage.ru_stime.tv_sec + 1.0e-6*usage.ru_stime.tv_usec);
	return wile_gen_list(2, vs, NULL);
    } else {
	return LVI_BOOL(false);
    }
}

