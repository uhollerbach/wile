// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_getcwd(lptr*, lptr, const char*)
{
    char str[1+PATH_MAX], *sp;
    sp = getcwd(str, sizeof(str));
    if (sp) {
	return LVI_STRING(sp);
    } else {
	return LVI_BOOL(false);
    }
}

