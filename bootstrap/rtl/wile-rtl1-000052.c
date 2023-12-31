// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_string_ci_hash_64(lptr* clos, lptr args, const char* loc)
{
    uint64_t hash;
    size_t i, n_os;

    if (args[0].vt != LV_STRING) {
	wile_exception("string-ci-hash-64", loc, "expects a string argument");
    }
    n_os = strlen(args[0].v.str);
    hash = 14695981039346656037UL;
    for (i = 0; i < n_os; ++i) {
	hash ^= (unsigned char) tolower(args[0].v.str[i]);
	hash *= 1099511628211UL;
    }
    return LVI_INT(hash);
}

