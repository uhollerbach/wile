// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_string_hash_32(lptr* clos, lptr args, const char* loc)
{
    uint32_t hash;
    size_t i, n_os;

    if (args[0].vt != LV_STRING) {
	wile_exception("string-hash-32", loc, "expects a string argument");
    }
    n_os = strlen(args[0].v.str);
    hash = 2166136261U;
    for (i = 0; i < n_os; ++i) {
	hash ^= (unsigned char) (args[0].v.str[i]);
	hash *= 16777619U;
    }
    return LVI_INT(hash);
}

