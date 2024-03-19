// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_bytevector_hash_64(lptr* clos, lptr args, const char* loc)
{
    uint64_t hash;
    size_t i;

    if (args[0].vt != LV_BVECTOR) {
	wile_exception("bytevector-hash-64",
		       loc, "expects a bytevector argument");
    }

    hash = 14695981039346656037UL;
    for (i = 0; i < args[0].v.bvec.capa; ++i) {
	hash ^= args[0].v.bvec.arr[i];
	hash *= 1099511628211UL;
    }
    return LVI_INT(hash);
}

