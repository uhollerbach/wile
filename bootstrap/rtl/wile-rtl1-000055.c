// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


// lower-level SHA functions

#include "sha256.h"

lval wile_sha256_init(lptr*, lptr, const char*)
{
    lval ret;

    ret.vt = LV_SHA256_DATA;
    ret.v.sha256_info = LISP_ALLOC(SHA256_info, 1);
    sha256_init(ret.v.sha256_info);

    return ret;
}

lval wile_sha256_update(lptr*, lptr args, const char* loc)
{
    if (args[0].vt != LV_SHA256_DATA ||
	(args[1].vt != LV_STRING && args[1].vt != LV_BVECTOR)) {
	wile_exception("sha-256-update", loc,
		       "expects a SHA-256 data structure and a string or bytevector");
    }
    if (args[1].vt == LV_STRING) {
	sha256_update(args[0].v.sha256_info, (uint8_t*) args[1].v.str,
		      strlen(args[1].v.str));
    } else {
	sha256_update(args[0].v.sha256_info, args[1].v.bvec.arr,
		      args[1].v.bvec.capa);
    }

    return LVI_BOOL(true);
}

lval wile_sha256_finish(lptr*, lptr args, const char* loc)
{
    int i;
    unsigned char digest[32];
    char hdig[65];

    if (args[0].vt != LV_SHA256_DATA) {
	wile_exception("sha-256-finish", loc,
		       "expects a SHA-256 data structure");
    }
    sha256_final(digest, args[0].v.sha256_info);
    for (i = 0; i < 32; ++i) {
	snprintf(hdig + 2*i, 3, "%02x", digest[i]);
    }
    hdig[64] = '\0';	// should be taken care of by snprintf, but make sure

    return LVI_STRING(hdig);
}

