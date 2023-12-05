// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


// compute the SHA digest of a string or FILE* stream

#include "sha256.h"

#define BLOCK_SIZE	8192

lval wile_sha256_wrap(lptr*, lptr args, const char* loc)
{
    int i;
    uint8_t data[BLOCK_SIZE];
    unsigned char digest[32];
    char hdig[65];
    SHA256_info sha_info;

    sha256_init(&sha_info);
    if (args[0].vt == LV_STRING) {
	sha256_update(&sha_info, (uint8_t*) args[0].v.str,
		      strlen(args[0].v.str));
    } else if (args[0].vt == LV_FILE_PORT ||
	       args[0].vt == LV_PIPE_PORT ||
	       args[0].vt == LV_SOCK_PORT) {
	while ((i = fread(data, 1, BLOCK_SIZE, args[0].v.fp)) > 0) {
	    sha256_update(&sha_info, (uint8_t*) data, i);
	}
    } else {
	wile_exception("sha-256", loc, "expects a string or port argument");
    }
    sha256_final(digest, &sha_info);
    for (i = 0; i < 32; ++i) {
	snprintf(hdig + 2*i, 3, "%02x", digest[i]);
    }
    hdig[64] = '\0';	// should be taken care of by snprintf, but make sure

    return LVI_STRING(hdig);
}

