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

lval wile_sha256_wrap(bool is_256, lval input)
{
    int i, lim;
    uint8_t data[BLOCK_SIZE];
    unsigned char digest[32];
    char hdig[65];
    SHA256_info sha_info;

    sha256_init(&sha_info, is_256);
    if (input.vt == LV_STRING) {
	sha256_update(&sha_info, (uint8_t*) input.v.str, strlen(input.v.str));
    } else if (input.vt == LV_FILE_PORT ||
	       input.vt == LV_PIPE_PORT ||
	       input.vt == LV_SOCK_PORT) {
	while ((i = fread(data, 1, BLOCK_SIZE, input.v.fp)) > 0) {
	    sha256_update(&sha_info, (uint8_t*) data, i);
	}
    } else {
	wile_exception(is_256 ? "sha-256" : "sha-224",
		       LISP_WHENCE, "expects a string or port argument");
    }
    sha256_final(digest, &sha_info);
    lim = is_256 ? 32 : 28;
    for (i = 0; i < lim; ++i) {
	snprintf(hdig + 2*i, 3, "%02x", digest[i]);
    }
    hdig[2*lim] = '\0';	// should be taken care of by snprintf, but make sure

    return LVI_STRING(hdig);
}

