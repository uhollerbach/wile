// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// This file is part of the wile RTL and is licensed under LGPL
// version 3 or later; see file 'LICENSE-LGPL' for details.

#ifndef SHA256_H
#define SHA256_H

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#define SHA256_BYTE_ORDER		1234

#define SHA256_BLOCKSIZE		64

typedef struct {
    uint32_t digest[8];			// message digest
    uint64_t count;			// 64-bit bit count
    uint8_t data[SHA256_BLOCKSIZE];	// SHA data buffer
    int local;				// unprocessed amount in data
} SHA256_info;

void sha256_init(SHA256_info*);
void sha256_update(SHA256_info*, uint8_t*, uint64_t);
void sha256_final(unsigned char[32], SHA256_info*);

void sha256_stream(unsigned char[32], SHA256_info*, FILE*);
void sha256_print(unsigned char[32]);

#endif /* SHA256_H */
