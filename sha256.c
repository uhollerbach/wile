// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// This file is part of the wile RTL and is licensed under LGPL
// version 3 or later; see file 'LICENSE-LGPL' for details.

// A couple of test strings:
//
// empty string "" ->
// e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
//
// "abc" ->
// ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
//
// "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" ->
// 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1
//
// "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu" ->
// cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1
//
// 1000000 repetitions of letter 'a' ->
// cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0
//
// 16777216 repetitions of
// "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno" ->
// 50e72a0e26442fe2552dc3938ac58658228c0cbfb1d2ca872ae435266fcd055e
//
// IF THIS CODE DOES NOT REPRODUCE THE ABOVE VALUES, THE FIRST PLACE
// TO LOOK IS THE BYTE ORDER! See below and in sha256.h

#include <string.h>
#include "sha256.h"

// SHA256 constants

static const uint32_t sha256_const[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
    0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
    0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

// 32-bit right-rotate

#define R32(x,n)	((x >> n) | (x << (32 - n)))

// do SHA256 transformation

static void sha256_transform(SHA256_info* sha_info)
{
    int i;
    uint8_t* dp;
    uint32_t T1, T2, S0, S1, ch, maj, A, B, C, D, E, F, G, H, W[64];

    dp = sha_info->data;

    for (i = 0; i < 16; ++i) {
	A = *((uint32_t*) dp);
	dp += 4;
#undef SWAP_DONE
#if (SHA256_BYTE_ORDER == 1234)
#define SWAP_DONE
	W[i] =  ((A << 24) & 0xff000000) | ((A <<  8) & 0x00ff0000) |
		((A >>  8) & 0x0000ff00) | ((A >> 24) & 0x000000ff);
#endif // SHA256_BYTE_ORDER == 1234
#if (SHA256_BYTE_ORDER == 4321)
#define SWAP_DONE
	W[i] = A;
#endif // SHA256_BYTE_ORDER == 4321
#ifndef SWAP_DONE
#error Unknown byte order -- you need to add code here
#endif // SWAP_DONE
    }

    for (i = 16; i < 64; ++i) {
	A = W[i-15];
	B = R32(A, 7) ^ R32(A, 18) ^ (A >> 3);
	C = W[i-2];
	D = R32(C, 17) ^ R32(C, 19) ^ (C >> 10);
	W[i] = W[i-16] + B + W[i-7] + D;
    }

    A = sha_info->digest[0];
    B = sha_info->digest[1];
    C = sha_info->digest[2];
    D = sha_info->digest[3];
    E = sha_info->digest[4];
    F = sha_info->digest[5];
    G = sha_info->digest[6];
    H = sha_info->digest[7];

    for (i =  0; i < 64; ++i) {
	S1 = R32(E, 6) ^ R32(E, 11) ^ R32(E, 25);
	ch = (E & F) ^ ((~E) & G);
	T1 = H + S1 + ch + sha256_const[i] + W[i];
	S0 = R32(A, 2) ^ R32(A, 13) ^ R32(A, 22);
	maj = (A & B) ^ (A & C) ^ (B & C);
	T2 = S0 + maj;
	H = G;
        G = F;
        F = E;
        E = D + T1;
	D = C;
        C = B;
        B = A;
        A = T1 + T2;
    }

    sha_info->digest[0] += A;
    sha_info->digest[1] += B;
    sha_info->digest[2] += C;
    sha_info->digest[3] += D;
    sha_info->digest[4] += E;
    sha_info->digest[5] += F;
    sha_info->digest[6] += G;
    sha_info->digest[7] += H;
}

// initialize the SHA digest

void sha256_init(SHA256_info* sha_info, bool is_256)
{
    sha_info->is_256 = is_256;

    if (is_256) {
	sha_info->digest[0] = 0x6a09e667;
	sha_info->digest[1] = 0xbb67ae85;
	sha_info->digest[2] = 0x3c6ef372;
	sha_info->digest[3] = 0xa54ff53a;
	sha_info->digest[4] = 0x510e527f;
	sha_info->digest[5] = 0x9b05688c;
	sha_info->digest[6] = 0x1f83d9ab;
	sha_info->digest[7] = 0x5be0cd19;
    } else {
	sha_info->digest[0] = 0xc1059ed8;
	sha_info->digest[1] = 0x367cd507;
	sha_info->digest[2] = 0x3070dd17;
	sha_info->digest[3] = 0xf70e5939;
	sha_info->digest[4] = 0xffc00b31;
	sha_info->digest[5] = 0x68581511;
	sha_info->digest[6] = 0x64f98fa7;
	sha_info->digest[7] = 0xbefa4fa4;
    }

    sha_info->count = 0;
    sha_info->local = 0;
}

// update the SHA digest

void sha256_update(SHA256_info* sha_info, uint8_t* buffer, uint64_t count)
{
    uint64_t i;

    sha_info->count += count << 3;
    if (sha_info->local) {
	i = SHA256_BLOCKSIZE - sha_info->local;
	if (i > count) {
	    i = count;
	}
	memcpy(sha_info->data + sha_info->local, buffer, i);
	count -= i;
	buffer += i;
	sha_info->local += i;
	if (sha_info->local == SHA256_BLOCKSIZE) {
	    sha256_transform(sha_info);
	} else {
	    return;
	}
    }
    while (count >= SHA256_BLOCKSIZE) {
	memcpy(sha_info->data, buffer, SHA256_BLOCKSIZE);
	buffer += SHA256_BLOCKSIZE;
	count -= SHA256_BLOCKSIZE;
	sha256_transform(sha_info);
    }
    memcpy(sha_info->data, buffer, count);
    sha_info->local = count;
}

// finish computing the SHA digest

void sha256_final(unsigned char digest[32], SHA256_info* sha_info)
{
    int count;
    uint64_t bit_count;

    bit_count = sha_info->count;
    count = (int) ((bit_count >> 3) & 0x3f);
    sha_info->data[count++] = 0x80;
    if (count > SHA256_BLOCKSIZE - 8) {
	memset(sha_info->data + count, 0, SHA256_BLOCKSIZE - count);
	sha256_transform(sha_info);
	memset(sha_info->data, 0, SHA256_BLOCKSIZE - 8);
    } else {
	memset(sha_info->data + count, 0, SHA256_BLOCKSIZE - 8 - count);
    }
    sha_info->data[56] = (unsigned char) ((bit_count >> 56) & 0xff);
    sha_info->data[57] = (unsigned char) ((bit_count >> 48) & 0xff);
    sha_info->data[58] = (unsigned char) ((bit_count >> 40) & 0xff);
    sha_info->data[59] = (unsigned char) ((bit_count >> 32) & 0xff);
    sha_info->data[60] = (unsigned char) ((bit_count >> 24) & 0xff);
    sha_info->data[61] = (unsigned char) ((bit_count >> 16) & 0xff);
    sha_info->data[62] = (unsigned char) ((bit_count >>  8) & 0xff);
    sha_info->data[63] = (unsigned char) ((bit_count >>  0) & 0xff);

    sha256_transform(sha_info);

    digest[ 0] = (unsigned char) ((sha_info->digest[0] >> 24) & 0xff);
    digest[ 1] = (unsigned char) ((sha_info->digest[0] >> 16) & 0xff);
    digest[ 2] = (unsigned char) ((sha_info->digest[0] >>  8) & 0xff);
    digest[ 3] = (unsigned char) ((sha_info->digest[0]      ) & 0xff);

    digest[ 4] = (unsigned char) ((sha_info->digest[1] >> 24) & 0xff);
    digest[ 5] = (unsigned char) ((sha_info->digest[1] >> 16) & 0xff);
    digest[ 6] = (unsigned char) ((sha_info->digest[1] >>  8) & 0xff);
    digest[ 7] = (unsigned char) ((sha_info->digest[1]      ) & 0xff);

    digest[ 8] = (unsigned char) ((sha_info->digest[2] >> 24) & 0xff);
    digest[ 9] = (unsigned char) ((sha_info->digest[2] >> 16) & 0xff);
    digest[10] = (unsigned char) ((sha_info->digest[2] >>  8) & 0xff);
    digest[11] = (unsigned char) ((sha_info->digest[2]      ) & 0xff);

    digest[12] = (unsigned char) ((sha_info->digest[3] >> 24) & 0xff);
    digest[13] = (unsigned char) ((sha_info->digest[3] >> 16) & 0xff);
    digest[14] = (unsigned char) ((sha_info->digest[3] >>  8) & 0xff);
    digest[15] = (unsigned char) ((sha_info->digest[3]      ) & 0xff);

    digest[16] = (unsigned char) ((sha_info->digest[4] >> 24) & 0xff);
    digest[17] = (unsigned char) ((sha_info->digest[4] >> 16) & 0xff);
    digest[18] = (unsigned char) ((sha_info->digest[4] >>  8) & 0xff);
    digest[19] = (unsigned char) ((sha_info->digest[4]      ) & 0xff);

    digest[20] = (unsigned char) ((sha_info->digest[5] >> 24) & 0xff);
    digest[21] = (unsigned char) ((sha_info->digest[5] >> 16) & 0xff);
    digest[22] = (unsigned char) ((sha_info->digest[5] >>  8) & 0xff);
    digest[23] = (unsigned char) ((sha_info->digest[5]      ) & 0xff);

    digest[24] = (unsigned char) ((sha_info->digest[6] >> 24) & 0xff);
    digest[25] = (unsigned char) ((sha_info->digest[6] >> 16) & 0xff);
    digest[26] = (unsigned char) ((sha_info->digest[6] >>  8) & 0xff);
    digest[27] = (unsigned char) ((sha_info->digest[6]      ) & 0xff);

    if (sha_info->is_256) {
	digest[28] = (unsigned char) ((sha_info->digest[7] >> 24) & 0xff);
	digest[29] = (unsigned char) ((sha_info->digest[7] >> 16) & 0xff);
	digest[30] = (unsigned char) ((sha_info->digest[7] >>  8) & 0xff);
	digest[31] = (unsigned char) ((sha_info->digest[7]      ) & 0xff);
    } else {
	digest[28] = 0x00;
	digest[29] = 0x00;
	digest[30] = 0x00;
	digest[31] = 0x00;
    }
}
