// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


uint16_t wile_binfo(void)
{
    int ret = 0, shift = 0;

    // bit 0: GC or no GC?

#ifdef WILE_USES_GC
    ret |= (1 << shift);
#endif
    shift += 1;

    // bit 1: 0 = drand48, 1 = RC4-rand

#ifdef WILE_USES_RC4_RAND
    ret |= (1 << shift);
#endif
    shift += 1;

    // bit 2: sqlite or no sqlite?

#ifdef WILE_USES_SQLITE
    ret |= (1 << shift);
#endif
    shift += 1;

    // bits 3-4: 0 = plain old double, 1 = long double, 2 = quad double

#ifdef WILE_USES_LONG_DOUBLE
    ret |= (1 << shift);
#elif WILE_USES_QUAD_DOUBLE
    ret |= (2 << shift);
#endif
    shift += 2;

    // bits 5-6: 0 = plain old long int, 1 = int128, 2 = bigint

#ifdef WILE_USES_INT128
    ret |= (1 << shift);
#elif WILE_USES_BIGINT
    ret |= (2 << shift);
#endif
    shift += 2;

    return ret;
}

