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

#if defined(WILE_USES_LONG_DOUBLE)
    ret |= (1 << shift);
#elif defined(WILE_USES_QUAD_DOUBLE)
    ret |= (2 << shift);
#endif
    shift += 2;

    // bits 5-6: 0 = plain old long int, 1 = int128, 2 = bigint

#if defined(WILE_USES_INT128)
    ret |= (1 << shift);
#elif defined(WILE_USES_BIGINT)
    ret |= (2 << shift);
#endif
//    shift += 2;

    return ret;
}

lval wile_os_name(void)
{
#if defined(__linux__)
    return LVI_STRING("GNU/Linux");
#elif defined(__OpenBSD__)
    return LVI_STRING("OpenBSD");
#elif defined(__CYGWIN__)
    return LVI_STRING("Cygwin");
#elif defined(__APPLE__)
    // TODO: what's the right thing here for Apple operating systems?
    return LVI_STRING("MacOS");
#else
    return LVI_STRING("Unknown-OS");
#endif
}

// get machine architecture; original found in approximately this form
// on stackexchange, coded by Freak -- thank you. these are all hugely
// aspirational, and entirely untested except for the first

lval wile_arch_name(void)
{
#if defined(__x86_64__) || defined(_M_X64)
    return LVI_STRING("x86-64");
#elif defined(i386) || defined(__i386__) ||	\
    defined(__i386) || defined(_M_IX86)
    return LVI_STRING("x86-32");
#elif defined(__ARM_ARCH_2__)
    return LVI_STRING("arm2");
#elif defined(__ARM_ARCH_3__) || defined(__ARM_ARCH_3M__)
    return LVI_STRING("arm3");
#elif defined(__ARM_ARCH_4T__) || defined(__TARGET_ARM_4T)
    return LVI_STRING("arm4t");
#elif defined(__ARM_ARCH_5_) || defined(__ARM_ARCH_5E_)
    return LVI_STRING("arm5");
#elif defined(__ARM_ARCH_6T2_) || defined(__ARM_ARCH_6T2_)
    return LVI_STRING("arm6t2");
#elif defined(__ARM_ARCH_6__) || defined(__ARM_ARCH_6J__) ||	\
    defined(__ARM_ARCH_6K__) || defined(__ARM_ARCH_6Z__) ||	\
    defined(__ARM_ARCH_6ZK__)
    return LVI_STRING("arm6");
#elif defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_7A__) ||	\
    defined(__ARM_ARCH_7R__) || defined(__ARM_ARCH_7M__) ||	\
    defined(__ARM_ARCH_7S__)
    return LVI_STRING("arm7");
#elif defined(__ARM_ARCH_7A__) || defined(__ARM_ARCH_7R__) ||	\
    defined(__ARM_ARCH_7M__) || defined(__ARM_ARCH_7S__)
    return LVI_STRING("arm7a");
#elif defined(__ARM_ARCH_7R__) || defined(__ARM_ARCH_7M__) ||	\
    defined(__ARM_ARCH_7S__)
    return LVI_STRING("arm7r");
#elif defined(__ARM_ARCH_7M__)
    return LVI_STRING("arm7m");
#elif defined(__ARM_ARCH_7S__)
    return LVI_STRING("arm7s");
#elif defined(__aarch64__) || defined(_M_ARM64)
    return LVI_STRING("arm64");
#elif defined(mips) || defined(__mips__) || defined(__mips)
    return LVI_STRING("mips");
#elif defined(_riscv) || defined(__riscv__) || defined(__riscv)
    return LVI_STRING("risc-v");
#elif defined(__sh__)
    return LVI_STRING("superh");
#elif defined(__powerpc) || defined(__powerpc__) ||		\
    defined(__powerpc64__) || defined(__POWERPC__) ||		\
    defined(__ppc__) || defined(__PPC__) || defined(_ARCH_PPC)
    return LVI_STRING("powerpc");
#elif defined(__PPC64__) || defined(__ppc64__) || defined(_ARCH_PPC64)
    return LVI_STRING("powerpc64");
#elif defined(__sparc__) || defined(__sparc)
    return LVI_STRING("sparc");
#elif defined(__m68k__)
    return LVI_STRING("m68k");
#else
    return LVI_STRING("unknown-arch");
#endif
}

