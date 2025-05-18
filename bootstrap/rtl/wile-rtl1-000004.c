// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


void wile_exception(const char* func_name, const char* loc,
		    const char* fmt, ...)
{
    char buf1[1024], buf2[1280];
    va_list ap;

    fflush(NULL);
////    wile_stack_trace_minimal(fileno(stderr));
    lval args[8];
    args[0] = LVI_FPORT(stderr);
    (void) wile_stack_trace(NULL, args, loc);
    fflush(NULL);
    va_start(ap, fmt);
    vsnprintf(buf1, sizeof(buf1), fmt, ap);
    va_end(ap);
    snprintf(buf2, sizeof(buf2), "'%s' %s", func_name, buf1);

    cachalot->errval = new_string(buf2);
    cachalot->whence = LISP_STRDUP(loc);
    longjmp(cachalot->cenv, 1);
}

