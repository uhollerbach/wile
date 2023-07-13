// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


#ifndef __OpenBSD__
#include <execinfo.h>
#endif // __OpenBSD__

void wile_stack_trace_minimal(int fd)
{
    // the "!" are so that the results of write() aren't ignored...
    // instead we ignore the results of negating the write() results,
    // and thus suppress an unsuppressable warning... wtf fu gcc

    (void) !write(fd, "wile stack trace begin\n", 23);
#ifndef __OpenBSD__
    // for some reason, backtrace is not showing up on openbsd,
    // even though the manpages claim it ought(?) to be there
    void* buff[64];
    int bsize = backtrace(buff, sizeof(buff)/sizeof(buff[0]));
    backtrace_symbols_fd(buff, bsize, fd);
#endif // __OpenBSD__
    (void) !write(fd, "wile stack trace end\n", 21);
}

