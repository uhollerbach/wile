// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_gmtime(lptr*, lptr args)
{
    time_t now;
    if (args == NULL) {
	now = time(NULL);
    } else if (args[0].vt != LV_INT) {
	wile_exception("UTCtime",
		       "expects no argument or one integer argument");
    } else {
	now = (time_t) args[0].v.iv;
    }
    struct tm tval;
    if (gmtime_r(&now, &tval)) {
	lval vs[9];
	vs[0] = LVI_INT(tval.tm_year + 1900);
	vs[1] = LVI_INT(tval.tm_mon + 1);
	vs[2] = LVI_INT(tval.tm_mday);
	vs[3] = LVI_INT(tval.tm_hour);
	vs[4] = LVI_INT(tval.tm_min);
	vs[5] = LVI_INT(tval.tm_sec);
	vs[6] = LVI_INT(tval.tm_wday);
	vs[7] = LVI_INT(tval.tm_yday);
	vs[8] = LVI_INT(tval.tm_isdst);
	return wile_gen_list(9, vs, NULL);
    } else {
	return LVI_BOOL(false);
    }
}

