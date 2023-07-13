// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_getuserinfo(lptr*, lptr args)
{
    struct passwd* pwp;
    // errno = 0;
    if (args[0].vt == LV_STRING) {
	pwp = getpwnam(args[0].v.str);
    } else if (args[0].vt == LV_INT) {
	pwp = getpwuid(args[0].v.iv);
    } else {
	wile_exception("get-user-information", "expects a username or uid");
    }
    if (pwp) {
	lval vs[7];
	vs[0] = LVI_STRING(pwp->pw_name);
	vs[1] = LVI_STRING(pwp->pw_passwd);
	vs[2] = LVI_INT(pwp->pw_uid);
	vs[3] = LVI_INT(pwp->pw_gid);
	vs[4] = LVI_STRING(pwp->pw_gecos);
	vs[5] = LVI_STRING(pwp->pw_dir);
	vs[6] = LVI_STRING(pwp->pw_shell);
	return gen_list(7, vs, NULL);
    } else {
	// DO_ERRNO();
	return LVI_BOOL(false);
    }
}

