// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_getalluserinfo(lptr* clos, lptr args, const char* loc)
{
    struct passwd* pwp;
    lptr p1, res = NULL;

    setpwent();
    while (1) {
	pwp = getpwent();
	if (pwp) {
	    lval vs[7];
	    vs[0] = LVI_STRING(pwp->pw_name);
	    vs[1] = LVI_STRING(pwp->pw_passwd);
	    vs[2] = LVI_INT(pwp->pw_uid);
	    vs[3] = LVI_INT(pwp->pw_gid);
	    vs[4] = LVI_STRING(pwp->pw_gecos);
	    vs[5] = LVI_STRING(pwp->pw_dir);
	    vs[6] = LVI_STRING(pwp->pw_shell);
	    p1 = new_lv(LV_NIL);
	    *p1 = wile_gen_list(7, vs, NULL);
	    res = new_pair(p1, res);
	} else {
	    endpwent();
	    return (res ? *res : LVI_NIL());
	}
    }
}

