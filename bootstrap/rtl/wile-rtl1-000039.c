// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_getgroupinfo(lptr*, lptr args)
{
    struct group* grp;
    if (args[0].vt == LV_STRING) {
	grp = getgrnam(args[0].v.str);
    } else if (args[0].vt == LV_INT) {
	grp = getgrgid(args[0].v.iv);
    } else {
	wile_exception("get-group-information", "expects a group name or uid");
    }
    if (grp) {
	lptr res = NULL;
	char** mem = grp->gr_mem;
	while (*mem) {
	    res = new_pair(new_string(*mem), res);
	    ++mem;
	}
	res = new_pair(res, NULL);
	res = new_pair(new_int(grp->gr_gid), res);
	res = new_pair(new_string(grp->gr_passwd), res);
	res = new_pair(new_string(grp->gr_name), res);
	return *res;
    } else {
	return LVI_BOOL(false);
    }
}

