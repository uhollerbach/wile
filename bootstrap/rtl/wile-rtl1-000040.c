// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_getallgroupinfo(lptr*, lptr args)
{
    struct group* grp;
    lptr res = NULL;

    setgrent();
    while (1) {
	grp = getgrent();
	if (grp) {
	    lptr p1 = NULL;
	    char** mem = grp->gr_mem;
	    while (*mem) {
		p1 = new_pair(new_string(*mem), p1);
		++mem;
	    }
	    p1 = new_pair(p1, NULL);
	    p1 = new_pair(new_int(grp->gr_gid), p1);
	    p1 = new_pair(new_string(grp->gr_passwd), p1);
	    p1 = new_pair(new_string(grp->gr_name), p1);
	    res = new_pair(p1, res);
	} else {
	    endgrent();
	    return (res ? *res : LVI_NIL());
	}
    }
}

