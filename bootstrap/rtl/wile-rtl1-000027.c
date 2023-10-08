// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval read_directory(lptr*, lptr args)
{
    DIR* dp;
    struct dirent* de;
    lptr res;

    if (args == NULL || args->vt == LV_NIL) {
	dp = opendir(".");
    } else if (args->vt == LV_STRING) {
	dp = opendir(args->v.str);
    } else {
	wile_exception("read-directory", "expects one string argument");
    }
    if (dp == NULL) {
	return LVI_BOOL(false);
    }
    res = NULL;
    while ((de = readdir(dp)) != NULL) {
	res = new_pair(new_pair(new_string(de->d_name),
				new_pair(new_int(de->d_ino),
					 NULL)),
		       res);
    }
    if (closedir(dp)) {
    }
    return (res ? *res : LVI_NIL());
}

