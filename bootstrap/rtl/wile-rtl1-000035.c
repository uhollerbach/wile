// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


lval wile_symlinkstat(lptr* clos, lptr args, const char* loc)
{
    struct stat sbuf;
    if (args[0].vt != LV_STRING) {
	wile_exception("get-symbolic-link-status", loc,
		       "expects one string argument");
    }
    if (lstat(args[0].v.str, &sbuf) == 0) {
	lval vs[13];
	vs[0] = LVI_INT(sbuf.st_dev);
	vs[1] = LVI_INT(sbuf.st_ino);
	vs[2] = LVI_INT(sbuf.st_mode);
	vs[3] = LVI_INT(sbuf.st_nlink);
	vs[4] = LVI_INT(sbuf.st_uid);
	vs[5] = LVI_INT(sbuf.st_gid);
	vs[6] = LVI_INT(sbuf.st_rdev);
	vs[7] = LVI_INT(sbuf.st_size);
	vs[8] = LVI_INT(sbuf.st_blksize);
	vs[9] = LVI_INT(sbuf.st_blocks);
	vs[10] = LVI_INT(sbuf.st_atime);
	vs[11] = LVI_INT(sbuf.st_mtime);
	vs[12] = LVI_INT(sbuf.st_ctime);
	return wile_gen_list(13, vs, NULL);
    } else {
	return LVI_BOOL(false);
    }
}

