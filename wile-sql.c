// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <complex.h>
#include <math.h>
#include <time.h>
#include <float.h>
#include <sys/types.h>
#include <unistd.h>

#include "wile-rtl1.h"

// trivial function to get sqlite code version

lval wile_sql_version(lptr* clos, lptr args, const char* loc)
{
#ifdef WILE_USES_SQLITE
    return LVI_STRING(sqlite3_libversion());
#else
    return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
}

// no args		open in-memory rw
// filename		open file ro, must exist
// filename 'read-write	open file rw, must exist
// filename 'create	open file rw, create if it doesn't exist

// fname = NULL -> in-memory rw
// mode = 0 -> ro, 1 -> rw, 2 -> rw/create

lval wile_sql_open(const char* fname, int mode, const char* loc)
{
#ifdef WILE_USES_SQLITE
    int flags;

    if (fname) {
	switch (mode) {
	case 0:
	    flags = SQLITE_OPEN_READONLY;
	    break;
	case 1:
	    flags = SQLITE_OPEN_READWRITE;
	    break;
	case 2:
	    flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
	    break;
	default:
	    wile_exception("sqlite-open", loc, "bad mode %d", mode);
	}
    } else {
	fname = "memory";
	flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_MEMORY;
    }

    lval ret;
    ret.vt = LV_SQLITE_PORT;
    // TODO: decrypt loc and use that for the origin story
    ret.origin = 0;
    if (sqlite3_open_v2(fname, &(ret.v.sqlite_conn),
			flags, NULL) != SQLITE_OK) {
	// harmless if the handle is NULL
	// TODO: use close or close_v2?
	(void) sqlite3_close_v2(ret.v.sqlite_conn);
	wile_exception("sqlite-open", loc, "failed");
    }
    return ret;
#else
    return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
}

// TODO: very preliminary interface

#ifdef WILE_USES_SQLITE
static int sql_collect(void* arg, int nres, char** vals, char** hdrs)
{
    lptr* res = (lptr*) arg;
    lptr row = NULL;
    int i;

    for (i = nres - 1; i >= 0; --i) {
	row = new_pair(vals[i] ? new_string(vals[i]) : NULL, row);
    }
    *res = new_pair(row, *res);

    return SQLITE_OK;
}

// (sqlite-run db cmd)

lval wile_sql_run(sqlite3* sqlite_conn, const char* cmd, const char* loc)
{
    lptr res;
    char *err;

    res = NULL;
    if (sqlite3_exec(sqlite_conn, cmd, sql_collect, &res, &err) != SQLITE_OK) {
	wile_exception("sqlite-run", loc, "error occurred: %s", err);
    }
    REVERSE_LIST_INPLACE(res);
    return res ? *res : LVI_NIL();
}
#endif // WILE_USES_SQLITE

// (sqlite-statement-prepare db statement)

lval wile_sql_stmt_prep(lptr* clos, lptr args, const char* loc)
{
#ifdef WILE_USES_SQLITE
    lval res;
    const char* stail;

    if (args[0].vt != LV_SQLITE_PORT || args[1].vt != LV_STRING) {
	wile_exception("sqlite-statement-prepare", loc,
		       "expects one sqlite-port and one string argument");
    }
    res.vt = LV_SQLITE_STMT;
    res.origin = args[0].origin;
    res.v.sqlite_stmt = NULL;
    if (sqlite3_prepare_v2(args[0].v.sqlite_conn, args[1].v.str, -1,
			   &(res.v.sqlite_stmt), &stail) != SQLITE_OK) {
	wile_exception("sqlite-statement-prepare", loc, "failed");
    }
    if (stail != NULL && strcmp(stail, "") != 0) {
	wile_exception("sqlite-statement-prepare", loc,
		       "cannot compile multiple statements: found %s", stail);
    }
    return res;
#else
    return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
}

// (sqlite-statement-cleanup statement)

lval wile_sql_stmt_clean(lptr* clos, lptr args, const char* loc)
{
#ifdef WILE_USES_SQLITE
    if (args[0].vt != LV_SQLITE_STMT) {
	wile_exception("sqlite-statement-cleanup", loc,
		       "expects one sqlite-stmt argument");
    }
    if (sqlite3_finalize(args[0].v.sqlite_stmt) != SQLITE_OK) {
	wile_exception("sqlite-statement-cleanup", loc, "failed");
    }
    // TODO: close out the input?
    // args->v.bv = false;
    // args->vt = LV_BOOL;
    return LVI_BOOL(true);
#else
    return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
}

// (sqlite-statement-info statement)

lval wile_sql_stmt_info(lptr* clos, lptr args, const char* loc)
{
#ifdef WILE_USES_SQLITE
    int i, n;
    lptr res;

    if (args[0].vt != LV_SQLITE_STMT) {
	wile_exception("sqlite-statement-info", loc,
		       "expects one sqlite-stmt argument");
    }
    n = sqlite3_bind_parameter_count(args[0].v.sqlite_stmt);
    res = NULL;
    for (i = n; i > 0; --i) {
	const char* np = sqlite3_bind_parameter_name(args->v.sqlite_stmt, i);
	res = new_pair(np ? new_string(np) : NULL, res);
    }
    return res ? *res : LVI_NIL();
#else
    return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
}

// (sqlite-statement-bind statement val1 ...)

lval wile_sql_stmt_bind(lptr* clos, lptr args, const char* loc)
{
#ifdef WILE_USES_SQLITE
    int i, n, ec;
    const char *name, *s;
    char c;
    sqlite3_stmt* ss;
    lisp_int_t num, den;

    if (args[0].vt != LV_SQLITE_STMT) {
	wile_exception("sqlite-statement-bind", loc,
		       "expects sqlite-stmt as first argument");
    }
    ss = args[0].v.sqlite_stmt;
    ++args;

    n = sqlite3_bind_parameter_count(ss);
    for (i = 1; i <= n; ++i) {
	if (args == NULL || args->vt == LV_NIL) {
	    break;
	}
	name = sqlite3_bind_parameter_name(ss, i);
	if (name) {
	    if (CAR(args) == NULL) {
		ec = sqlite3_bind_null(ss, i);
	    } else {
		switch (CAR(args)->vt) {
		case LV_INT:
		    ec = sqlite3_bind_int64(ss, i,
			     (sqlite3_int64) (CAR(args)->v.iv));
		    break;
		case LV_REAL:
		    ec = sqlite3_bind_double(ss, i, (double) (CAR(args)->v.rv));
		    break;
		case LV_RAT:
		    num = CAR(args)->v.irv.num;
		    den = CAR(args)->v.irv.den;
		    ec = sqlite3_bind_double(ss, i,
			     ((double) num)/((double) den));
		    break;
		case LV_STRING:
		    s = CAR(args)->v.str;
		    ec = sqlite3_bind_text(ss, i, s,
			     strlen(s), SQLITE_TRANSIENT);
		    break;
		case LV_CHAR:
		    c = CAR(args)->v.chr;
		    ec = sqlite3_bind_text(ss, i, &c, 1, SQLITE_TRANSIENT);
		    break;
		case LV_BVECTOR:
		    ec = sqlite3_bind_blob64(ss, i, CAR(args)->v.bvec.arr,
			     (sqlite3_uint64) CAR(args)->v.bvec.capa,
			     SQLITE_TRANSIENT);
		    break;
		default:
		    wile_exception("sqlite-statement-bind", loc,
				   "cannot bind %s to SQL",
				   typename(CAR(args)->vt));
		}
	    }
	    if (ec != SQLITE_OK) {
		wile_exception("sqlite-statement-bind", loc,
			       "failed to bind %s", name);
	    }
	    args = CDR(args);
	}
    }
    return LVI_BOOL(true);
#else
    return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
}

// (sqlite-statement-run statement)

lval wile_sql_stmt_run(lptr* clos, lptr args, const char* loc)
{
#ifdef WILE_USES_SQLITE
    int i, n, ec;
    bool first;
    lptr row, res;

    if (args->vt != LV_SQLITE_STMT) {
	wile_exception("sqlite-statement-run", loc,
		       "expects one sqlite-stmt argument");
    }

    res = NULL;
    first = true;
    while (1) {
	ec = sqlite3_step(args->v.sqlite_stmt);

	if (first) {
	    n = sqlite3_column_count(args->v.sqlite_stmt);
	    row = NULL;
	    for (i = n - 1; i >= 0; --i) {
		const char* np = sqlite3_column_name(args->v.sqlite_stmt, i);
		row = new_pair(np ? new_string(np) : NULL, row);
	    }
	    res = new_pair(row, res);
	    first = false;
	}

	if (ec == SQLITE_ROW) {
	    // got data, record it
	    row = NULL;
	    for (i = n - 1; i >= 0; --i) {
		const char* np =
		    (char*) sqlite3_column_text(args->v.sqlite_stmt, i);
		if (np == NULL &&
		    sqlite3_column_type(args->v.sqlite_stmt, i) != SQLITE_NULL) {
		    // something bad happened, address it? how?
		}
		row = new_pair(np ? new_string(np) : NULL, row);
	    }
	    res = new_pair(row, res);
	} else {
	    // all done, one way or another; finish up

	    // todo: check these for errors?
	    (void) sqlite3_reset(args->v.sqlite_stmt);
	    (void) sqlite3_clear_bindings(args->v.sqlite_stmt);

	    if (ec == SQLITE_DONE) {
		REVERSE_LIST_INPLACE(res);
		return res ? *res : LVI_NIL();
	    } else {
		// error finish
		wile_exception("sqlite-statement-run", loc,
			       "coughed up a hairball");
	    }
	}
    }
#else
    return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
}
