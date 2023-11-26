// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


extern const int wile_tc_min_args;

// (apply proc arg1 . . . args)
// The apply procedure calls proc with the elements of the list
// (append (list arg1 . . . ) args) as the actual arguments.

lval wile_apply_function(lptr args, const char* loc)
{
    lval proc, lv;
    lptr ap, app;
    int i, arity, len;
    bool caboose;

    if (args == NULL || args->vt != LV_PAIR) {
	wile_exception("apply", loc, "failed while fetching proc");
    } else {
	proc = CAR(args) ? *(CAR(args)) : LVI_NIL();
	args = CDR(args);
    }

    app = NULL;
    ap = args;
    while (IS_PAIR(ap) && CDR(ap) != NULL) {
	app = ap;
	ap = CDR(ap);
    }
    ap = CAR(ap);
    if (!(ap == NULL || ap->vt == LV_NIL || IS_PAIR(ap))) {
	wile_exception("apply", loc, "last argument is not a list!");
    }
    if (app) {
// TODO: we are modifying the list structure of the input here, is that kosher?
	CDR(app) = ap;
    } else {
	args = ap;
    }
    lv = wile_list_length(NULL, args, loc);
    if (lv.vt != LV_INT || lv.v.iv < 0) {
	wile_exception("apply", loc, "got a bad list length!?!");
    }
    len = lv.v.iv;

    if (proc.vt == LV_CLAMBDA || proc.vt == LV_ILAMBDA) {
	arity = (proc.vt == LV_CLAMBDA) ?
	    proc.v.clambda.arity :
	    proc.v.ilambda->arity;
	if (arity >= 0) {
	    if (arity != len) {
		wile_exception("apply", loc,
			       "proc expects %s %d arguments, got %d arguments",
			       "exactly", arity, len);
	    }
	    caboose = false;
	} else {
	    arity = (-arity) - 1;
	    if (len < arity) {
		wile_exception("apply", loc,
			       "proc expects %s %d arguments, got %d arguments",
			       "at least", arity, len);
	    }
	    caboose = true;
	}

	if (proc.vt == LV_CLAMBDA) {
	    i = arity + (caboose ? 1 : 0);
	    if (i < wile_tc_min_args) {
		i = wile_tc_min_args;
	    }
	    ap = LISP_ALLOC(lval, i);
	    LISP_ASSERT(ap != NULL);
	    for (i = 0; i < arity; ++i) {
		ap[i] = CAR(args) ? *(CAR(args)) : LVI_NIL();
		args = CDR(args);
	    }
	    if (caboose) {
		ap[arity] = args ? *args : LVI_NIL();
	    }

	    if (wile_tc_min_args > 0) {
		// Oddly, clang does not like this as a TAIL_CALL
		return proc.v.clambda.fn(proc.v.clambda.closure, ap, loc);
	    } else {
		lv = proc.v.clambda.fn(proc.v.clambda.closure, ap, loc);
		LISP_FREE(ap);
		return lv;
	    }
	} else {
	    i = 2;
	    if (i < wile_tc_min_args) {
		i = wile_tc_min_args;
	    }
	    app = LISP_ALLOC(lval, i);
	    LISP_ASSERT(app != NULL);
	    app[0] = proc;
	    app[1] = args ? *args : LVI_NIL();
	    return wile_eval_apply_lambda(NULL, app, loc);
	}
    } else if (proc.vt == LV_CONT) {
	wile_invoke_continuation(&proc, args);
    } else {
	wile_exception("apply", loc,
		       "failed while fetching proc - bad type %d", proc.vt);
    }
}

