// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


extern const int global_tc_min_args;

// (apply proc arg1 . . . args)
// The apply procedure calls proc with the elements of the list
// (append (list arg1 . . . ) args) as the actual arguments.

lval wile_apply_function(lptr args, const char* file_name, int line_no)
{
    lval proc, lv;
    lptr ap, app;
    int i, arity, len;
    bool caboose;

    if (args == NULL || args->vt != LV_PAIR) {
	wile_exception2("apply", file_name, line_no,
			"failed while fetching proc");
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
	wile_exception2("apply", file_name, line_no,
			"last argument is not a list!");
    }
    if (app) {
// TODO: we are modifying the list structure of the input here, is that kosher?
	CDR(app) = ap;
    } else {
	args = ap;
    }
    lv = wile_list_length(NULL, args);
    if (lv.vt != LV_INT || lv.v.iv < 0) {
	wile_exception2("apply", file_name, line_no,
			"got a bad list length!?!");
    }
    len = lv.v.iv;

    if (proc.vt == LV_LAMBDA) {
	arity = proc.v.lambda.arity;
	if (arity >= 0) {
	    if (arity != len) {
		wile_exception2("apply", file_name, line_no,
				"proc expects %s %d arguments, got %d arguments",
				"exactly", arity, len);
	    }
	    caboose = false;
	} else {
	    arity = (-arity) - 1;
	    if (len < arity) {
		wile_exception2("apply", file_name, line_no,
				"proc expects %s %d arguments, got %d arguments",
				"at least", arity, len);
	    }
	    caboose = true;
	}

	i = arity + (caboose ? 1 : 0);
	if (i < global_tc_min_args) {
	    i = global_tc_min_args;
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

	if (global_tc_min_args > 0) {
	    // Oddly, openbsd cc and clang do not like this as a TAIL_CALL
	    return proc.v.lambda.fn(proc.v.lambda.closure, ap);
	} else {
	    lv = proc.v.lambda.fn(proc.v.lambda.closure, ap);
	    LISP_FREE(ap);
	    return lv;
	}
    } else if (proc.vt == LV_CONT) {
	invoke_continuation(&proc, args);
    } else {
	wile_exception2("apply", file_name, line_no,
			"failed while fetching proc - bad type %d", proc.vt);
    }
}

