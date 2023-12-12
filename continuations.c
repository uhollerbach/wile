// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "wile.h"
#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "alloc.h"
#include "lib-macros.h"

// let's see what this does

#pragma GCC optimize ("O0")

// these two variables are instantiated elsewhere, next to main() in
// wile-rtl1.c, in order to minimize dependencies in linking if the
// scheme program does not use continuations

// if stack-grow direction is known, set this to +1 or -1 as appropriate:
// stack grows upward => +1, stack grows downward => -1
// determining stack-grow direction is hard and non-portable (there may
// not even be a stack!)

extern int wile_cont_stack_grow_dir;

// base pointer from which to save the stack; this must be set by caller
// in main()

extern unsigned char* wile_cont_stack_base;

////////////////////////////////////////////////////////////////
// this is nasty unportable code which is not used if we know the
// stack direction; so comment it out unless and until we need it

#if 0

// this is necessary to keep gcc from warning that we're returning a local
// address and "helpfully" setting the pointer to NULL

static void* stp;

static void st_chk_wk(int iter)
{
    int sentinel;

    stp = &sentinel;
    if (iter > 0) {
	// hope that compiler doesn't optimize this out from under us!
	st_chk_wk(iter - 1);
    }
}

// if this routine fails somehow, manually find the stack-grow direction
// and set it appropriately above

int stack_check(int verbose)
{
    int *p0, *p1, *p2;
    int d1, d2, dr, d1a;

    st_chk_wk(1);
    p0 = stp;
    st_chk_wk(1+128);
    p1 = stp;
    st_chk_wk(1+2*128);
    p2 = stp;

    d1 = (int) (p1 - p0);
    d1a = (d1 > 0) ? d1 : -d1;
    if (d1a < 128) {
	if (wile_cont_stack_grow_dir == 0) {
	    FATAL("stack_check",
		  "stack-grow check failed: d1 %d seems wrong", d1);
	} else {
	    if (verbose > 0) {
		WARN("stack_check",
		     "stack-grow verify failed: d1 %d seems wrong", d1);
	    }
	    return wile_cont_stack_grow_dir;
	}
    }
    d2 = (int) (p2 - p0);
    if (verbose > 0) {
	printf("stack-grow measure: d1 %d d2 %d\n", d1, d2);
    }
    dr = 64*(d2 - 2*d1);
    if (dr < 0) {
	dr = -dr;
    }

    if (dr > d1a) {
	if (wile_cont_stack_grow_dir == 0) {
	    FATAL("stack_check",
		  "stack-grow check failed: %d should be near 2x %d"
		  "p0 %p p1 %p p2 %p\n",
		  d2, d1, (void*) p0, (void*) p1, (void*) p2);
	} else {
	    if (verbose > 0) {
		WARN("stack_check",
		     "stack-grow verify failed: %d should be near 2x %d\n"
		     "p0 %p p1 %p p2 %p",
		     d2, d1, (void*) p0, (void*) p1, (void*) p2);
	    }
	    return wile_cont_stack_grow_dir;
	}
    }

    d1a = (d1 > 0) ? 1 : -1;
    if (wile_cont_stack_grow_dir == 0) {
	wile_cont_stack_grow_dir = d1a;
	if (verbose > 0) {
	    printf("stack-grow measure: found direction %d\n",
		   wile_cont_stack_grow_dir);
	}
    } else if (wile_cont_stack_grow_dir != d1a) {
	FATAL("stack_check",
	      "stack-grow verify failed: preset %d, measured %d",
	      wile_cont_stack_grow_dir, d1a);
    }
    return wile_cont_stack_grow_dir;
}

#endif // 0

// end of nasty unportable code
////////////////////////////////////////////////////////////////

// save a context & wrap it up into a package; caller needs to setjmp etc

static lval save_context(int safety)
{
    uint64_t padding[32];
    uintptr_t pd;

    if (safety) {
	return save_context(0);
    }

    unsigned char* st_cur = (unsigned char*) padding;

    if (wile_cont_stack_base == NULL) {
	FATAL("save-context", "base pointer was not initialized!");
    }
    if (st_cur == NULL) {
	FATAL("save-context", "bad cur pointer");
    }
    if (wile_cont_stack_grow_dir < 0) {
	// st_cur is below wile_cont_stack_base
	pd = 1 + wile_cont_stack_base - st_cur;
    } else if (wile_cont_stack_grow_dir > 0) {
	pd = 1 + st_cur - wile_cont_stack_base;
	st_cur = wile_cont_stack_base;
    } else {
	FATAL("save-context", "stack-grow direction was not set!");
    }

    lval ret;
    ret.vt = LV_CONT;
    ret.v.cont = LISP_ALLOC(lisp_cont_t, 1);
    ret.v.cont->st_size = pd;
    ret.v.cont->st_save = LISP_ALLOC(unsigned char, ret.v.cont->st_size);
    ret.v.cont->st_wk = st_cur;
    ret.v.cont->ret = new_lv(LV_NIL);
    memcpy(ret.v.cont->st_save, ret.v.cont->st_wk, ret.v.cont->st_size);
    return ret;
}

static lisp_cont_t* cont_in_flight;

static void do_restore(lisp_cont_t* cont, int safety)
    WILE_ATTR((noreturn));

static void do_restore(lisp_cont_t* cont, int safety)
{
    uint64_t padding[32];
    uintptr_t pd;

    unsigned char* st_cur = (unsigned char*) padding;

    if (wile_cont_stack_grow_dir < 0) {
	// st_cur is below wile_cont_stack_base
	pd = 1 + wile_cont_stack_base - st_cur;
	if (pd < cont->st_size) {
	    do_restore(cont, 1);
	}
    } else {
	pd = 1 + st_cur - wile_cont_stack_base;
	if (pd < cont->st_size) {
	    do_restore(cont, 1);
	}
    }
    if (safety) {
	do_restore(cont, 0);
    }

    memcpy(cont->st_wk, cont->st_save, cont->st_size);

    cont_in_flight = cont;
    longjmp(cont->registers, 1);
}

void wile_invoke_continuation(lptr cc, lptr args)
{
    if (wile_cont_stack_grow_dir == 0) {
	FATAL("<continuation>", "stack-grow direction was not set!");
    }
    LISP_ASSERT(cc != NULL && cc->vt == LV_CONT && cc->v.cont->ret != NULL);

    *(cc->v.cont->ret) = 
	(IS_PAIR(args) && CDR(args) == NULL)
	? (CAR(args) ? *(CAR(args)) : LVI_NIL())
	: (args ? *args : LVI_NIL());

    do_restore(cc->v.cont, 1);
}

extern const int wile_tc_min_args;

lval wile_call_cc(lptr*, lptr args, const char* loc)
{
    lval cc;

    // TODO: handle primitives, special forms, macros...?
    if (!(IS_CLAMBDA(args) || IS_ILAMBDA(args) || IS_CONT(args))) {
	wile_exception("call/cc", loc,
		       "expects one procedure or continuation");
    }
    if ((IS_CLAMBDA(args) && args->v.clambda.arity != 1) ||
	(IS_ILAMBDA(args) && args->v.ilambda->arity != 1)) {
	wile_exception("call/cc", loc,
		       "procedure expects other than exactly one argument");
    }

    cc = save_context(1);
    cc.origin = args->origin;
    cont_in_flight = cc.v.cont;
    if (setjmp(cc.v.cont->registers) == 0) {
	// initial capture of the continuation

	int i;
	i = 2;
	if (i < wile_tc_min_args) {
	    i = wile_tc_min_args;
	}
	lptr fargs = LISP_ALLOC(lval, i);

	fargs[0] = cc;
	switch (args->vt) {
	case LV_CLAMBDA:
	    // if we return from ths call, the continuation
	    // was not invoked; so just return the results
	    return args->v.clambda.fn(args->v.clambda.closure, fargs, loc);

	case LV_ILAMBDA:
	    // if we return from ths call, the continuation
	    // was not invoked; so just return the results

	    fargs[0] = args[0];
	    fargs[1] = cc;
	    return wile_eval_apply_lambda(NULL, fargs, loc);

	case LV_CONT:
	    wile_invoke_continuation(args, fargs);
	    break;

	default:
	    wile_exception("call/cc", loc,
			   "impossible input type %d!", args->vt);
	}
    }

    return *(cont_in_flight->ret);
}
