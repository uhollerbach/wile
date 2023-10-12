// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


#include <inttypes.h>

lisp_escape_t cachalot;

// these two variables are used to implement continuations; see
// continuations.c. they are instantiated here because this is
// part of initialization, and we want to minimize what code gets
// pulled in if it's not used... if no continuations are used,
// we don't want those functions linked in at all.

// there is code in continuations.c to determine stack grow direction;
// but it is fraught and non-portable, so better to set it statically
// if known. on x86 and with gcc, stack grows downward.

int wile_cont_stack_grow_dir = -1;
unsigned char* wile_cont_stack_base = NULL;

extern struct wile_profile_t* wile_profile;
extern int wile_profile_size;

int main(int argc, char** argv)
{
    lptr val;
    struct lisp_escape_info tcatch;
    void* pl;

#ifdef WILE_USES_GC
    GC_INIT();
#endif

    // For continuations, we need to get the right stack base pointer;
    // unfortunately, because we need to examine the variables of main(),
    // we can't delegate this to some routine.

    pl = &val;
    if (wile_cont_stack_grow_dir > 0) {
	pl = LISP_PMAX(pl, &tcatch);
    } else if (wile_cont_stack_grow_dir < 0) {
	pl = LISP_PMIN(pl, &tcatch);
    } else {
	// need to mess with stack_check() in continuations.c if this is hit
	FATAL("wile RTL", "stack grow direction is not set!");
    }
    wile_cont_stack_base = pl;

    srand48((time(NULL)) ^ (getpid() << 4));

    tcatch.errval = NULL;
    tcatch.next = NULL;
    cachalot = &tcatch;
    if (setjmp(tcatch.cenv) == 0) {
	scheme_main(argc, argv);
    } else {
	fputs("caught exception", stderr);
	if (cachalot->c_whence) {
	    fprintf(stderr, " from %s", cachalot->c_whence);
	}
	if (cachalot->errval) {
	    fputs("\n    ", stderr);
	    if (IS_PAIR(cachalot->errval) &&
		IS_SYMBOL(CAR(cachalot->errval)) &&
		strcmp(CAR(cachalot->errval)->v.str, "wile") == 0) {
		if (IS_STRING(CDR(cachalot->errval))) {
		    fputs((CDR(cachalot->errval))->v.str, stderr);
		} else if (CDR(cachalot->errval)) {
		    display(*(CDR(cachalot->errval)), stderr);
		} else {
		    fputs("()!", stderr);
		}
	    } else if (IS_STRING(cachalot->errval)) {
		fputs(cachalot->errval->v.str, stderr);
	    } else {
		display(*(cachalot->errval), stderr);
	    }
	} else {
	    fputc('!', stderr);
	}
	fputc('\n', stderr);
	if (errno) {
	    fprintf(stderr, "errno is set to %d :: %s\n",
		    errno, strerror(errno));
	}
    }

    if (wile_profile) {
	int i;
	FILE* fp = fopen("wile-profile.out", "w");
	if (fp == NULL) {
	    fputs("wile error: unable to open wile-profile.out\n", stderr);
	    fp = stderr;
	}
	for (i = 0; i < wile_profile_size; ++i) {
	    fprintf(fp, "%"PRIu64"\t%s\n",
		    wile_profile[i].count, wile_profile[i].name);
	}
	if (fp != stderr) {
	    fclose(fp);
	}
    }

    return 0;
}

static lptr display_hooks = NULL;

void display(lval val, FILE* fp)
{
    if (fp == NULL) {
	fp = stdout;
    }
    if (val.vt == LV_VECTOR &&
	val.v.vec.arr != NULL &&
	val.v.vec.capa > 0 && 
	val.v.vec.arr[0] != NULL &&
	val.v.vec.arr[0]->vt == LV_SYMBOL) {
	const char* vname = val.v.vec.arr[0]->v.str;
	LISP_ASSERT(vname != NULL);

	lptr hooks = display_hooks;
	while (hooks) {
	    LISP_ASSERT(hooks->vt == LV_PAIR);
	    lptr hp = CAR(hooks);
	    LISP_ASSERT(hp != NULL && hp->vt == LV_PAIR);
	    lptr rsym = CAR(hp);
	    LISP_ASSERT(rsym != NULL &&
			rsym->vt == LV_SYMBOL &&
			rsym->v.str != NULL);
	    if (strcmp(rsym->v.str, vname) == 0) {
		rsym = CDR(hp);
		LISP_ASSERT(rsym != NULL && rsym->vt == LV_LAMBDA);
		lval vs[2];
		vs[0] = val;
		vs[1] = LVI_FPORT(fp);
		(void) rsym->v.lambda.fn(rsym->v.lambda.closure, vs);
		return;
	    }
	    hooks = CDR(hooks);
	}
    }
    print_lisp_val(&val, fp, NULL);
}

lval register_display_proc(const char* sym, lval proc,
			   const char* fname, int lno)
{
    if (sym) {
	lptr p1, p2, p3, p4;
	p1 = new_lv(LV_NIL);
	*p1 = LVI_SYMBOL(sym);
	p2 = new_lv(LV_NIL);
	*p2 = proc;
	p3 = new_lv(LV_NIL);
	*p3 = LVI_PAIR(p1, p2);
	p4 = new_lv(LV_NIL);
	*p4 = LVI_PAIR(p3, display_hooks);
	display_hooks = p4;
	return LVI_BOOL(true);
    } else {
	wile_exception2("display-object-hook", fname, lno, "no symbol!");
    }
}

lval num2string(lval num, int base, int prec, const char* fname, int lno)
{
    char buf[1280];

    if (num.vt == LV_INT || num.vt == LV_RAT ||
	num.vt == LV_REAL || num.vt == LV_CMPLX) {
	if (base < 2 || base > 36) {
	    wile_exception2("number->string", fname, lno,
			    "base %d is illegal", base);
	}
	if (prec != INT_MIN && (prec < -999 || prec > 999)) {
	    wile_exception2("number->string", fname, lno,
			    "precision %d is illegal", prec);
	}
	sprint_lisp_num(buf, sizeof(buf), &num, base, prec, false);
	return LVI_STRING(buf);
    } else {
	wile_exception2("number->string", fname, lno,
			"first input is not numeric");
    }
}

