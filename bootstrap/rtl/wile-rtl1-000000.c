// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


#include <inttypes.h>
#include <sys/time.h>
#include <sys/resource.h>

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
    int ret;
    lptr val;
    struct rlimit lims;
    struct lisp_escape_info tcatch;
    void* pl;

#ifdef WILE_USES_GC
    GC_INIT();
#endif

    // For continuations, we need to get the right stack base pointer;
    // unfortunately, because we need to examine the variables of main(),
    // we can't delegate this to some routine.

    pl = &ret;
    if (wile_cont_stack_grow_dir > 0) {
	pl = LISP_PMAX(pl, &val);
	pl = LISP_PMAX(pl, &lims);
	pl = LISP_PMAX(pl, &tcatch);
    } else if (wile_cont_stack_grow_dir < 0) {
	pl = LISP_PMIN(pl, &val);
	pl = LISP_PMIN(pl, &lims);
	pl = LISP_PMIN(pl, &tcatch);
    } else {
	// need to mess with stack_check() in continuations.c if this is hit
	FATAL("wile RTL", "stack grow direction is not set!");
    }
    wile_cont_stack_base = pl;

    getrlimit(RLIMIT_STACK, &lims);
    if (lims.rlim_cur < 64*1024*1024) {
	lims.rlim_cur = 64*1024*1024;
	setrlimit(RLIMIT_STACK, &lims);
    }
    srand48((time(NULL)) ^ (getpid() << 4));

    tcatch.errval = NULL;
    tcatch.next = NULL;
    cachalot = &tcatch;
    ret = EXIT_SUCCESS;
    if (setjmp(tcatch.cenv) == 0) {
	wile_main(argc, argv);
    } else {
	fputs("caught exception", stderr);
	if (cachalot->c_whence) {
	    fprintf(stderr, " from %s", cachalot->c_whence);
	}
	if (cachalot->errval) {
	    fputs("\n    ", stderr);
	    if (IS_PAIR(cachalot->errval) &&
		CAR(cachalot->errval) != NULL &&
		IS_SYMBOL(CAR(cachalot->errval)) &&
		strcmp(CAR(cachalot->errval)->v.str, "wile") == 0) {
		if (IS_STRING(CDR(cachalot->errval))) {
		    fputs((CDR(cachalot->errval))->v.str, stderr);
		} else if (CDR(cachalot->errval)) {
		    wile_print_lisp_val(CDR(cachalot->errval),
					stderr, "<main>");
		} else {
		    fputs("()!", stderr);
		}
	    } else if (IS_STRING(cachalot->errval)) {
		fputs(cachalot->errval->v.str, stderr);
	    } else {
		wile_print_lisp_val(cachalot->errval, stderr, "<main>");
	    }
	} else {
	    fputc('!', stderr);
	}
	fputc('\n', stderr);
	if (errno) {
	    fprintf(stderr, "errno is set to %d :: %s\n",
		    errno, strerror(errno));
	}
	ret = EXIT_FAILURE;
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

    return ret;
}

// trivial function to get gc code version

lval wile_gc_version(lptr*, lptr, const char*)
{
#ifdef WILE_USES_GC
    char buf[64];
    snprintf(buf, sizeof(buf), "%d.%d.%d",
	     GC_VERSION_MAJOR, GC_VERSION_MINOR, GC_VERSION_MICRO);
    return LVI_STRING(buf);
#else
    return LVI_BOOL(false);
#endif // WILE_USES_GC
}

lptr display_hooks = NULL;

lval wile_register_display_proc(const char* sym, lval proc, const char* loc)
{
    if (sym) {
	lptr p1, p2;
	p1 = new_lv(LV_NIL);
	*p1 = LVI_SYMBOL(sym);
	p2 = new_lv(LV_NIL);
	*p2 = proc;
	display_hooks = new_pair(new_pair(p1, p2), display_hooks);
	return LVI_BOOL(true);
    } else {
	wile_exception("display-object-hook", loc, "no symbol!");
    }
}

lval wile_num2string(lval num, int base, int prec, const char* loc)
{
    char buf[1280];

    if (num.vt == LV_INT || num.vt == LV_RAT ||
	num.vt == LV_REAL || num.vt == LV_CMPLX) {
	if (base < 2 || base > 36) {
	    wile_exception("number->string", loc,
			   "base %d is illegal", base);
	}
	if (prec != INT_MIN && (prec < -999 || prec > 999)) {
	    wile_exception("number->string", loc,
			   "precision %d is illegal", prec);
	}
	wile_sprint_lisp_num(buf, sizeof(buf), &num, base, prec, false);
	return LVI_STRING(buf);
    } else {
	wile_exception("number->string", loc, "first input is not numeric");
    }
}

