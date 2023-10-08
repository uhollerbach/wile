// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;

// --8><----8><----8><--

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

// --8><----8><----8><--

uint16_t wile_binfo(void)
{
    int ret = 0, shift = 0;

    // bit 0: GC or no GC?

#ifdef WILE_USES_GC
    ret |= (1 << shift);
#endif
    shift += 1;

    // bit 1: 0 = drand48, 1 = RC4-rand

#ifdef WILE_USES_RC4_RAND
    ret |= (1 << shift);
#endif
    shift += 1;

    // bit 2: sqlite or no sqlite?

#ifdef WILE_USES_SQLITE
    ret |= (1 << shift);
#endif
    shift += 1;

    // bits 3-4: 0 = plain old double, 1 = long double, 2 = quad double

#ifdef WILE_USES_LONG_DOUBLE
    ret |= (1 << shift);
#elif WILE_USES_QUAD_DOUBLE
    ret |= (2 << shift);
#endif
    shift += 2;

    // bits 5-6: 0 = plain old long int, 1 = int128, 2 = bigint

#ifdef WILE_USES_INT128
    ret |= (1 << shift);
#elif WILE_USES_BIGINT
    ret |= (2 << shift);
#endif
    shift += 2;

    return ret;
}

// --8><----8><----8><--

#ifndef __OpenBSD__
#include <execinfo.h>
#endif // __OpenBSD__

void wile_stack_trace_minimal(int fd)
{
    // the "!" are so that the results of write() aren't ignored...
    // instead we ignore the results of negating the write() results,
    // and thus suppress an unsuppressable warning... wtf fu gcc

    (void) !write(fd, "wile stack trace begin\n", 23);
#ifndef __OpenBSD__
    // for some reason, backtrace is not showing up on openbsd,
    // even though the manpages claim it ought(?) to be there
    void* buff[64];
    int bsize = backtrace(buff, sizeof(buff)/sizeof(buff[0]));
    backtrace_symbols_fd(buff, bsize, fd);
#endif // __OpenBSD__
    (void) !write(fd, "wile stack trace end\n", 21);
}

// --8><----8><----8><--

void wile_exception(const char* fname, const char* fmt, ...)
{
    char buf1[1024], buf2[1280];
    va_list ap;

    wile_stack_trace_minimal(fileno(stderr));
    va_start(ap, fmt);
    vsnprintf(buf1, sizeof(buf1), fmt, ap);
    va_end(ap);
    snprintf(buf2, sizeof(buf2), "'%s' %s", fname, buf1);

    cachalot->errval = new_string(buf2);
    cachalot->l_whence = 0;
    cachalot->c_whence = LISP_WHENCE;
    longjmp(cachalot->cenv, 1);
}

// --8><----8><----8><--

// updated version which includes file/line# info;
// don't want to convert everything all at once.
// TODO: eventually use this everywhere and remove the above

void wile_exception2(const char* func_name, const char* file_name,
		     int line_no, const char* fmt, ...)
{
    char buf1[1024], buf2[1280];
    va_list ap;

    wile_stack_trace_minimal(fileno(stderr));
    va_start(ap, fmt);
    vsnprintf(buf1, sizeof(buf1), fmt, ap);
    va_end(ap);
    snprintf(buf2, sizeof(buf2), "'%s' %s", func_name, buf1);

    cachalot->errval = new_string(buf2);
    cachalot->l_whence = 0;
    snprintf(buf1, sizeof(buf1), "<%s:%d>", file_name, line_no);
    cachalot->c_whence = LISP_STRDUP(buf1);
    longjmp(cachalot->cenv, 1);
}

// --8><----8><----8><--

lval get_gensym(void)
{
    static unsigned long count = 0;
    char buf[64];
    lval res;

    snprintf(buf, sizeof(buf), " symbol.%lu", ++count);
    res.vt = LV_SYMBOL;
    res.v.str = LISP_STRDUP(buf);
    return res;
}

// --8><----8><----8><--

lval run_system_command(lval cmd, const char* fname, int lno)
{
    if (cmd.vt != LV_STRING) {
	wile_exception2("run-command", fname, lno, "got bad input type!");
    }
    int status = system(cmd.v.str);
    if (status < 0) {
	return LVI_BOOL(false);
    } else {
	if (WIFEXITED(status)) {
	    status = WEXITSTATUS(status);
	} else if (WIFSIGNALED(status)) {
	    status = -WTERMSIG(status);
	} else {
	    status = INT_MIN;
	}
	return LVI_INT(status);
    }
}

// --8><----8><----8><--

lval run_pipe_command(lval cmd, const char* rw, const char* fname, int lno)
{
    if (cmd.vt != LV_STRING ||
	(strcmp(rw, "r") != 0 && strcmp(rw, "w") != 0)) {
	wile_exception2("run-read/write-command", fname, lno,
			"got bad input type!");
    }
    if (strcmp(rw, "r") != 0 && strcmp(rw, "w") != 0) {
	wile_exception2("run-read/write-command", fname, lno,
			"got bad read/write mode %s", rw);
    }
    FILE* fp = popen(cmd.v.str, rw);
    if (fp) {
	return LVI_PPORT(fp);
    } else {
	return LVI_BOOL(false);
    }
}

// --8><----8><----8><--

lval wile_temp_file(lptr*, lptr args)
{
    char *template, *tp;
    size_t tlen;
    int i, nt;
    FILE* fp;

    if (args->vt != LV_STRING) {
	wile_exception("open-temporary-file", "expects one string argument");
    }
    tlen = strlen(args->v.str);
    template = LISP_ALLOC(char, tlen + 8);
    LISP_ASSERT(template != NULL);
    strcpy(template, args->v.str);
    tp = template + tlen;
    nt = (tlen < 6) ? tlen : 6;
    for (i = 0; i < nt; ++i) {
	if (*(--tp) != 'X') {
	    break;
	}
    }
    tp = template + tlen;
    while (i++ < 6) {
	*tp++ = 'X';
    }
    *tp++ = '\0';
    nt = mkstemp(template);
    if (nt < 0) {
	LISP_FREE_STR(template);
	wile_exception("open-temporary-file", "could not create temporary file");
    }
    lval vs[2];
    vs[1] = LVI_STRING(template);
    LISP_FREE_STR(template);
    fp = fdopen(nt, "w+");
    if (fp == NULL) {
	wile_exception("open-temporary-file", "could not create temporary file");
    }
    vs[0] = LVI_FPORT(fp);
    return gen_list(2, vs, NULL);
}

// --8><----8><----8><--

// need to protect this as if it were a header file because we need it
// in several places; if this file gets split, it needs to be included
// more than once

#ifndef WILE_NEEDS_ULEX
#define WILE_NEEDS_ULEX
#include "wile-parse.h"
#include "wile-lex.h"

void set_start_state(struct ulex_context* context);
#endif // WILE_NEEDS_ULEX

lval string2num(lval str, const char* fname, int lno)
{
    lval val1, val2;
    unsigned char* text;

    set_lisp_loc_file(NULL);
    struct ulex_context* context = ulex_init(ulex_TEXT, str.v.str);
    if (context == NULL) {
	wile_exception2("string->number", fname, lno,
			"was unable to set up lexer");
    }
    set_start_state(context);
    (void) wile_lex(context, &val1, NULL, &text);

    if (IS_NUMERIC(&val1) && wile_lex(context, &val2, NULL, &text) == 0) {
	if (val1.vt == LV_INT ||
	    val1.vt == LV_RAT ||
	    val1.vt == LV_REAL || val1.vt == LV_CMPLX) {
	    return val1;
	} else {
	    wile_exception2("string->number", fname, lno,
			    "got bad type %d!", val1.vt);
	}
	ulex_cleanup(context);
    } else {
	ulex_cleanup(context);
	wile_exception2("string->number", fname, lno,
			"got bad input string '%s'", str.v.str);
    }
}

// --8><----8><----8><--

lisp_int_t powi(lisp_int_t a, lisp_int_t b)
{
    lisp_int_t p;

    p = 1;
    while (b) {
	if (b%2) {
	    p *= a;
	}
	b /= 2;
	a = a*a;
    }
    return p;
}

// --8><----8><----8><--

lisp_real_t pcheby1(int n, lisp_real_t x)
{
    int k;
    lisp_real_t pm, pc, pp;

    if (n == 0) {
	return 1.0;
    } else if (n == 1) {
	return x;
    } else {
	pm = 1.0;
	pc = x;
	x *= 2.0;
	for (k = 1; k < n; ++k) {
	    pp = x*pc - pm;
	    pm = pc;
	    pc = pp;
	}
	return pc;
    }
}

// --8><----8><----8><--

lisp_real_t pcheby2(int n, lisp_real_t x)
{
    int k;
    lisp_real_t pm, pc, pp;

    x *= 2.0;
    if (n == 0) {
	return 1.0;
    } else if (n == 1) {
	return x;
    } else {
	pm = 1.0;
	pc = x;
	for (k = 1; k < n; ++k) {
	    pp = x*pc - pm;
	    pm = pc;
	    pc = pp;
	}
	return pc;
    }
}

// --8><----8><----8><--

lisp_real_t plegendre(int n, lisp_real_t x)
{
    int k;
    lisp_real_t a, pm, pc, pp;

    if (n == 0) {
	return 1.0;
    } else if (n == 1) {
	return x;
    } else {
	pm = 1.0;
	pc = x;
	for (k = 1; k < n; ++k) {
	    a = ((lisp_real_t) k)/((lisp_real_t) (k + 1));
	    pp = (1.0 + a)*x*pc - a*pm;
	    pm = pc;
	    pc = pp;
	}
	return pc;
    }
}

// --8><----8><----8><--

lisp_real_t plaguerre(int n, lisp_real_t x)
{
    int k;
    lisp_real_t a, pm, pc, pp;

    if (n == 0) {
	return 1.0;
    } else if (n == 1) {
	return 1.0 - x;
    } else {
	pm = 1.0;
	pc = 1.0 - x;
	for (k = 1; k < n; ++k) {
	    a = ((lisp_real_t) k)/((lisp_real_t) (k + 1));
	    pp = (1.0 + a - x/(k + 1))*pc - a*pm;
	    pm = pc;
	    pc = pp;
	}
	return pc;
    }
}

// --8><----8><----8><--

lisp_real_t phermite1(int n, lisp_real_t x)
{
    int k;
    lisp_real_t pm, pc, pp;

    if (n == 0) {
	return 1.0;
    } else if (n == 1) {
	return 2.0*x;
    } else {
	pm = 1.0;
	pc = 2.0*x;
	for (k = 1; k < n; ++k) {
	    pp = 2.0*x*pc - 2.0*k*pm;
	    pm = pc;
	    pc = pp;
	}
	return pc;
    }
}

// --8><----8><----8><--

lisp_real_t phermite2(int n, lisp_real_t x)
{
    int k;
    lisp_real_t pm, pc, pp;

    if (n == 0) {
	return 1.0;
    } else if (n == 1) {
	return x;
    } else {
	pm = 1.0;
	pc = x;
	for (k = 1; k < n; ++k) {
	    pp = x*pc - k*pm;
	    pm = pc;
	    pc = pp;
	}
	return pc;
    }
}

// --8><----8><----8><--

void floor_qr(lisp_int_t n1, lisp_int_t n2, lisp_int_t* nq, lisp_int_t* nr)
{
    if (n2 == 0) {
	wile_exception("floor_qr", "division by zero!");
    }
    *nq = n1/n2;
    *nr = n1 - *nq*n2;
    if (*nr != 0 && (n1 < 0) != (n2 < 0)) {
	*nq -= 1;
	*nr = n1 - *nq*n2;
    }
}

// --8><----8><----8><--

void trunc_qr(lisp_int_t n1, lisp_int_t n2, lisp_int_t* nq, lisp_int_t* nr)
{
    if (n2 == 0) {
	wile_exception("trunc_qr", "division by zero!");
    }
    *nq = n1/n2;
    *nr = n1 - *nq*n2;
}

// --8><----8><----8><--

void ceil_qr(lisp_int_t n1, lisp_int_t n2, lisp_int_t* nq, lisp_int_t* nr)
{
    if (n2 == 0) {
	wile_exception("ceiling_qr", "division by zero!");
    }
    *nq = n1/n2;
    *nr = n1 - *nq*n2;
    if (*nr != 0 && (n1 < 0) == (n2 < 0)) {
	*nq += 1;
	*nr = n1 - *nq*n2;
    }
}

// --8><----8><----8><--

// Yes, an lval* is an lptr. Remember that this is an array of lvals,
// not a single lptr. This routine is not intended for zero-length lists;
// if 0 items are passed in, a NULL pointer dereference will result
// and the program will crash.

lval gen_list(size_t nitems, lval* items, lval* tail)
{
    lptr p1, list = NULL;

    if (tail) {
	list = new_lv(LV_NIL);
	*list = *tail;
    }
    while (nitems > 0) {
	p1 = new_lv(LV_NIL);
	*p1 = items[--nitems];
	list = new_pair(p1, list);
    }
    return *list;
}

// --8><----8><----8><--

bool do_eqv(lptr arg1, lptr arg2)
{
    size_t i;

 top:
    if (arg1 == NULL && arg2 == NULL) {
	return true;
    } else if (arg1 == NULL || arg2 == NULL) {
	return false;
    } else if (arg1->vt != arg2->vt) {
	return false;
    } else {
	switch (arg1->vt) {
	case LV_NIL:		return true;
	case LV_SYMBOL:		return (strcmp(arg1->v.str, arg2->v.str) == 0);
	case LV_BOOL:		return (arg1->v.bv == arg2->v.bv);
	case LV_CHAR:		return (arg1->v.chr == arg2->v.chr);
	case LV_STRING:		return (strcmp(arg1->v.str, arg2->v.str) == 0);
	case LV_INT:		return (arg1->v.iv == arg2->v.iv);

	case LV_RAT:
	    return (arg1->v.irv.num == arg2->v.irv.num &&
		    arg1->v.irv.den == arg2->v.irv.den);

	case LV_REAL:		return (arg1->v.rv == arg2->v.rv ||
					(ISNAN(arg1->v.rv) &&
					 ISNAN(arg2->v.rv)));

	case LV_CMPLX:
	    return (arg1->v.cv == arg2->v.cv ||
		    ((ISNAN(CREAL(arg1->v.cv)) || ISNAN(CIMAG(arg1->v.cv))) &&
		     (ISNAN(CREAL(arg2->v.cv)) || ISNAN(CIMAG(arg2->v.cv)))));

	case LV_PAIR:
	    if (!do_eqv(CAR(arg1), CAR(arg2))) {
		return false;
	    }
	    arg1 = CDR(arg1);
	    arg2 = CDR(arg2);
	    goto top;

	case LV_FILE_PORT:
	case LV_PIPE_PORT:
	case LV_SOCK_PORT:	return (arg1->v.fp == arg2->v.fp);

#ifdef WILE_USES_SQLITE
	case LV_SQLITE_PORT:	return (arg1->v.sqlite_conn ==
					arg2->v.sqlite_conn);
	case LV_SQLITE_STMT:	return (arg1->v.sqlite_stmt ==
					arg2->v.sqlite_stmt);
#endif // WILE_USES_SQLITE

	case LV_VECTOR:
	    if (arg1->v.vec.capa != arg2->v.vec.capa) {
		return false;
	    }
	    for (i = 0; i < arg1->v.vec.capa; ++i) {
		if (!do_eqv(arg1->v.vec.arr[i], arg2->v.vec.arr[i])) {
		    return false;
		}
	    }
	    return true;

	case LV_BVECTOR:
	    if (arg1->v.bvec.capa != arg2->v.bvec.capa) {
		return false;
	    }
	    for (i = 0; i < arg1->v.bvec.capa; ++i) {
		if (arg1->v.bvec.arr[i] != arg2->v.bvec.arr[i]) {
		    return false;
		}
	    }
	    return true;

	// TODO: implement these
//////	case LV_STR_PORT:
//	case LV_PROMISE:
//	case LV_LAMBDA:

	default:		return false;
	}
    }
}

// --8><----8><----8><--

#define AB_ATYPE	char
#define AB_STYPE	ab_char
#define AB_STATIC
#define AB_ALLOC	LISP_ALLOC
#define AB_REALLOC	LISP_REALLOC
#define AB_FREE		LISP_FREE

#include "array_builder.h"

lval wile_read_line(lptr*, lptr args)
{
    FILE* fp;

    if (args[0].vt == LV_FILE_PORT ||
	args[0].vt == LV_PIPE_PORT ||
	args[0].vt == LV_SOCK_PORT) {
	fp = args[0].v.fp;
    } else {
	wile_exception("read-line", "expects one port argument");
    }

    ab_char mya_str = ab_char_setup(64);
    char buf[128], *bp;
    while (fgets(buf, sizeof(buf), fp)) {
	bp = buf;
	while (*bp) {
	    if (*bp == '\r' || *bp == '\n') {
		break;
	    }
	    ab_char_append(&mya_str, *bp++);
	}
	if (*bp) {
	    break;
	}
    }

    lval res;
    if (ab_char_get_size(&mya_str) == 0 && feof(fp)) {
	res = LVI_BOOL(false);
    } else {
	ab_char_append(&mya_str, '\0');
	res = LVI_STRING(ab_char_get_array(&mya_str));
    }
    ab_char_destroy(&mya_str);
    return res;
}

// --8><----8><----8><--

// need to protect this as if it were a header file because we need it
// in several places; if this file gets split, it needs to be included
// more than once

#ifndef WILE_NEEDS_ULEX
#define WILE_NEEDS_ULEX
#include "wile-parse.h"
#include "wile-lex.h"

void set_start_state(struct ulex_context* context);
#endif // WILE_NEEDS_ULEX

lval parse_string(lptr*, lptr args)
{
    lval res;

    if (!IS_STRING(args)) {
	wile_exception("parse-string", "expects one string argument");
    }

    lptr lp = NULL;
    set_lisp_loc_file(NULL);
    struct ulex_context* context = ulex_init(ulex_TEXT, args[0].v.str);

    if (context == NULL) {
	wile_exception("parse-string", "unable to set up lexer");
    } else {
	set_start_state(context);
	if (wile_parse(context, &lp, 1)) {
	    res = LVI_BOOL(false);
	} else {
	    res = *lp;
	}
    }
    ulex_cleanup(context);
    return res;
}

// --8><----8><----8><--

// need to protect this as if it were a header file because we need it
// in several places; if this file gets split, it needs to be included
// more than once

#ifndef WILE_NEEDS_ULEX
#define WILE_NEEDS_ULEX
#include "wile-parse.h"
#include "wile-lex.h"

void set_start_state(struct ulex_context* context);
#endif // WILE_NEEDS_ULEX

lval parse_file(lptr*, lptr args)
{
    lval res;

    if (!IS_STRING(args) && !IS_FPORT(args) &&
	!IS_PPORT(args) && !IS_SOCKPORT(args)) {
	wile_exception("read-all",
		       "expects one string or file/pipe/socket port argument");
    }

    lptr lp = NULL;
    set_lisp_loc_file(IS_STRING(args) ? args->v.str : NULL);
    struct ulex_context* context =
	IS_STRING(args)
	? ulex_init(ulex_FILE, args->v.str)
	: ulex_init(ulex_STREAM, args->v.fp);

    if (context == NULL) {
	wile_exception("read-all", "unable to set up lexer");
    } else {
	set_start_state(context);
	if (wile_parse(context, &lp, 1)) {
	    res = LVI_BOOL(false);
	} else {
	    res = *lp;
	}
    }
    ulex_cleanup(context);
    return res;
}

// --8><----8><----8><--

// (regex-match pattern string) => #f || (pre-match match post-match)

lval wile_regex_match(lptr*, lptr args)
{
    struct nfa_state* nstate;
    struct nfa_work* nwork;
    char *ms, *me, save;
    lptr sstr[2];
    lval res;

    if (args[0].vt != LV_STRING || args[1].vt != LV_STRING) {
	wile_exception("regex-match", "expects two string arguments");
    }

    nstate = regex_parse(args[0].v.str, NULL, REGEX_OPTION_8BIT, 0);
    if (nstate == NULL) {
	wile_exception("regex-match", "regular expression parse failed");
    }
    nwork = regex_wrap(nstate, REGEX_OPTION_8BIT);
    if (regex_match((unsigned char*) args[1].v.str, nwork,
		    (unsigned char**) &ms, (unsigned char**) &me)) {
	++me;
	save = *ms;
	*ms = '\0';
	sstr[0] = new_string(args[1].v.str);
	*ms = save;
	save = *me;
	*me = '\0';
	sstr[1] = new_string(ms);
	*me = save;
	sstr[1] = new_pair(sstr[1], new_pair(new_string(me), NULL));
	res = *(new_pair(sstr[0], sstr[1]));
    } else {
	res = LVI_BOOL(false);
    }

    regex_free(nwork);
    nfa_free(nstate);

    return res;
}

// --8><----8><----8><--

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

// --8><----8><----8><--

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

// --8><----8><----8><--

lval wile_char2string(lptr*, lptr args)
{
    lval lv, ac;
    lisp_int_t i, len;

    if (args == NULL || args->vt == LV_NIL) {
	return LVI_STRING("");
    }
    lv = wile_list_length(NULL, args);
    if (lv.vt != LV_INT || lv.v.iv < 0) {
	wile_exception("char->string", "got a bad list length!?!");
    }
    len = lv.v.iv;

    if (len == 1) {
	ac = CAR(args) ? *(CAR(args)) : LVI_NIL();
	if (ac.vt == LV_PAIR || ac.vt == LV_NIL) {
	    args = CAR(args);
	    lv = wile_list_length(NULL, args);
	    if (lv.vt != LV_INT || lv.v.iv < 0) {
		wile_exception("char->string", "got a bad list length!?!");
	    }
	    len = lv.v.iv;
	}
    }

    LISP_ASSERT(len >= 0);

    lv.vt = LV_STRING;
    lv.v.str = LISP_ALLOC(char, len + 1);
    LISP_ASSERT(lv.v.str != NULL);

    for (i = 0; i < len; ++i) {
	LISP_ASSERT(args != NULL && args->vt == LV_PAIR);
	ac = CAR(args) ? *(CAR(args)) : LVI_NIL();
	if (ac.vt != LV_CHAR) {
	    wile_exception("char->string", "got a non-character argument");
	}
	lv.v.str[i] = ac.v.chr;
	args = CDR(args);
    }
    LISP_ASSERT(args == NULL || args->vt == LV_NIL);
    lv.v.str[len] = '\0';
    return lv;
}

// --8><----8><----8><--

static int tcp_proto = 0;

#define GET_PROTO(fname)						\
    do {								\
	if (tcp_proto == 0) {						\
	    struct protoent* pro = getprotobyname("tcp");		\
	    if (pro == NULL) {						\
		endprotoent();						\
		wile_exception((fname),					\
			       "can't find tcp protocol number! no networking?"); \
	    }								\
	    tcp_proto = pro->p_proto;					\
	    endprotoent();						\
	}								\
    } while (0)

lval wile_listen_port(lptr*, lptr args)
{
    int sd;
    FILE* fp;

    if (args[0].vt != LV_INT) {
	wile_exception("listen-on", "got a non-integer argument");
    }
    if (args[0].v.iv < 0 || args[0].v.iv > 65535) {
	wile_exception("listen-on", "got bad port number %lld",
		     (long long) args[0].v.iv);
    }
    GET_PROTO("listen-on");

    sd = socket(AF_INET, SOCK_STREAM, tcp_proto);
    if (sd < 0) {
	return LVI_BOOL(false);
    } else {
	struct sockaddr_in my_addr;

	memset(&my_addr, 0, sizeof(my_addr));
	my_addr.sin_family = AF_INET;
	my_addr.sin_addr.s_addr = htonl(INADDR_ANY);
	my_addr.sin_port = htons(args[0].v.iv);

	if (bind(sd, (struct sockaddr*) &my_addr, sizeof(my_addr)) < 0) {
	    return LVI_BOOL(false);
	} else {
	    if (listen(sd, 16) < 0) {
		return LVI_BOOL(false);
	    } else {
		fp = fdopen(sd, "rb+");
		if (fp == NULL) {
		    // TODO: close sd? I think so
		    return LVI_BOOL(false);
		} else {
		    setvbuf(fp, NULL, _IONBF, 0);
		    return LVI_SPORT(fp);
		}
	    }
	}
    }
}

lval wile_accept_connection(lptr*, lptr args)
{
    int fd;
    unsigned int psize;
    char buf[INET_ADDRSTRLEN+1];
    struct sockaddr_in peer;

    if (args[0].vt != LV_SOCK_PORT) {
	wile_exception("accept", "expects one socket-port argument");
    } else {
	memset(&peer, 0, sizeof(peer));
	psize = sizeof(peer);
	fd = accept(fileno(args->v.fp), (struct sockaddr*) &peer, &psize);
	if (fd < 0) {
	    return LVI_BOOL(false);
	} else {
	    lval vs[3];

	    vs[0] = LVI_SPORT(fdopen(fd, "rb+"));
	    if (vs[0].v.fp == NULL) {
		vs[0] = LVI_BOOL(false);
	    } else {
		setvbuf(vs[0].v.fp, NULL, _IONBF, 0);
	    }
	    if (inet_ntop(AF_INET, &(peer.sin_addr), buf, sizeof(buf))) {
		vs[1] = LVI_STRING(buf);
	    } else {
		vs[1] = LVI_STRING("<unknown>");
	    }
	    vs[2] = LVI_INT(ntohs(peer.sin_port));
	    return gen_list(3, vs, NULL);
	}
    }
}

lval wile_connect_to(lptr*, lptr args)
{
    char pstr[8];
    struct addrinfo hints, *server, *sp;
    lval ret;
    int sd;

    if (args[0].vt != LV_STRING || args[1].vt != LV_INT) {
	wile_exception("connect-to",
		       "expects one string and one int argument");
    }
    if (args[1].v.iv < 0 || args[1].v.iv > 65535) {
	wile_exception("connect-to", "got bad port number %lld",
		       (long long) args[1].v.iv);
    }
    GET_PROTO("connect-to");

    snprintf(pstr, sizeof(pstr), "%d", (int) args[1].v.iv);

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;	// might as well be prepared for IPv6
    hints.ai_socktype = SOCK_STREAM;

    if (getaddrinfo(args[0].v.str, pstr, &hints, &server) != 0) {
	wile_exception("connect-to", "getaddrinfo failed");
    }

    sp = server;
    while (sp) {
	sd = socket(sp->ai_family, sp->ai_socktype, sp->ai_protocol);
	if (sd >= 0) {
	    if (connect(sd, sp->ai_addr, sp->ai_addrlen) < 0) {
		close(sd);
	    } else {
		// good
		break;
	    }
	}
	sp = sp->ai_next;
    }

    if (sp == NULL) {
	ret = LVI_BOOL(false);
    } else {
	ret = LVI_SPORT(fdopen(sd, "rb+"));
	if (ret.v.fp == NULL) {
	    ret = LVI_BOOL(false);
	} else {
	    setvbuf(ret.v.fp, NULL, _IONBF, 0);
	}
    }
    freeaddrinfo(server);
    return ret;
}

// --8><----8><----8><--

lval wile_rand_normal_pair(lisp_real_t m, lisp_real_t s)
{
    while (1) {
	lisp_real_t v1, v2, r2;
	v1 = 2.0*drand48() - 1.0;
	v2 = 2.0*drand48() - 1.0;
	r2 = v1*v1 + v2*v2;
	if (r2 > 0.0 && r2 < 1.0) {
	    lval vs[2];
	    s *= SQRT(-2.0*LOG(r2)/r2);
	    vs[0] = LVI_REAL(m + s*v1);
	    vs[1] = LVI_REAL(m + s*v2);
	    return gen_list(2, vs, NULL);
	}
    }
}

// --8><----8><----8><--

lval wile_gethostname(lptr*, lptr)
{
    char buf[HOST_NAME_MAX+1];
    if (gethostname(buf, sizeof(buf)) < 0) {
	return LVI_BOOL(false);
    } else {
	return LVI_STRING(buf);
    }
}

// --8><----8><----8><--

lval wile_getdomainname(lptr*, lptr)
{
    char buf[HOST_NAME_MAX+1];
    if (getdomainname(buf, sizeof(buf)) < 0) {
	return LVI_BOOL(false);
    } else {
	return LVI_STRING(buf);
    }
}

// --8><----8><----8><--

lval wile_getcwd(lptr*, lptr)
{
    char str[1+PATH_MAX], *sp;
    sp = getcwd(str, sizeof(str));
    if (sp) {
	return LVI_STRING(sp);
    } else {
	return LVI_BOOL(false);
    }
}

// --8><----8><----8><--

lval wile_cputime(lptr*, lptr)
{
    struct rusage usage;
    if (getrusage(RUSAGE_SELF, &usage) == 0) {
	lval vs[2];
	vs[0] = LVI_REAL(usage.ru_utime.tv_sec + 1.0e-6*usage.ru_utime.tv_usec);
	vs[1] = LVI_REAL(usage.ru_stime.tv_sec + 1.0e-6*usage.ru_stime.tv_usec);
	return gen_list(2, vs, NULL);
    } else {
	return LVI_BOOL(false);
    }
}

// --8><----8><----8><--

lval wile_filestat(lptr*, lptr args)
{
    struct stat sbuf;
    if (args[0].vt != LV_STRING) {
	wile_exception("get-file-status", "expects one string argument");
    }
    if (stat(args[0].v.str, &sbuf) == 0) {
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
	return gen_list(13, vs, NULL);
    } else {
	return LVI_BOOL(false);
    }
}

// --8><----8><----8><--

lval wile_symlinkstat(lptr*, lptr args)
{
    struct stat sbuf;
    if (args[0].vt != LV_STRING) {
	wile_exception("get-symbolic-link-status",
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
	return gen_list(13, vs, NULL);
    } else {
	return LVI_BOOL(false);
    }
}

// --8><----8><----8><--

lval wile_getuserinfo(lptr*, lptr args)
{
    struct passwd* pwp;
    if (args[0].vt == LV_STRING) {
	pwp = getpwnam(args[0].v.str);
    } else if (args[0].vt == LV_INT) {
	pwp = getpwuid(args[0].v.iv);
    } else {
	wile_exception("get-user-information", "expects a username or uid");
    }
    if (pwp) {
	lval vs[7];
	vs[0] = LVI_STRING(pwp->pw_name);
	vs[1] = LVI_STRING(pwp->pw_passwd);
	vs[2] = LVI_INT(pwp->pw_uid);
	vs[3] = LVI_INT(pwp->pw_gid);
	vs[4] = LVI_STRING(pwp->pw_gecos);
	vs[5] = LVI_STRING(pwp->pw_dir);
	vs[6] = LVI_STRING(pwp->pw_shell);
	return gen_list(7, vs, NULL);
    } else {
	return LVI_BOOL(false);
    }
}

// --8><----8><----8><--

lval wile_localtime(lptr*, lptr args)
{
    time_t now;
    if (args == NULL) {
	now = time(NULL);
    } else if (args[0].vt != LV_INT) {
	wile_exception("localtime",
		       "expects no argument or one integer argument");
    } else {
	now = (time_t) args[0].v.iv;
    }
    struct tm tval;
    if (localtime_r(&now, &tval)) {
	lval vs[9];
	vs[0] = LVI_INT(tval.tm_year + 1900);
	vs[1] = LVI_INT(tval.tm_mon + 1);
	vs[2] = LVI_INT(tval.tm_mday);
	vs[3] = LVI_INT(tval.tm_hour);
	vs[4] = LVI_INT(tval.tm_min);
	vs[5] = LVI_INT(tval.tm_sec);
	vs[6] = LVI_INT(tval.tm_wday);
	vs[7] = LVI_INT(tval.tm_yday);
	vs[8] = LVI_INT(tval.tm_isdst);
	return gen_list(9, vs, NULL);
    } else {
	return LVI_BOOL(false);
    }
}

// --8><----8><----8><--

lval wile_gmtime(lptr*, lptr args)
{
    time_t now;
    if (args == NULL) {
	now = time(NULL);
    } else if (args[0].vt != LV_INT) {
	wile_exception("UTCtime",
		       "expects no argument or one integer argument");
    } else {
	now = (time_t) args[0].v.iv;
    }
    struct tm tval;
    if (gmtime_r(&now, &tval)) {
	lval vs[9];
	vs[0] = LVI_INT(tval.tm_year + 1900);
	vs[1] = LVI_INT(tval.tm_mon + 1);
	vs[2] = LVI_INT(tval.tm_mday);
	vs[3] = LVI_INT(tval.tm_hour);
	vs[4] = LVI_INT(tval.tm_min);
	vs[5] = LVI_INT(tval.tm_sec);
	vs[6] = LVI_INT(tval.tm_wday);
	vs[7] = LVI_INT(tval.tm_yday);
	vs[8] = LVI_INT(tval.tm_isdst);
	return gen_list(9, vs, NULL);
    } else {
	return LVI_BOOL(false);
    }
}

// --8><----8><----8><--

lval wile_closeport(lptr*, lptr args)
{
    if (args[0].vt == LV_FILE_PORT ||
	args[0].vt == LV_SOCK_PORT) {
	return LVI_BOOL(fclose(args[0].v.fp) == 0);
    } else if (args[0].vt == LV_PIPE_PORT) {
	return LVI_BOOL(pclose(args[0].v.fp) == 0);
    } else if (args[0].vt == LV_STR_PORT) {
	return LVI_BOOL(false);
#ifdef WILE_USES_SQLITE
    } else if (args[0].vt == LV_SQLITE_PORT) {
	return LVI_BOOL(sqlite3_close_v2(args[0].v.sqlite_conn) == SQLITE_OK);
#endif // WILE_USES_SQLITE
    } else {
	wile_exception("close-port", "expects one port argument");
    }
}
	   
// --8><----8><----8><--

lval wile_flushport(lptr*, lptr args)
{
    if (args[0].vt == LV_FILE_PORT ||
	args[0].vt == LV_PIPE_PORT ||
	args[0].vt == LV_SOCK_PORT) {
	return LVI_BOOL(fflush(args[0].v.fp) == 0);
    } else if (args[0].vt == LV_STR_PORT) {
	return LVI_BOOL(true);
#ifdef WILE_USES_SQLITE
    } else if (args[0].vt == LV_SQLITE_PORT) {
	// TODO: issue some kind of commit command?
	return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
    } else {
	wile_exception("flush-port", "expects one port argument");
    }
}

// --8><----8><----8><--

lval wile_setlinebuffering(lptr*, lptr args)
{
    if (args[0].vt == LV_FILE_PORT ||
	args[0].vt == LV_PIPE_PORT ||
	args[0].vt == LV_SOCK_PORT) {
	return LVI_BOOL(setvbuf(args[0].v.fp, NULL, _IOLBF, 0) == 0);
    } else if (args[0].vt == LV_STR_PORT) {
	return LVI_BOOL(true);
#ifdef WILE_USES_SQLITE
    } else if (args[0].vt == LV_SQLITE_PORT) {
	return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
    } else {
	wile_exception("set-line-buffering!", "expects one port argument");
    }
}

// --8><----8><----8><--

lval wile_setnobuffering(lptr*, lptr args)
{
    if (args[0].vt == LV_FILE_PORT ||
	args[0].vt == LV_PIPE_PORT ||
	args[0].vt == LV_SOCK_PORT) {
	return LVI_BOOL(setvbuf(args[0].v.fp, NULL, _IONBF, 0) == 0);
    } else if (args[0].vt == LV_STR_PORT) {
	return LVI_BOOL(true);
#ifdef WILE_USES_SQLITE
    } else if (args[0].vt == LV_SQLITE_PORT) {
	return LVI_BOOL(false);
#endif // WILE_USES_SQLITE
    } else {
	wile_exception("set-no-buffering!", "expects one port argument");
    }
}

// --8><----8><----8><--

lval wile_setfilepos2(lptr*, lptr args)
{
    if (args[0].vt != LV_FILE_PORT ||
	args[1].vt != LV_INT) {
	wile_exception("set-file-position",
		       "expects a file port and an offset");
    }
    return LVI_BOOL(fseek(args[0].v.fp, args[1].v.iv, SEEK_SET) == 0);
}

// --8><----8><----8><--

lval wile_setfilepos3(lptr*, lptr args)
{
    if (args[0].vt != LV_FILE_PORT ||
	args[1].vt != LV_INT ||
	args[2].vt != LV_SYMBOL) {
	wile_exception("set-file-position",
		       "expects a file port, an offset, and a location symbol");
    }
    int whence;
    if (strcmp(args[2].v.str, "start") == 0) {
	whence = SEEK_SET;
    } else if (strcmp(args[2].v.str, "cur") == 0) {
	whence = SEEK_CUR;
    } else if (strcmp(args[2].v.str, "end") == 0) {
	whence = SEEK_END;
    } else {
	wile_exception("set-file-position", "got an unknown location symbol");
    }
    return LVI_BOOL(fseek(args[0].v.fp, args[1].v.iv, whence) == 0);
}

// --8><----8><----8><--

lval wile_string_reverse(lptr*, lptr args)
{
    size_t i, j;
    char c;
    if (args[0].vt != LV_STRING) {
	wile_exception("string-reverse", "expects a string argument");
    }
    lval ret = LVI_STRING(args[0].v.str);
    i = 0;
    j = strlen(ret.v.str);
    while (i < j) {
	c = ret.v.str[--j];
	ret.v.str[j] = ret.v.str[i];
	ret.v.str[i++] = c;
    }
    return ret;
}

// --8><----8><----8><--

lval wile_string_hash_32(lptr*, lptr args)
{
    uint32_t hash;
    size_t i, n_os;

    if (args[0].vt != LV_STRING) {
	wile_exception("string-hash-32", "expects a string argument");
    }
    n_os = strlen(args[0].v.str);
    hash = 2166136261U;
    for (i = 0; i < n_os; ++i) {
	hash ^= (unsigned char) (args[0].v.str[i]);
	hash *= 16777619U;
    }
    return LVI_INT(hash);
}

// --8><----8><----8><--

lval wile_string_hash_64(lptr*, lptr args)
{
    uint64_t hash;
    size_t i, n_os;

    if (args[0].vt != LV_STRING) {
	wile_exception("string-hash-64", "expects a string argument");
    }
    n_os = strlen(args[0].v.str);
    hash = 14695981039346656037UL;
    for (i = 0; i < n_os; ++i) {
	hash ^= (unsigned char) (args[0].v.str[i]);
	hash *= 1099511628211UL;
    }
    return LVI_INT(hash);
}

// --8><----8><----8><--

lval wile_string_ci_hash_32(lptr*, lptr args)
{
    uint32_t hash;
    size_t i, n_os;

    if (args[0].vt != LV_STRING) {
	wile_exception("string-ci-hash-32", "expects a string argument");
    }
    n_os = strlen(args[0].v.str);
    hash = 2166136261U;
    for (i = 0; i < n_os; ++i) {
	hash ^= (unsigned char) tolower(args[0].v.str[i]);
	hash *= 16777619U;
    }
    return LVI_INT(hash);
}

// --8><----8><----8><--

lval wile_string_ci_hash_64(lptr*, lptr args)
{
    uint64_t hash;
    size_t i, n_os;

    if (args[0].vt != LV_STRING) {
	wile_exception("string-ci-hash-64", "expects a string argument");
    }
    n_os = strlen(args[0].v.str);
    hash = 14695981039346656037UL;
    for (i = 0; i < n_os; ++i) {
	hash ^= (unsigned char) tolower(args[0].v.str[i]);
	hash *= 1099511628211UL;
    }
    return LVI_INT(hash);
}

// --8><----8><----8><--

lval wile_cfft_good_n(lptr*, lptr args)
{
    if (args[0].vt != LV_INT) {
	wile_exception("cfft-good-n?", "expects an integer argument");
    }
    return LVI_BOOL(swll_cfft_good_n(args[0].v.iv));
}

lval wile_cfft(lptr*, lptr args)
{
    if (args[0].vt != LV_INT || args[1].vt != LV_VECTOR) {
	wile_exception("vector-cfft!", "expects an integer transform direction and a vector of complex values");
    }

    lptr *arr;
    int si;
    size_t n = 0, i;
    lisp_cmplx_t *a1, *a2, *ap;

    swll_cfft_init();

    if (args[0].v.iv > 0) {
	si = 1;
    } else if (args[0].v.iv < 0) {
	si = -1;
    } else {
	wile_exception("vector-cfft!",
		       "transform direction was not specified");
    }

    n = args[1].v.vec.capa;
    if (!swll_cfft_good_n(n)) {
	wile_exception("vector-cfft!",
		       "%zu is not a multiple of (2,3,5,7,11)", n);
    }
    arr = args[1].v.vec.arr;
    a1 = LISP_ALLOC(lisp_cmplx_t, n);
    for (i = 0; i < n; ++i) {
	switch (arr[i]->vt) {
	case LV_INT:
	    a1[i] = CMPLX((lisp_real_t) arr[i]->v.iv, 0.0);
	    break;
	case LV_RAT:
	    a1[i] = CMPLX(LV_RAT2REAL(*(arr[i])), 0.0);
	    break;
	case LV_REAL:
	    a1[i] = CMPLX(arr[i]->v.rv, 0.0);
	    break;
	case LV_CMPLX:
	    a1[i] = arr[i]->v.cv;
	    break;
	default:
	    LISP_FREE(a1);
	    wile_exception("vector-cfft!",
			   "input contains non-numeric value at index %zu", i);
	    break;
	}
    }
    a2 = LISP_ALLOC(lisp_cmplx_t, n);

    ap = swll_cfft(si, n, n, a1, a2);

    for (i = 0; i < n; ++i) {
	arr[i]->vt = LV_CMPLX;
	arr[i]->v.cv = ap[i];
    }

    LISP_FREE(a1);
    LISP_FREE(a2);

    return args[1];
}

// --8><----8><----8><--

void WILE_CONFIG_SYM4(void)
{
}