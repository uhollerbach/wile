// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// This file is part of the wile RTL and is licensed under LGPL
// version 3 or later; see file 'LICENSE-LGPL' for details.

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <inttypes.h>
#include <math.h>

#include "wile.h"
#include "alloc.h"
#include "lib-macros.h"
#include "wile-rtl1.h"

extern lval var_show_sign;
extern lval var_int_base;
extern lval var_flt_base;
extern lval var_flt_precision;

static void print_lisp_char(unsigned char cv, FILE* fp);

static void print_int_bin(lisp_int_t n, char* buf, bool psign);
static void print_int_oct(lisp_int_t n, char* buf, bool psign);
static void print_int_dec(lisp_int_t n, char* buf, bool psign);
static void print_int_hex(lisp_int_t n, char* buf, bool psign);
static void print_int_gen(lisp_int_t n, int base, char* buf, bool psign);

static void print_real_bin(char* buf, /*size_t bsize,*/
			   lisp_real_t n, int base, int prec, bool psign);
static void print_real_dec(char* buf, size_t bsize,
			   lisp_real_t n, int prec, bool psign);

static const char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";

void err_print(const char* fname, lisp_loc_t l_whence,
	       const char* c_whence, const char* fmt, ...)
{
    char buf1[1024], buf2[1280];
    va_list ap;
    lptr err1, err2;

    wile_stack_trace_minimal(fileno(stderr));
    va_start(ap, fmt);
    vsnprintf(buf1, sizeof(buf1), fmt, ap);
    va_end(ap);
    if (fname) {
	snprintf(buf2, sizeof(buf2), "'%s' %s", fname, buf1);
	err2 = new_string(buf2);
    } else {
	err2 = new_string(buf1);
    }
    err1 = new_string("wile");
    err1->vt = LV_SYMBOL;
    cachalot->errval = new_pair(err1, err2);
    cachalot->l_whence = l_whence;
    cachalot->c_whence = c_whence;
    longjmp(cachalot->cenv, 1);
}

#define PUTC(c)		fputc((c), fp)
#define PUTS(s)		fputs((s), fp)

extern lptr display_hooks;

void wile_print_lisp_val(lptr vp, FILE* fp, const char* loc)
{
    char buf[BSIZE];
    size_t i;
    int base, prec;
    char sep;
    bool psign;

    LISP_ASSERT(fp != NULL);
    base = 10;
    prec = INT_MIN;
    psign = false;
    if (vp) {
	if (vp->vt == LV_VECTOR &&
	    vp->v.vec.arr != NULL &&
	    vp->v.vec.capa > 0 && 
	    vp->v.vec.arr[0] != NULL &&
	    vp->v.vec.arr[0]->vt == LV_SYMBOL) {
	    const char* vname = vp->v.vec.arr[0]->v.str;
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
		    LISP_ASSERT(rsym != NULL && rsym->vt == LV_CLAMBDA);
		    lval vs[2];
		    vs[0] = *vp;
		    vs[1] = LVI_FPORT(fp);
		    (void) rsym->v.clambda.fn(rsym->v.clambda.closure, vs, loc);
		    return;
		}
		hooks = CDR(hooks);
	    }
	}

	switch (vp->vt) {
	case LV_NIL:
	    PUTS("()");
	    break;

	case LV_SYMBOL:
	    PUTS(vp->v.str);
	    break;

	case LV_BOOL:
	    PUTC('#');
	    PUTC(vp->v.bv ? 't' : 'f');
	    break;

	case LV_CHAR:
	    print_lisp_char(vp->v.chr, fp);
	    break;

	case LV_STRING:
	    PUTC('"');
	    PUTS(vp->v.str);
	    PUTC('"');
	    break;

	case LV_INT:
	case LV_RAT:
	    if (var_int_base.vt == LV_INT &&
		var_int_base.v.iv >= 2 &&
		var_int_base.v.iv <= 36) {
		base = var_int_base.v.iv;
	    }
	    psign = !(var_show_sign.vt == LV_BOOL &&
		      var_show_sign.v.bv == false);
	    wile_sprint_lisp_num(buf, sizeof(buf), vp, base, INT_MIN, psign);
	    PUTS(buf);
	    break;

	case LV_REAL:
	case LV_CMPLX:
	    if (var_flt_base.vt == LV_INT &&
		(var_flt_base.v.iv == 2 ||
		 var_flt_base.v.iv == 8 ||
		 var_flt_base.v.iv == 10 ||
		 var_flt_base.v.iv == 16)) {
		base = var_flt_base.v.iv;
	    }
	    if (var_flt_precision.vt == LV_INT &&
		var_flt_precision.v.iv >= -999 &&
		var_flt_precision.v.iv <= 999) {
		prec = var_flt_precision.v.iv;
	    }
	    psign = !(var_show_sign.vt == LV_BOOL &&
		      var_show_sign.v.bv == false);
	    wile_sprint_lisp_num(buf, sizeof(buf), vp, base, prec, psign);
	    PUTS(buf);
	    break;

	case LV_PAIR:
	    PUTC('(');
	recurse:
	    wile_print_lisp_val(CAR(vp), fp, loc);
	    if (CDR(vp)) {
		PUTC(' ');
		if (IS_PAIR(CDR(vp))) {
		    vp = CDR(vp);
		    goto recurse;
		} else {
		    PUTC('.');
		    PUTC(' ');
		    wile_print_lisp_val(CDR(vp), fp, loc);
		}
	    }
	    PUTC(')');
	    break;

	case LV_VECTOR:
	    PUTC('#');
	    sep = '(';
	    for (i = 0; i < vp->v.vec.capa; ++i) {
		PUTC(sep);
		wile_print_lisp_val(vp->v.vec.arr[i], fp, loc);
		sep = ' ';
	    }
	    if (sep == '(') {
		PUTC(sep);
	    }
	    PUTC(')');
	    break;

	case LV_BVECTOR:
	    PUTC('#');
	    PUTC('u');
	    PUTC('8');
	    sep = '(';
	    for (i = 0; i < vp->v.bvec.capa; ++i) {
		snprintf(buf, sizeof(buf), "%c#x%x", sep, vp->v.bvec.arr[i]);
		PUTS(buf);
		sep = ' ';
	    }
	    if (sep == '(') {
		PUTC(sep);
	    }
	    PUTC(')');
	    break;

	case LV_FILE_PORT:	PUTS("<file-port>");		break;
	case LV_PIPE_PORT:	PUTS("<pipe-port>");		break;
	case LV_SOCK_PORT:	PUTS("<socket-port>");		break;
	case LV_STR_PORT:	PUTS("<string-port>");		break;
	case LV_SQLITE_PORT:	PUTS("<sqlite-port>");		break;
	case LV_SQLITE_STMT:	PUTS("<sqlite-statement>");	break;
	case LV_CLAMBDA:	PUTS("<compiled-procedure>");	break;
	case LV_ILAMBDA:	PUTS("<interpreted-procedure>");break;
	case LV_CONT:		PUTS("<continuation>");		break;

//// TODO: print the actual code
	case LV_PROMISE:	PUTS("<promise>");		break;

	default:
	    wile_exception("<print>", loc, "bad type %d", vp->vt);
	}
    } else {
	PUTS("()");
    }
}

static void print_lisp_char(unsigned char cv, FILE* fp)
{
    char buf[32];

    switch (cv) {
    case '\0':		PUTS("#\\null");		break;
    case 0x07:		PUTS("#\\alarm");		break;
    case 0x08:		PUTS("#\\backspace");		break;
    case '\t':		PUTS("#\\tab");			break;
    case '\n':		PUTS("#\\newline");		break;
    case 0x0B:		PUTS("#\\vtab");		break;
    case 0x0C:		PUTS("#\\page");		break;
    case '\r':		PUTS("#\\return");		break;
    case 0x1B:		PUTS("#\\escape");		break;
    case ' ':		PUTS("#\\space");		break;
    case 0x7F:		PUTS("#\\delete");		break;

    case '!':  case '"':  case '#':  case '$':  case '%':  case '&':
    case '\'': case '(':  case ')':  case '*':  case '+':  case ',':
    case '-':  case '.':  case '/':  case '0':  case '1':  case '2':
    case '3':  case '4':  case '5':  case '6':  case '7':  case '8':
    case '9':  case ':':  case ';':  case '<':  case '=':  case '>':
    case '?':  case '@':  case 'A':  case 'B':  case 'C':  case 'D':
    case 'E':  case 'F':  case 'G':  case 'H':  case 'I':  case 'J':
    case 'K':  case 'L':  case 'M':  case 'N':  case 'O':  case 'P':
    case 'Q':  case 'R':  case 'S':  case 'T':  case 'U':  case 'V':
    case 'W':  case 'X':  case 'Y':  case 'Z':  case '[':  case '\\':
    case ']':  case '^':  case '_':  case '`':  case 'a':  case 'b':
    case 'c':  case 'd':  case 'e':  case 'f':  case 'g':  case 'h':
    case 'i':  case 'j':  case 'k':  case 'l':  case 'm':  case 'n':
    case 'o':  case 'p':  case 'q':  case 'r':  case 's':  case 't':
    case 'u':  case 'v':  case 'w':  case 'x':  case 'y':  case 'z':
    case '{':  case '|':  case '}':  case '~':
	PUTS("#\\");
	PUTC(cv);
	break;

    default:
	snprintf(buf, sizeof(buf), "#\\x%02x", (int) cv);
	PUTS(buf);
    }
}

void wile_sprint_lisp_num(char* buf, size_t bsize, lptr num,
			  int base, int prec, bool psign)
{
    char b0[8], b1[BSIZE], b2[BSIZE];
    lisp_real_t rval;
    lval part;

    LISP_ASSERT(num != NULL);

    LISP_ASSERT(base >= 2 && base <= 36);
    switch (base) {
    case 2:	strcpy(b0, "#b");				break;
    case 8:	strcpy(b0, "#o");				break;
    case 10:	*b0 = '\0';					break;
    case 16:	strcpy(b0, "#x");				break;
    default:	snprintf(b0, sizeof(b0), "#{%d}", base);	break;
    }

    switch (num->vt) {
    case LV_INT:
	if (prec == INT_MIN) {
	    switch (base) {
	    case 2:	print_int_bin(num->v.iv, b1, psign);		break;
	    case 8:	print_int_oct(num->v.iv, b1, psign);		break;
	    case 10:	print_int_dec(num->v.iv, b1, psign);		break;
	    case 16:	print_int_hex(num->v.iv, b1, psign);		break;
	    default:	print_int_gen(num->v.iv, base, b1, psign);	break;
	    }
	    snprintf(buf, bsize, "%s%s", b0, b1);
	} else {
	    rval = num->v.iv;
	    goto real_case;
	}
	break;

    case LV_RAT:
	CAN_RAT(num->v.irv.num, num->v.irv.den);
	if (prec == INT_MIN) {
	    switch (base) {
	    case 2:
		print_int_bin(num->v.irv.num, b1, psign);
		print_int_bin(num->v.irv.den, b2, false);
		break;
	    case 8:
		print_int_oct(num->v.irv.num, b1, psign);
		print_int_oct(num->v.irv.den, b2, false);
		break;
	    case 10:
		print_int_dec(num->v.irv.num, b1, psign);
		print_int_dec(num->v.irv.den, b2, false);
		break;
	    case 16:
		print_int_hex(num->v.irv.num, b1, psign);
		print_int_hex(num->v.irv.den, b2, false);
		break;
	    default:
		print_int_gen(num->v.irv.num, base, b1, psign);
		print_int_gen(num->v.irv.den, base, b2, false);
		break;
	    }
	    snprintf(buf, bsize, "%s%s/%s", b0, b1, b2);
	} else {
	    rval = LV_RAT2REAL(*num);
	    goto real_case;
	}
	break;

    case LV_REAL:
	rval = num->v.rv;
    real_case:
	if (ISNAN(rval)) {
	    if (COPYSIGN(1.0, rval) > 0) {
		strcpy(buf, "+NaN.0");
	    } else {
		strcpy(buf, "-NaN.0");
	    }
	} else if (ISINF(rval)) {
	    if (rval > 0.0) {
		strcpy(buf, "+Inf.0");
	    } else {
		strcpy(buf, "-Inf.0");
	    }
	} else {
	    if (prec == INT_MIN) {
#if defined(WILE_USES_QUAD_DOUBLE)
		prec = -35;
#elif defined(WILE_USES_LONG_DOUBLE)
		prec = -21;
#else
		prec = -17;
#endif	// WILE_USES_QUAD_DOUBLE
	    } else if (prec > 250) {
		prec = 250;
	    } else if (prec < -250) {
		prec = -250;
	    }

	    if (base == 10) {
		print_real_dec(buf, bsize, rval, prec, psign);
	    } else if (base == 2 || base == 8 || base == 16) {
		print_real_bin(buf, /*bsize,*/ rval, base, prec, psign);
	    } else {
		ERR("number->string", "bad base %d for FP values", base);
	    }
	}
	break;

    case LV_CMPLX:
	// TODO: prettify more?
	part.vt = LV_REAL;
	if (ISNAN(CIMAG(num->v.cv)) ||
	    ISINF(CIMAG(num->v.cv)) ||
	    CIMAG(num->v.cv) != 0.0) {
	    if (ISNAN(CREAL(num->v.cv)) ||
		ISINF(CREAL(num->v.cv)) ||
		CREAL(num->v.cv) != 0.0) {
		part.v.rv = CREAL(num->v.cv);
		wile_sprint_lisp_num(b1, sizeof(b1), &part, base, prec, psign);
		part.v.rv = CIMAG(num->v.cv);
		wile_sprint_lisp_num(b2, sizeof(b2), &part, base, prec, true);
		snprintf(buf, bsize, "%s%si", b1, (*b2 == '#') ? b2 + 2 : b2);
	    } else {
		part.v.rv = CIMAG(num->v.cv);
		wile_sprint_lisp_num(b2, sizeof(b2), &part, base, prec, psign);
		snprintf(buf, bsize, "%si", b2);
	    }
	} else {
	    part.v.rv = CREAL(num->v.cv);
	    wile_sprint_lisp_num(buf, bsize, &part, base, prec, psign);
	}
	break;

    default:
	ERR("number->string", "expects a numeric argument");
    }
}

#define GEN_BODY(base)					\
    do {						\
	char bl[BSIZE], *bp;				\
	lisp_uint_t un;					\
							\
	un = n;						\
	if (n == 0) {					\
	    *buf++ = '0';				\
	    *buf = '\0';				\
	    return;					\
	} else if (n < 0) {				\
	    *buf++ = '-';				\
	    un = -n;					\
	} else if (psign) {				\
	    *buf++ = '+';				\
	}						\
	bp = bl;					\
	while (un != 0) {				\
	    *bp++ = digits[un%(base)];			\
	    un /= (base);				\
	}						\
	while (bp != bl) {				\
	    --bp;					\
	    *buf = *bp;					\
	    ++buf;					\
	}						\
	*buf = '\0';					\
    } while (0)

static void print_int_bin(lisp_int_t n, char* buf, bool psign)
{
    GEN_BODY(2);
}

static void print_int_oct(lisp_int_t n, char* buf, bool psign)
{
    if (n > INT64_MIN && n < INT64_MAX) {
	if (n < 0) {
	    *buf++ = '-';
	    n = -n;
	} else if (psign) {
	    *buf++ = '+';
	}
	sprintf(buf, "%" PRIo64, (int64_t) n);
    } else {
	GEN_BODY(8);
    }
}

static void print_int_dec(lisp_int_t n, char* buf, bool psign)
{
    if (n > INT64_MIN && n < INT64_MAX) {
	sprintf(buf, psign ? "%+" PRId64 : "%" PRId64, (int64_t) n);
    } else {
	GEN_BODY(10);
    }
}

static void print_int_hex(lisp_int_t n, char* buf, bool psign)
{
    if (n > INT64_MIN && n < INT64_MAX) {
	if (n < 0) {
	    *buf++ = '-';
	    n = -n;
	} else if (psign) {
	    *buf++ = '+';
	}
	sprintf(buf, "%" PRIx64, (int64_t) n);
    } else {
	GEN_BODY(16);
    }
}

static void print_int_gen(lisp_int_t n, int base, char* buf, bool psign)
{
    LISP_ASSERT(base >= 2 && base <= 36);
    GEN_BODY(base);
}

static void print_real_bin(char* buf, /*size_t bsize,*/ lisp_real_t n,
			   int base, int prec, bool psign)
{
    char *bp = buf;
    int bl, ex, iprec;
    lisp_int_t mi;
    static const char pref[] = "_bqox";
    static const char hexd[] = "0123456789abcdef";

    iprec = prec;

    switch (base) {
    case 2:	bl = 1;		break;
    case 8:	bl = 3;		break;
    case 16:	bl = 4;		break;
    default:	wile_exception("number->string", LISP_WHENCE,
			       "bad base %d!", base);
    }

 retry:		// if %f-style notation runs out of steam, we can overflow
		// and crash; avoid this and switch to %e-style instead

    sprintf(bp, "#%c", pref[bl]);
    bp += 2;
    if (n < REAL_LIT(0.0)) {
	sprintf(bp, "-");
	n = -n;
	++bp;
    }

    if (prec >= 0) {
	mi = FLOOR(n);
	switch (base) {
	case 2:		print_int_bin(mi, bp, psign);	break;
	case 8:		print_int_oct(mi, bp, psign);	break;
	case 16:	print_int_hex(mi, bp, psign);	break;
	}
	while (*bp) {
	    ++bp;
	}
	*bp++ = '.';

	while (prec > 0) {
	    n -= mi;
	    n *= base;
	    mi = (int) FLOOR(n);
	    if (mi < 0 || mi >= base) {
		prec = -iprec;
		bp = buf;
		goto retry;
	    }

	    *bp++ = hexd[mi];
	    --prec;
	}
	*bp = '\0';
    } else {
	n = FREXP(n, &ex);
	while (ex%bl != 0) {
	    --ex;
	    n *= REAL_LIT(2.0);
	}
	ex /= bl;
	if (n < REAL_LIT(1.0)) {
	    --ex;
	    n *= base;
	}
	mi = (int) FLOOR(n);
	LISP_ASSERT(mi >= 0 && mi < base);
	if (psign) {
	    *bp++ = '+';
	}
	*bp++ = hexd[mi];
	*bp++ = '.';

	while (prec < 0) {
	    n -= mi;
	    n *= base;
	    mi = (int) FLOOR(n);
	    *bp++ = hexd[mi];
	    ++prec;
	}

	*bp++ = (base == 16) ? 'x' : 'e';
	switch (base) {
	case 2:	print_int_bin(ex, bp, false);	break;
	case 8:
	    if (ex >= 0) {
		sprintf(bp, "%o", ex);
	    } else {
		sprintf(bp, "-%o", -ex);
	    }
	    break;
	case 16:
	    if (ex >= 0) {
		sprintf(bp, "%x", ex);
	    } else {
		sprintf(bp, "-%x", -ex);
	    }
	    break;
	}
    }
}

static void print_real_dec(char* buf, size_t bsize,
			   lisp_real_t n, int prec, bool psign)
{
    char pfmt[32], f;

    if (prec < 0) {
	f = 'e';
	prec = -prec;
    } else {
	f = 'f';
    }
    if (psign && n > REAL_LIT(0.0)) {
	*buf++ = '+';
	--bsize;
    }

#if defined(WILE_USES_QUAD_DOUBLE)
    snprintf(pfmt, sizeof(pfmt), "%%.%dQ%c", prec, f);
    quadmath_snprintf(buf, bsize, pfmt, n);
#elif defined(WILE_USES_LONG_DOUBLE)
    snprintf(pfmt, sizeof(pfmt), "%%.%dL%c", prec, f);
    snprintf(buf, bsize, pfmt, n);
#else
    snprintf(pfmt, sizeof(pfmt), "%%.%d%c", prec, f);
    snprintf(buf, bsize, pfmt, n);
#endif
}

const char* typename(enum val_type vt)
{
    switch (vt) {
    case LV_SYMBOL:		return "symbol";
    case LV_BOOL:		return "boolean";
    case LV_CHAR:		return "character";
    case LV_STRING:		return "string";
    case LV_INT:		return "integer";
    case LV_RAT:		return "rational";
    case LV_REAL:		return "real";
    case LV_CMPLX:		return "complex";
    case LV_PAIR:		return "pair";
    case LV_FILE_PORT:		return "file-port";
    case LV_PIPE_PORT:		return "pipe-port";
    case LV_SOCK_PORT:		return "socket-port";
    case LV_STR_PORT:		return "string-port";
    case LV_SQLITE_PORT:	return "sqlite-port";
    case LV_SQLITE_STMT:	return "sqlite-statement";
    case LV_VECTOR:		return "vector";
    case LV_BVECTOR:		return "bytevector";
    case LV_PROMISE:		return "promise";
    case LV_CLAMBDA:		return "compiled-procedure";
    case LV_ILAMBDA:		return "interpreted-procedure";
    case LV_CONT:		return "continuation";

    case VT_UNINIT:
	wile_exception("typename", LISP_WHENCE, "uninitialized lisp-val!");

    default:
	wile_exception("typename", LISP_WHENCE, "unknown type!?!");
    }
}
