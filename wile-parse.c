/* -*- mode: c; -*-
This file is part of yucc... Uwe's compiler-compiler
Copyright 2013-2014, Uwe Hollerbach <uhollerbach@gmail.com>
License: 2clause BSD, see file 'LICENSE' for details
$Id: yucc_c.skel,v 1.39 2016/02/18 07:21:01 uwe Exp $
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#define __USE_XOPEN_EXTENDED	/* to see strdup on linux */
#include <string.h>
#include <limits.h>

#define YUCC_ME_FECIT


#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <math.h>

#include "wile.h"
#include "alloc.h"
#include "wile-lex.h"

static lptr cp_lv(const lptr old);
static lptr rev_lst(lptr list, lptr tail);

#define PUSHQ(q)				\
    do {					\
	YYSTYPE v;				\
	v.vt = LV_SYMBOL;			\
	v.v.str = LISP_STRDUP(q);		\
	LISP_ASSERT(v.v.str != NULL);		\
	PUSH(v);				\
    } while (0)

#include "wile-parse.h"

static void yy_exit(int status, const char* fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    exit(status);
}


struct yy_symbol {
    unsigned int id;
    const char* name;
    const unsigned int* row;
    const unsigned int* first;
    const unsigned int* follow;
    unsigned int aid;
};

struct yy_rule {
    unsigned int n_rhs;
    const unsigned int* rhs;
};

static const unsigned int yy_n_terms = 19;
static const unsigned int yy_n_nonterms = 26;

static const unsigned int yy_start_id = 271;
static const unsigned int yy_end_id = 294;

static const unsigned int yy_row_sym[] = { 39, 40, 41, 44, 46, 64, 96, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 294 };

static const unsigned int yy_row_271[] = { 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
static const unsigned int yy_row_269[] = { 6, 3, 0, 6, 0, 0, 6, 2, 5, 2, 2, 0, 2, 2, 2, 2, 2, 4, 0 };
static const unsigned int yy_row_267[] = { 0, 0, 0, 0, 0, 0, 0, 8, 0, 9, 14, 0, 11, 12, 13, 10, 7, 0, 0 };
static const unsigned int yy_row_272[] = { 15, 0, 0, 17, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
static const unsigned int yy_row_268[] = { 18, 18, 18, 18, 18, 0, 18, 18, 18, 18, 18, 0, 18, 18, 18, 18, 18, 18, 0 };
static const unsigned int yy_row_270[] = { 19, 19, 19, 19, 19, 0, 19, 19, 19, 19, 19, 0, 19, 19, 19, 19, 19, 19, 19 };
static const unsigned int yy_row_274[] = { 20, 20, 0, 20, 0, 0, 20, 20, 20, 20, 20, 0, 20, 20, 20, 20, 20, 20, 21 };
static const unsigned int yy_row_273[] = { 23, 23, 0, 23, 0, 0, 23, 23, 23, 23, 23, 22, 23, 23, 23, 23, 23, 23, 23 };
static const unsigned int yy_row_277[] = { 0, 0, 25, 0, 24, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
static const unsigned int yy_row_275[] = { 26, 26, 27, 26, 27, 0, 26, 26, 26, 26, 26, 0, 26, 26, 26, 26, 26, 26, 0 };
static const unsigned int yy_row_276[] = { 0, 0, 0, 0, 28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
static const unsigned int yy_row_278[] = { 29, 29, 30, 29, 0, 0, 29, 29, 29, 29, 29, 0, 29, 29, 29, 29, 29, 29, 0 };
static const unsigned int yy_row_279[] = { 31, 31, 32, 31, 0, 0, 31, 31, 31, 31, 31, 0, 31, 31, 31, 31, 31, 31, 0 };
static const unsigned int yy_row_280[] = { 34, 34, 0, 34, 0, 33, 34, 34, 34, 34, 34, 0, 34, 34, 34, 34, 34, 34, 0 };

static const unsigned int yy_fi_267[] = { 256, 258, 259, 261, 262, 263, 264, 265, 0 };
static const unsigned int yy_fo_267[] = { 39, 40, 41, 44, 46, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 294, 0 };
static const unsigned int yy_fi_268[] = { 293, 0 };
static const unsigned int yy_fo_268[] = { 39, 40, 41, 44, 46, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 0 };
static const unsigned int yy_fi_269[] = { 39, 40, 44, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 0 };
static const unsigned int yy_fo_269[] = { 39, 40, 41, 44, 46, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 294, 0 };
static const unsigned int yy_fi_270[] = { 293, 0 };
static const unsigned int yy_fo_270[] = { 39, 40, 41, 44, 46, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 294, 0 };
static const unsigned int yy_fi_271[] = { 39, 40, 44, 96, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 293, 0 };
static const unsigned int yy_fo_271[] = { 294, 0 };
static const unsigned int yy_fi_272[] = { 39, 44, 96, 0 };
static const unsigned int yy_fo_272[] = { 39, 40, 44, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 0 };
static const unsigned int yy_fi_273[] = { 260, 293, 0 };
static const unsigned int yy_fo_273[] = { 39, 40, 44, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 294, 0 };
static const unsigned int yy_fi_274[] = { 39, 40, 44, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 293, 0 };
static const unsigned int yy_fo_274[] = { 294, 0 };
static const unsigned int yy_fi_275[] = { 39, 40, 44, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 293, 0 };
static const unsigned int yy_fo_275[] = { 41, 46, 0 };
static const unsigned int yy_fi_276[] = { 46, 0 };
static const unsigned int yy_fo_276[] = { 41, 0 };
static const unsigned int yy_fi_277[] = { 46, 293, 0 };
static const unsigned int yy_fo_277[] = { 41, 0 };
static const unsigned int yy_fi_278[] = { 39, 40, 44, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 293, 0 };
static const unsigned int yy_fo_278[] = { 41, 0 };
static const unsigned int yy_fi_279[] = { 39, 40, 44, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 293, 0 };
static const unsigned int yy_fo_279[] = { 41, 0 };
static const unsigned int yy_fi_280[] = { 64, 293, 0 };
static const unsigned int yy_fo_280[] = { 39, 40, 44, 96, 256, 257, 258, 259, 261, 262, 263, 264, 265, 266, 0 };

/* If user specifies token values,
   this table won't generally be sorted! */
static const struct yy_symbol yy_symbols[] = {
    { 39, "'\''", NULL, NULL, NULL, 0 },
    { 40, "'('", NULL, NULL, NULL, 0 },
    { 41, "')'", NULL, NULL, NULL, 0 },
    { 44, "','", NULL, NULL, NULL, 0 },
    { 46, "'.'", NULL, NULL, NULL, 0 },
    { 64, "'@'", NULL, NULL, NULL, 0 },
    { 96, "'`'", NULL, NULL, NULL, 0 },
    { 256, "BOOLEAN", NULL, NULL, NULL, 0 },
    { 257, "BVECTOR", NULL, NULL, NULL, 0 },
    { 258, "CHARACTER", NULL, NULL, NULL, 0 },
    { 259, "COMPLEX", NULL, NULL, NULL, 0 },
    { 260, "HASHBANG", NULL, NULL, NULL, 0 },
    { 261, "INTEGER", NULL, NULL, NULL, 0 },
    { 262, "RATIONAL", NULL, NULL, NULL, 0 },
    { 263, "REAL", NULL, NULL, NULL, 0 },
    { 264, "STRING", NULL, NULL, NULL, 0 },
    { 265, "SYMBOL", NULL, NULL, NULL, 0 },
    { 266, "VECTOR", NULL, NULL, NULL, 0 },
    { 267, "atomic", yy_row_267, yy_fi_267, yy_fo_267, 0 },
    { 268, "b-list", yy_row_268, yy_fi_268, yy_fo_268, 0 },
    { 269, "datum", yy_row_269, yy_fi_269, yy_fo_269, 0 },
    { 270, "e-list", yy_row_270, yy_fi_270, yy_fo_270, 0 },
    { 271, "file", yy_row_271, yy_fi_271, yy_fo_271, 0 },
    { 272, "quote", yy_row_272, yy_fi_272, yy_fo_272, 0 },
    { 273, "~aux1", yy_row_273, yy_fi_273, yy_fo_273, 0 },
    { 274, "~aux2", yy_row_274, yy_fi_274, yy_fo_274, 0 },
    { 275, "~aux3", yy_row_275, yy_fi_275, yy_fo_275, 0 },
    { 276, "~aux4", yy_row_276, yy_fi_276, yy_fo_276, 0 },
    { 277, "~aux5", yy_row_277, yy_fi_277, yy_fo_277, 0 },
    { 278, "~aux6", yy_row_278, yy_fi_278, yy_fo_278, 0 },
    { 279, "~aux7", yy_row_279, yy_fi_279, yy_fo_279, 0 },
    { 280, "~aux8", yy_row_280, yy_fi_280, yy_fo_280, 0 },
    { 281, NULL, NULL, NULL, NULL, 1 },
    { 282, NULL, NULL, NULL, NULL, 10 },
    { 283, NULL, NULL, NULL, NULL, 11 },
    { 284, NULL, NULL, NULL, NULL, 12 },
    { 285, NULL, NULL, NULL, NULL, 2 },
    { 286, NULL, NULL, NULL, NULL, 3 },
    { 287, NULL, NULL, NULL, NULL, 4 },
    { 288, NULL, NULL, NULL, NULL, 5 },
    { 289, NULL, NULL, NULL, NULL, 6 },
    { 290, NULL, NULL, NULL, NULL, 7 },
    { 291, NULL, NULL, NULL, NULL, 8 },
    { 292, NULL, NULL, NULL, NULL, 9 },
    { 294, "end of input", NULL, NULL, NULL, 0 }
};

static const unsigned int yy_rule_1[] = { 273, 274, 281, };
static const unsigned int yy_rule_2[] = { 267, 285, };
static const unsigned int yy_rule_3[] = { 40, 268, 275, 277, 41, 270, };
static const unsigned int yy_rule_4[] = { 266, 268, 278, 41, 270, 287, };
static const unsigned int yy_rule_5[] = { 257, 268, 279, 41, 270, 288, };
static const unsigned int yy_rule_6[] = { 272, 269, 289, };
static const unsigned int yy_rule_7[] = { 265, };
static const unsigned int yy_rule_8[] = { 256, };
static const unsigned int yy_rule_9[] = { 258, };
static const unsigned int yy_rule_10[] = { 264, };
static const unsigned int yy_rule_11[] = { 261, };
static const unsigned int yy_rule_12[] = { 262, };
static const unsigned int yy_rule_13[] = { 263, };
static const unsigned int yy_rule_14[] = { 259, };
static const unsigned int yy_rule_15[] = { 39, 290, };
static const unsigned int yy_rule_16[] = { 96, 291, };
static const unsigned int yy_rule_17[] = { 44, 280, };
static const unsigned int yy_rule_18[] = { 283, };
static const unsigned int yy_rule_19[] = { 284, };
static const unsigned int yy_rule_20[] = { 269, 274, };
static const unsigned int yy_rule_22[] = { 260, };
static const unsigned int yy_rule_24[] = { 276, };
static const unsigned int yy_rule_26[] = { 269, 275, };
static const unsigned int yy_rule_28[] = { 46, 269, 286, };
static const unsigned int yy_rule_29[] = { 269, 278, };
static const unsigned int yy_rule_31[] = { 269, 279, };
static const unsigned int yy_rule_33[] = { 64, 292, };
static const unsigned int yy_rule_34[] = { 282, };

static const struct yy_rule yy_rules[] = {
    { 0, NULL },	/* placeholder */
    { 3, yy_rule_1 },
    { 2, yy_rule_2 },
    { 6, yy_rule_3 },
    { 6, yy_rule_4 },
    { 6, yy_rule_5 },
    { 3, yy_rule_6 },
    { 1, yy_rule_7 },
    { 1, yy_rule_8 },
    { 1, yy_rule_9 },
    { 1, yy_rule_10 },
    { 1, yy_rule_11 },
    { 1, yy_rule_12 },
    { 1, yy_rule_13 },
    { 1, yy_rule_14 },
    { 2, yy_rule_15 },
    { 2, yy_rule_16 },
    { 2, yy_rule_17 },
    { 1, yy_rule_18 },
    { 1, yy_rule_19 },
    { 2, yy_rule_20 },
    { 0, NULL },
    { 1, yy_rule_22 },
    { 0, NULL },
    { 1, yy_rule_24 },
    { 0, NULL },
    { 2, yy_rule_26 },
    { 0, NULL },
    { 3, yy_rule_28 },
    { 2, yy_rule_29 },
    { 0, NULL },
    { 2, yy_rule_31 },
    { 0, NULL },
    { 2, yy_rule_33 },
    { 1, yy_rule_34 },
};

static const unsigned int yy_sync_sym[]
    __attribute__((unused)) = { 295 };
static const unsigned int yy_n_syncs __attribute__((unused)) = 0;

enum yy_tok_src { YY_INTERNAL, YY_EXTERNAL };
static const struct yy_symbol yy_unknown = { 0, "unknown token", NULL, NULL, NULL, 0 };

static const struct yy_symbol* yy_get_sym(unsigned int token,
					  enum yy_tok_src src)
{
    unsigned int i;

    for (i = 0; i < yy_n_terms + yy_n_nonterms; ++i) {
	if (token == yy_symbols[i].id) {
	    return(yy_symbols + i);
	}
    }
    if (src == YY_INTERNAL) {
	yy_exit(1, "wile_parse error: yy_get_sym unknown token %u!?!\n", token);
	return(NULL);	/* never reached; silence a compiler warning */
    } else {
	return(&yy_unknown);
    }
}

static unsigned int yy_get_index(unsigned int yy_new)
{
    unsigned int i;

    unsigned int lo, hi;

    /* binary search, O(log(N)) */
    lo = 0;
    hi = yy_n_terms - 1;
    while (hi >= lo) {
	i = (hi + lo)/2;
	if (yy_row_sym[i] == yy_new) {
	    return(i);
	} else if (yy_row_sym[i] > yy_new) {
	    hi = i - 1;
	} else {
	    lo = i + 1;
	}
    }
    yy_exit(1, "wile_parse error: unknown token %u!?!\n", yy_new);
    return(0);	/* never reached; silence a compiler warning */
}

static void yy_print_context(unsigned char** toks, int n_toks, size_t line_no)
{
    fprintf(stderr, " at line %zu:\n  ...", line_no);
    while (n_toks-- > 0) {
	if (*toks) {
	    fputc(' ', stderr);
	    fputs((char*) *toks, stderr);
	}
	++toks;
    }
    fputs(" <---\n", stderr);
}

#define YY_N_TOKS	8

#define YY_GROW_VS()					\
    do {						\
	if (yy_vss >= yy_vsmax) {			\
	    yy_vsmax *= 2;				\
	    yy_vs =					\
		LISP_REALLOC(YYSTYPE, yy_vs, yy_vsmax);	\
	    LISP_ASSERT(yy_vs != NULL);			\
	}						\
    } while (0)

/* The following three macros are for use in user actions, to get
   values onto and off the value stack. Hence no YY_ prefix. */

#define PUSH(val)					\
    do {						\
	YY_GROW_VS();					\
	yy_vs[yy_vss++] = val;				\
    } while (0)

#define PEEK(val)					\
    do {						\
	if (yy_vss == 0) {				\
	    fprintf(stderr,				\
		    "wile_parse error: value"	\
		    " stack underflow at %s:%d\n",	\
		    __FILE__, __LINE__);		\
	    ++yy_err;					\
	    goto yy_finish;				\
	}						\
	val = yy_vs[yy_vss-1];				\
    } while (0)

#define POP(val)					\
    do {						\
	if (yy_vss == 0) {				\
	    fprintf(stderr,				\
		    "wile_parse error: value"	\
		    " stack underflow at %s:%d\n",	\
		    __FILE__, __LINE__);		\
	    ++yy_err;					\
	    goto yy_finish;				\
	}						\
	val = yy_vs[--yy_vss];				\
    } while (0)

#define YY_LEX()					\
    do {						\
	while (1) {					\
	    yy_lexval.vt = VT_UNINIT;			\
	    yy_new =					\
		wile_lex(yy_context, &yy_lexval,	\
			      user_data, &yy_text);	\
	    if (yy_new == 0) {				\
		yy_new = yy_end_id;			\
	    }						\
	    LISP_FREE(yy_toks[0]);			\
	    for (yy_i = 0; yy_i < YY_N_TOKS - 1; ++yy_i) {\
		yy_toks[yy_i] = yy_toks[yy_i+1];	\
	    }						\
	    yy_toks[YY_N_TOKS-1] =			\
		(unsigned char*) LISP_STRDUP((char*) yy_text);	\
	    if (yy_get_sym(yy_new, YY_EXTERNAL) == &yy_unknown) {\
		fprintf(stderr, "wile_parse error: unknown token '%s'", yy_text);\
		yy_print_context(yy_toks, YY_N_TOKS,	\
				 ulex_lineno(yy_context));\
		++yy_err;				\
	    } else {					\
		break;					\
	    }						\
	}						\
	if (yy_lcur.vt != VT_UNINIT) {			\
	    YY_GROW_VS();				\
	    yy_vs[yy_vss++] = yy_lcur;			\
	}						\
	yy_lcur = yy_lexval;				\
    } while (0)

#define YY_TOK(yi)					\
    do {						\
	if (yy_tss == 0) {				\
	    fprintf(stderr, "wile_parse error: token stack underflow\n");\
	    ++yy_err;					\
	    goto yy_finish;				\
	}						\
	yy_tos = yy_ts[yi];				\
    } while (0)

#define YY_POP_TOKEN()	YY_TOK(--yy_tss)
#define YY_PEEK_TOKEN()	YY_TOK(yy_tss-1)

/* For use inside user actions, to indicate failure; don't attempt recovery,
   as anything at all could have gone wrong at this point */

#define FAIL()						\
    do {						\
	fprintf(stderr,					\
		"wile_parse error: action %u failed", \
		yy_sp->aid);				\
	yy_print_context(yy_toks, YY_N_TOKS,		\
			 ulex_lineno(yy_context));	\
	++yy_err;					\
	goto yy_finish;					\
    } while (0)

unsigned int wile_parse(struct ulex_context* yy_context,
			     void* user_data,
			     int yy_do_actions)
{
    unsigned int *yy_ts, yy_tos, yy_new, yy_i, yy_err;
    unsigned char* yy_text;
    unsigned char* yy_toks[YY_N_TOKS];
    size_t yy_tss, yy_tsmax, yy_vss, yy_vsmax;
    const struct yy_symbol* yy_sp;
    const struct yy_rule* yy_rp;
    YYSTYPE yy_lcur, yy_lexval, *yy_vs;
    const unsigned int* yy_fp;


    YYSTYPE vv;
    lptr stk = new_pair(NULL, NULL), tail = NULL, np;
    lptr* ret = (lptr*) user_data;

    yy_tsmax = yy_vsmax = 16;
    yy_ts = LISP_ALLOC(unsigned int, yy_tsmax);
    LISP_ASSERT(yy_ts != NULL);
    yy_vs = LISP_ALLOC(YYSTYPE, yy_vsmax);
    LISP_ASSERT(yy_vs != NULL);
    yy_tss = yy_vss = 0;
    yy_ts[yy_tss++] = yy_end_id;
    yy_ts[yy_tss++] = yy_start_id;
    yy_lcur.vt = VT_UNINIT;
    yy_err = 0;
    for (yy_i = 0; yy_i < YY_N_TOKS; ++yy_i) {
	yy_toks[yy_i] = NULL;
    }
    YY_LEX();
    while (1) {
	YY_PEEK_TOKEN();
	yy_sp = yy_get_sym(yy_tos, YY_INTERNAL);
	if (yy_sp->name == NULL) {		/* an action */
	    YY_POP_TOKEN();
	    if (yy_do_actions) {
		switch (yy_sp->aid) {

case 1: {
 *ret = rev_lst(CDR(stk), NULL); 
} break;
case 2: {
 POP(vv); CDR(stk) = new_pair(cp_lv(&vv), CDR(stk)); 
} break;
case 3: {
 np = CDR(stk);
				CDR(stk) = CDR(np);
				tail = CAR(np);
				LISP_FREE_LV(np); 
} break;
case 4: {
 CADR(stk) = lst2vec(CADR(stk)); 
} break;
case 5: {
 CADR(stk) = lst2bvec(CADR(stk)); 
} break;
case 6: {
 POP(vv);
			 np = new_pair(CADR(stk), NULL);
			 CADR(stk) = new_pair(cp_lv(&vv), np); 
} break;
case 7: {
 PUSHQ("quote"); 
} break;
case 8: {
 PUSHQ("quasiquote"); 
} break;
case 9: {
 PUSHQ("unquote-splicing"); 
} break;
case 10: {
 PUSHQ("unquote"); 
} break;
case 11: {
 stk = new_pair(stk, NULL); tail = NULL; 
} break;
case 12: {
 np = stk;
	     stk = CAR(stk);
	     if (stk == NULL) {
		 FAIL();
	     }
	     CAR(np) = rev_lst(CDR(np), tail);
	     CDR(np) = CDR(stk);
	     CDR(stk) = np;
	     tail = NULL; 
} break;

		default:
		    yy_exit(1, "wile_parse error: unknown action!?!\n");
		}
	    }
	} else if (yy_sp->row == NULL) {	/* a terminal */
	    if (yy_tos == yy_new) {
		if (yy_tos == yy_end_id) {
		    /* From here is the error-free return:
		       don't increment the error count */
		    goto yy_finish;
		} else {
		    YY_POP_TOKEN();
		    YY_LEX();
		}
	    } else {
		++yy_err;
		fprintf(stderr,
			"wile_parse error: looking for %s, saw %s",
			yy_sp->name, yy_get_sym(yy_new, YY_EXTERNAL)->name);
		yy_print_context(yy_toks, YY_N_TOKS,
				 ulex_lineno(yy_context));
		YY_POP_TOKEN();
	    }
	} else {			/* a non-terminal */
	    yy_i = yy_get_index(yy_new);
	    if (yy_sp->row[yy_i] > 0) {
		YY_POP_TOKEN();
		yy_rp = yy_rules + yy_sp->row[yy_i];
		yy_i = yy_rp->n_rhs;
		while (yy_i-- > 0) {
		    if (yy_tss >= yy_tsmax) {
			yy_tsmax *= 2;
			yy_ts = LISP_REALLOC(unsigned int, yy_ts, yy_tsmax);
			LISP_ASSERT(yy_ts != NULL);
		    }
		    yy_ts[yy_tss++] = yy_rp->rhs[yy_i];
		}
	    } else {
		++yy_err;
		fprintf(stderr,
			"wile_parse error: trying to expand %s, saw %s",
			yy_sp->name, yy_get_sym(yy_new, YY_EXTERNAL)->name);
		yy_print_context(yy_toks, YY_N_TOKS,
				 ulex_lineno(yy_context));
		while (1) {
		    YY_LEX();
		    yy_fp = yy_sp->first;
		    while (*yy_fp > 0) {
			if (*yy_fp == yy_new) {
			    break;
			}
			++yy_fp;
		    }
		    if (*yy_fp > 0) {
			break;
		    }
		    yy_fp = yy_sp->follow;
		    while (*yy_fp > 0) {
			if (*yy_fp == yy_new) {
			    break;
			}
			++yy_fp;
		    }
		    if (*yy_fp > 0) {
			YY_POP_TOKEN();
			break;
		    }
		    if (yy_new == yy_end_id) {
			goto yy_finish;
		    }
		}
	    }
	}
    }
  yy_finish:
    LISP_FREE(yy_ts);
    LISP_FREE(yy_vs);
    return(yy_err);
}





static lptr cp_lv(const lptr old)
{
    lptr ret = new_lv(old->vt);
    ret->v = old->v;
    ret->origin = old->origin;
    return ret;
}

static lptr rev_lst(lptr list, lptr tail)
{
    while (list) {
	lptr p = CDR(list);
	CDR(list) = tail;
	tail = list;
	list = p;
    }
    return tail;
}
