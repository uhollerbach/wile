// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

/*
This is the regular-expression parser, which converts a regular expression
in the usual textual format into an NFA. The target is roughly something
like POSIX ERE or a bare-bones version of perl regular expressions, although
certainly not all of either of these; but we don't want to introduce (m)any
gratuitous incompatibilities. Roughly, we've got

	reg-exp	:=	alt-exp ( '|' alt-exp )*
	alt-exp	:=	cat-exp*
	cat-exp	:=	base ('?' | '+' | '*')?
	base	:=	ordinary character			|
			'[' character class ']'			|
			'[^' negated character class ']'	|
			'.'					|
			'\' escaped-char			|
			'(' reg-exp ')'

and now we've added quoted-strings, which are basically a concatenation
of ordinary characters.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include "wile.h"
#include "fsi_set.h"
#include "nfa.h"
#include "regex.h"
#include "alloc.h"

#define LPAREN		'('
#define RPAREN		')'

#define END		'\0'
#define ALTERNATE	'|'

#define CLOS_ZP		'*'
#define CLOS_ZO		'?'
#define CLOS_OP		'+'

#define CONCAT_END(n)	((n) == END || (n) == ALTERNATE || (n) == RPAREN)
#define CLOSURE(n)	((n) == CLOS_ZP || (n) == CLOS_OP || (n) == CLOS_ZO)

/* Keep track of all the state of the parse: where we are
   in the input, what the status is, any options, etc. */

struct p_state {
    const char* bp;		/* current location being parsed in regex */
    const char* end;		/* optional end pointer */
    unsigned int status;	/* return status, so that we can escape out
				   of deeply-nested calls without having to
				   pass out a status flag at every level */
    unsigned int options;	/* any options which may be set */
    struct regex_defs* rdefs;	/* key/value list of definitions, for {FOO} */
};

/* Basic "input" stuff which traverses the regex during parse:
   routine to peek at but not consume current character,
   routine to consume and return current character.
   These both look at the character to see if it's NUL, and
   also at pstate->end, in case we want to parse some sub-string
   of a larger string; currently used for $ anchoring, so that
   we can have true read-only inputs, and will be used in ulex
   in the future. */

static int examine(struct p_state* pstate)
{
    if (pstate->bp == pstate->end || *(pstate->bp) == END) {
	return(END);
    } else {
	return(*(pstate->bp));
    }
}

static int consume(struct p_state* pstate)
{
    int ret;

    if (pstate->bp == pstate->end || *(pstate->bp) == END) {
	return(END);
    } else {
	ret = *(pstate->bp);
	++(pstate->bp);
	return(ret);
    }
}

/* Remove \n and optionally \r from a character class */

static void remove_lf(struct fsi_set* set, unsigned int options)
{
    fsi_set_remove(set, '\n');
    if (options & REGEX_OPTION_CRLF_EQUIVALENT) {
	fsi_set_remove(set, '\r');
    }
}

/* Construct an elementary NFA to recognize a single character */

static struct nfa_state* n_char(char c)
{
    struct nfa_state *start, *end;

    start = nfa_state_alloc();
    end = nfa_state_alloc();

    start->type = TR_CHAR;
    start->tr_char = c;
    start->next1 = end;
    return(start);
}

/* Construct an elementary NFA to recognize a character class
   given by a function */

static struct nfa_state* n_cc_byfn(int (*fn)(int), int inverse,
				   unsigned int options)
{
    unsigned int c, n_chars;
    struct nfa_state *start, *end;

    start = nfa_state_alloc();
    end = nfa_state_alloc();

    n_chars = 1 + ((options & REGEX_OPTION_8BIT) ? UCHAR_MAX : CHAR_MAX);

    start->type = TR_CC;
    start->tr_cc = fsi_set_alloc(n_chars);

    if (inverse) {
	for (c = 0; c < n_chars; ++c) {
	    if (!fn(c)) {
		fsi_set_insert(start->tr_cc, c);
	    }
	}
    } else {
	for (c = 0; c < n_chars; ++c) {
	    if (fn(c)) {
		fsi_set_insert(start->tr_cc, c);
	    }
	}
    }
    remove_lf(start->tr_cc, options);

    start->next1 = end;
    return(start);
}

/* Construct an elementary NFA to recognize the 'any except newline'
   character class */

static struct nfa_state* n_cc_all(unsigned int options)
{
    unsigned int c, n_chars;
    struct nfa_state *start, *end;

    start = nfa_state_alloc();
    end = nfa_state_alloc();

    n_chars = 1 + ((options & REGEX_OPTION_8BIT) ? UCHAR_MAX : CHAR_MAX);

    start->type = TR_CC;
    start->tr_cc = fsi_set_alloc(n_chars);

    for (c = 0; c < n_chars; ++c) {
	fsi_set_insert(start->tr_cc, c);
    }
    remove_lf(start->tr_cc, options);

    start->next1 = end;
    return(start);
}

/* Utility function for '\o' == octal digit */

static int isodigit(int c)
{
    return(isdigit(c) && c != '8' && c != '9');
}

/* Utility function to return the numeric equivalent of a hex digit */

static int hexval(int c)
{
    if (c >= '0' && c <= '9') {
	return(c - '0');
    } else if (c >= 'a' && c <= 'f') {
	return(c + 0xa - 'a');
    } else if (c >= 'A' && c <= 'F') {
	return(c + 0xa - 'A');
    } else {
	/* This should be a never-happen case...
	   but just in case "never" happens */
	fprintf(stderr, "regex parse internal error, abort\n");
	exit(99);
    }
}

#define GEN_HEX_CHAR()							\
    do {								\
	cur = 0;							\
	next = examine(pstate);						\
	if (isxdigit(next)) {						\
	    cur = hexval(next);						\
	    consume(pstate);						\
	    next = examine(pstate);					\
	    if (isxdigit(next)) {					\
		cur <<= 4;						\
		cur += hexval(next);					\
		consume(pstate);					\
	    }								\
	}								\
    } while (0)

/* This macro gets used in character classes, gen_cclass(),
   and also in quoted strings, p_qstring() */
/* TODO: somehow combine this with analogous escaped-char stuff below? */

#define GEN_ESC_CHAR()							\
    do {								\
	cur = consume(pstate);						\
	if (cur == END) {						\
	    fprintf(stderr,						\
		    "regex parse error: uncompleted escaped-char\n");	\
	    pstate->status = REGEX_ERR;					\
	} else {							\
	    switch (cur) {						\
	    case 'a':	cur = '\a';	break;				\
	    case 'b':	cur = '\b';	break;				\
	    case 'f':	cur = '\f';	break;				\
	    case 'n':	cur = '\n';	break;				\
	    case 'r':	cur = '\r';	break;				\
	    case 't':	cur = '\t';	break;				\
	    case 'v':	cur = '\v';	break;				\
	    case 'x':	GEN_HEX_CHAR();	break;				\
	    case 'c':							\
	    case 'd':							\
	    case 'h':							\
	    case 'o':							\
	    case 'p':							\
	    case 's':							\
	    case 'w':							\
	    case 'C':							\
	    case 'D':							\
	    case 'H':							\
	    case 'O':							\
	    case 'P':							\
	    case 'S':							\
	    case 'W':							\
		fprintf(stderr,						\
			"regex parse warning: '\\%c' is not an"		\
			" escape character inside char-classes\n", cur);\
	    default:	/* relax */	break;				\
	    }								\
	}								\
    } while (0)

/* Generate a character class */

static void gen_cclass(struct p_state* pstate, struct fsi_set* cc)
{
    int invert, first, range, cur, prev, next;

    if (examine(pstate) == '^') {
	consume(pstate);
	invert = 1;
    } else {
	invert = 0;
    }
    first = 1;
    range = 0;
    prev = -1;
    while (1) {
	cur = consume(pstate);
	if (cur == END) {
	    fprintf(stderr, "regex parse error: uncompleted char-class\n");
	    pstate->status = REGEX_ERR;
	    return;
	}
	if (cur == ']' && first == 0) {
	    break;
	} else if (cur == '-') {
	    if (first == 0) {
		range = 1;
		cur = consume(pstate);
		if (cur == END) {
		    fprintf(stderr,
			    "regex parse error: uncompleted char-class\n");
		    pstate->status = REGEX_ERR;
		    return;
		} else if (cur == ']') {
		    fsi_set_insert(cc, '-');
		    break;
		} else if (cur == '\\') {
		    GEN_ESC_CHAR();
		    if (pstate->status == REGEX_ERR) {
			return;
		    }
		}
	    }
	} else if (cur == '\\') {
	    GEN_ESC_CHAR();
	    if (pstate->status == REGEX_ERR) {
		return;
	    }
	}
	if (range == 1) {
	    if (prev == -1) {
		fprintf(stderr,
			"regex parse error: incomplete character range\n");
		pstate->status = REGEX_ERR;
		return;
	    }
	    if (prev <= cur) {
		while (++prev <= cur) {
		    fsi_set_insert(cc, prev);
		}
	    } else {
		while (--prev >= cur) {
		    fsi_set_insert(cc, prev);
		}
	    }
	    prev = -1;
	    range = 0;
	} else {
	    fsi_set_insert(cc, cur);
	    prev = cur;
	}
	first = 0;
    }
    if (invert) {
	fsi_set_not(cc);
#if 0
	/* TODO: is this correct? */
	/* Even for inverted character classes, we remove \n and perhaps \r */
	remove_lf(cc, pstate->options);
#endif
    }
}

static struct nfa_state* p_regex(struct p_state* pstate);

/* This is an auxiliary "regex" parser, just a simple quoted string */

static struct nfa_state* p_qstring(struct p_state* pstate)
{
    int cur, next;
    struct nfa_state *nfa, *end;

    consume(pstate);
    nfa = nfa_state_alloc();
    end = nfa;
    while (1) {
	cur = consume(pstate);
	if (cur == END) {
	    fprintf(stderr,
		    "regex parse error: uncompleted quoted-string\n");
	    pstate->status = REGEX_ERR;
	    return(NULL);
	} else if (cur == '"') {
	    break;
	} else if (cur == '\\') {
	    GEN_ESC_CHAR();
	    if (pstate->status == REGEX_ERR) {
		nfa_state_free(nfa);
		return(NULL);
	    }
	}
	end->type = TR_CHAR;
	end->tr_char = cur;
	end->next1 = nfa_state_alloc();
	end = end->next1;
    }
    return(nfa);
}

/* Another auxiliary parser, for {FOO} macros */

static struct nfa_state* p_macro(struct p_state* pstate)
{
    const char* np;
    int cur;
    unsigned int nlen;
    struct regex_defs* rdefs;
    struct nfa_state *nfa;
    struct p_state qstate;

    consume(pstate);
    nlen = 0;
    np = pstate->bp;
    while (1) {
	cur = consume(pstate);
	if (cur == END) {
	    fprintf(stderr, "regex parse error: uncompleted macro call\n");
	    pstate->status = REGEX_ERR;
	    return(NULL);
	} else if (cur == '}') {
	    break;
	}
	++nlen;
    }
    rdefs = pstate->rdefs;
    while (rdefs) {
	if (strncmp(np, rdefs->name, nlen) == 0 &&
	    rdefs->name[nlen] == '\0') {
	    qstate.bp = rdefs->regex;
	    qstate.end = NULL;
	    qstate.options = pstate->options;
	    qstate.status = REGEX_OK;

	    /* This requires that any macro used within a macro definition
	       shall be previously defined (farther down the linked list),
	       so it eliminates the infinite loop

		   FOO	isa{FOO}isFUBAR

	       Since there is no provision in these macros for a base case
	       of a recursion, there is no reason ever to allow looping. */
	    qstate.rdefs = rdefs->next;

	    nfa = p_regex(&qstate);
	    pstate->status = qstate.status;
	    return(nfa);
	}
	rdefs = rdefs->next;
    }
    fputs("regex parse error: undefined macro {", stderr);
    while (nlen-- > 0) {
	fputc(*np++, stderr);
    }
    fputs("}\n", stderr);
    exit(99);
}

/* The four basic mutually-recursive routines which together
   form the recursive-descent parser for regexes:
   p_regex -> p_concat -> p_closure -> p_base (-> p_regex)
   In addition to these, but a bit "off to one side" are
   p_qstring, for quoted-strings, and p_macro, for macros. */

static struct nfa_state* p_base(struct p_state* pstate)
{
    int next;
    unsigned char cur;
    struct nfa_state* nfa;

    next = examine(pstate);
    if (CONCAT_END(next)) {
	return(nfa_state_alloc());
    }
    if (CLOSURE(next)) {
	fprintf(stderr, "regex parse error: unexpected '%c'\n", next);
	pstate->status = REGEX_ERR;
	return(NULL);
    }

    if (next == LPAREN) {		/* begin grouped regex */
	consume(pstate);
	nfa = p_regex(pstate);
	if (nfa == NULL || pstate->status == REGEX_ERR) {
	    pstate->status = REGEX_ERR;
	    return(NULL);
	}
	if (examine(pstate) == RPAREN) {
	    consume(pstate);
	} else {
	    fprintf(stderr, "regex parse error: unmatched parentheses\n");
	    pstate->status = REGEX_ERR;
	    return(NULL);
	}
    } else if (next == '\\') {		/* escaped char */
	consume(pstate);
	next = consume(pstate);
	switch (next) {
	case 'a':	nfa = n_char('\a');	break;
	case 'b':	nfa = n_char('\b');	break;
	case 'f':	nfa = n_char('\f');	break;
	case 'n':	nfa = n_char('\n');	break;
	case 'r':	nfa = n_char('\r');	break;
	case 't':	nfa = n_char('\t');	break;
	case 'v':	nfa = n_char('\v');	break;

	    /* The following are extensions: from here... */
	case 'c':	nfa = n_cc_byfn(iscntrl, 0, pstate->options);	break;
	case 'd':	nfa = n_cc_byfn(isdigit, 0, pstate->options);	break;
	case 'h':	nfa = n_cc_byfn(isxdigit, 0, pstate->options);	break;
	case 'o':	nfa = n_cc_byfn(isodigit, 0, pstate->options);	break;
	case 'p':	nfa = n_cc_byfn(ispunct, 0, pstate->options);	break;
	case 's':	nfa = n_cc_byfn(isspace, 0, pstate->options);	break;
	case 'w':	nfa = n_cc_byfn(isalnum, 0, pstate->options);	break;

	case 'C':	nfa = n_cc_byfn(iscntrl, 1, pstate->options);	break;
	case 'D':	nfa = n_cc_byfn(isdigit, 1, pstate->options);	break;
	case 'H':	nfa = n_cc_byfn(isxdigit, 1, pstate->options);	break;
	case 'O':	nfa = n_cc_byfn(isodigit, 1, pstate->options);	break;
	case 'P':	nfa = n_cc_byfn(ispunct, 1, pstate->options);	break;
	case 'S':	nfa = n_cc_byfn(isspace, 1, pstate->options);	break;
	case 'W':	nfa = n_cc_byfn(isalnum, 1, pstate->options);	break;

	case 'x':	GEN_HEX_CHAR();    nfa = n_char(cur);		break;
	    /* ... down to here */

	default:	nfa = n_char(next);	break;
	}
    } else if (next == '.') {		/* "any except newline" char class */
	consume(pstate);
    	nfa = n_cc_all(pstate->options);
    } else if (next == '[') {		/* char class... */
	consume(pstate);
	nfa = n_char('\0');
	nfa->type = TR_CC;
	nfa->tr_cc = fsi_set_alloc(1 + ((pstate->options & REGEX_OPTION_8BIT)
					? UCHAR_MAX : CHAR_MAX));
	gen_cclass(pstate, nfa->tr_cc);
	if (pstate->status == REGEX_ERR) {
	    nfa_state_free(nfa);
	    return(NULL);
	}
    } else if (next == '"') {		/* quoted string */
	nfa = p_qstring(pstate);
    } else if (next == '{') {		/* macro {FOO} */
	nfa = p_macro(pstate);
    } else {
	consume(pstate);
	nfa = n_char(next);
    }
    return(nfa);
}

static struct nfa_state* p_closure(struct p_state* pstate)
{
    int next;
    struct nfa_state *nfa, *end, *cs, *ce;

    nfa = p_base(pstate);
    if (nfa == NULL || pstate->status == REGEX_ERR) {
	nfa_state_free(nfa);
	pstate->status = REGEX_ERR;
	return(NULL);
    }
    next = examine(pstate);
    if (CLOSURE(next)) {
	consume(pstate);
	end = nfa_find_end(nfa);

	cs = nfa_state_alloc();
	ce = nfa_state_alloc();
	cs->next1 = nfa;
	cs->type = TR_EPS1;
	end->next1 = ce;
	end->type = TR_EPS1;

	if (next != CLOS_OP) {
	    cs->next2 = ce;
	    cs->type = TR_EPS2;
	}
	if (next != CLOS_ZO) {
	    end->next2 = nfa;
	    end->type = TR_EPS2;
	}
	nfa = cs;
    }
    return(nfa);
}

static struct nfa_state* p_concat(struct p_state* pstate)
{
    int next;
    struct nfa_state *nfa, *aux, *end;

    nfa = end = NULL;
    while (1) {
	aux = p_closure(pstate);
	if (aux == NULL || pstate->status == REGEX_ERR) {
	    pstate->status = REGEX_ERR;
	    return(NULL);
	}
	if (nfa == NULL) {
	    nfa = aux;
	    end = nfa_find_end(aux);
	} else {
	    /* Optimization: we rely here on the fact that there is no
	       internal state in any NFA that points back at the start
	       state: in closures, the back-pointers point back at the
	       start state of the internal NFA which is "wrapped" into
	       an enclosing start/end pair, so this remains true.

	       This being the case, we can merge the old end and aux
	       states, unhook the aux state from the rest of its NFA,
	       and free just that aux state. Without that, we would
	       just add in an epsilon-transition from the old end
	       state to the aux state and keep both in the new
	       combined NFA.

	       Note that the free of the aux has to be a shallow free,
	       since we've just transferred any char-class array to
	       the old end state. */
	    *end = *aux;
	    end = nfa_find_end(aux);
	    nfa_state_shallow_free(aux);
	}
	next = examine(pstate);
	if (CONCAT_END(next)) {
	    break;
	}
    }
    return(nfa);
}

static struct nfa_state* p_regex(struct p_state* pstate)
{
    int next;
    struct nfa_state *nfa, *aux, *end, *os, *oe;

    nfa = NULL;
    while (1) {
	aux = p_concat(pstate);
	if (aux == NULL || pstate->status == REGEX_ERR) {
	    pstate->status = REGEX_ERR;
	    return(NULL);
	}
	if (nfa == NULL) {
	    nfa = aux;
	} else {
	    os = nfa_state_alloc();
	    os->next1 = aux;
	    os->next2 = nfa;
	    os->type = TR_EPS2;

	    oe = nfa_state_alloc();
	    end = nfa_find_end(nfa);
	    end->next1 = oe;
	    end->type = TR_EPS1;

	    end = nfa_find_end(aux);
	    end->next1 = oe;
	    end->type = TR_EPS1;

	    nfa = os;
	}
	next = examine(pstate);
	if (next == ALTERNATE) {
	    consume(pstate);
	} else if (next == END || next == RPAREN) {
	    break;
	} else {
	    nfa_state_free(nfa);
	    fprintf(stderr, "regex parse error: unexpected '%c'\n", next);
	    pstate->status = REGEX_ERR;
	    return(NULL);
	}
    }
    return(nfa);
}

/* Publicly-visible driver for the recursive-descent parser */

struct nfa_state* regex_parse(const char* regex, struct regex_defs* rdefs,
			      unsigned int options, unsigned int end_tag)
{
    int next, anchor;
    struct nfa_state* nfa;
    struct p_state pstate;

    if (regex == NULL) {
	fprintf(stderr, "regex parse error: no regex specified!\n");
	return(NULL);
    }

    /* TODO: more-standard regex engines can handle anchors inside the
       text of the regex, as long as they are logically at the beginning,
       eg, 'foo|^bar'. Implement this? For now, this is enough though. */
    pstate.bp = regex;
    pstate.end = NULL;
    anchor = NA_NONE;
    next = examine(&pstate);
    if (next == '^') {
	anchor |= NA_BEGIN;
	consume(&pstate);
    }
    next = strlen(pstate.bp);
    if (next > 0 && pstate.bp[next-1] == '$') {
	anchor |= NA_END;
	pstate.end = pstate.bp + next - 1;
    }
    pstate.status = REGEX_OK;
    pstate.options = options;
    pstate.rdefs = rdefs;
    nfa = p_regex(&pstate);
    if (nfa == NULL || pstate.status != REGEX_OK) {
	fprintf(stderr, "regex parse error\n");
	return(NULL);
    }
    next = examine(&pstate);
    if (next != END) {
	nfa_state_free(nfa);
	fprintf(stderr, "regex parse error: failed to reach end of regex!\n");
	return(NULL);
    }
    if (anchor & NA_BEGIN) {
	nfa->anchor |= NA_BEGIN;
    }
    if (anchor & NA_END) {
	nfa_find_end(nfa)->anchor |= NA_END;
    }
    nfa_find_end(nfa)->tag = end_tag;

    return(nfa);
}

static void cp_ptr(struct nfa_state* nfa, void* dp)
{
    ((struct nfa_state**) dp)[nfa->id] = nfa;
}

/* A wrapper that turns a pure NFA into an nfa_work which gets passed
   to regex_match() below or processed in some other fashion. */

struct nfa_work* regex_wrap(struct nfa_state* nfa, unsigned int options)
{
    struct nfa_work* wrapper;

    if (nfa == NULL) {
	fprintf(stderr, "regex parse error: no NFA to wrap!\n");
	exit(99);
    }

    wrapper = LISP_ALLOC(struct nfa_work, 1);
    wrapper->nfa = nfa;
    wrapper->n_states = nfa_gen_epsilon_closure(nfa);
    wrapper->states = LISP_ALLOC(struct nfa_state*, wrapper->n_states);
    wrapper->n_chars =
	1 + ((options & REGEX_OPTION_8BIT) ? UCHAR_MAX : CHAR_MAX);

    nfa_foreach(nfa, cp_ptr, wrapper->states, 0);
    wrapper->st_old = fsi_set_alloc(wrapper->n_states);
    wrapper->st_new = fsi_set_alloc(wrapper->n_states);

    return(wrapper);
}

/* Match routine: accept text, an NFA processed by regex_parse & regex_wrap
   above, and return whether or not some portion of the text matches.
   Return true if there is a match, false if no match. Additionally,
   if the pointers-to-pointers ms and me are non-NULL, they will get
   set to the beginning and end of the matching area. */

int regex_match(unsigned char* text, struct nfa_work* wk,
		unsigned char** ms, unsigned char** me)
{
    unsigned int i, got_trans, is_begin, do_begin;
    unsigned char c, *tp, *last_accept;

    last_accept = NULL;

    /* check if "begin" processing will be needed at all */

    do_begin = 0;
    for (i = 0; i < wk->n_states; ++i) {
	if (wk->states[i]->anchor & NA_BEGIN) {
	    do_begin = 1;
	    break;
	}
    }

    /* turn on "begin" context for the first trip through the loop */
    is_begin = 1;

    /* for each position in the input text, check for a match starting
       at that position */
    while (*text != '\0') {
	tp = text;
	last_accept = NULL;
	/* we're no longer at the beginning, and this is an anchored NFA;
	   thus no need to work hard, as we've already failed */
	if ((wk->nfa->anchor & NA_BEGIN) && !is_begin) {
	    break;
	}
	fsi_set_xfer(wk->st_old, wk->nfa->eps_closure);
	while (1) {
	    c = *tp++;
	    if (c == '\0') {
		break;
	    }

	    /* if we have begin-anchored regexes, we don't automatically
	       accept the eps-transition to the begin-anchored state; it must
	       be in the right context. we check for that here, and pull in
	       the epsilon-closures only if we are in an appropriate position
	       in the text */

	    if (do_begin && is_begin) {
		fsi_set_xfer(wk->st_new, wk->st_old);
		for (i = 0; i < wk->n_states; ++i) {
		    if (fsi_set_ismember(wk->st_old, i)) {
			if ((wk->states[i]->type == TR_EPS1 ||
			     wk->states[i]->type == TR_EPS2) &&
			    wk->states[i]->next1->anchor & NA_BEGIN) {
			    fsi_set_or(wk->st_new,
				       wk->states[i]->next1->eps_closure);
			}
			if (wk->states[i]->type == TR_EPS2 &&
			    wk->states[i]->next2->anchor & NA_BEGIN) {
			    fsi_set_or(wk->st_new,
				       wk->states[i]->next2->eps_closure);
			}
		    }
		}
		fsi_set_xfer(wk->st_old, wk->st_new);
	    }

	    fsi_set_clear(wk->st_new);
	    got_trans = 0;
	    for (i = 0; i < wk->n_states; ++i) {
		if (fsi_set_ismember(wk->st_old, i) &&
		    ((wk->states[i]->type == TR_CHAR &&
		      wk->states[i]->tr_char == c) ||
		     (wk->states[i]->type == TR_CC &&
		      fsi_set_ismember(wk->states[i]->tr_cc, c)))) {
		    fsi_set_or(wk->st_new, wk->states[i]->next1->eps_closure);
		    got_trans = 1;
		}
	    }
	    if (got_trans == 0) {
		break;
	    }
	    fsi_set_xfer(wk->st_old, wk->st_new);
	    for (i = 0; i < wk->n_states; ++i) {
		if (fsi_set_ismember(wk->st_old, i) &&
		    wk->states[i]->type == TR_NONE) {
		    if (!(wk->states[i]->anchor & NA_END) ||
			*tp == '\n' || *tp == '\0') {
			last_accept = tp - 1;
		    }
		}
	    }
	}
	if (last_accept) {
	    break;
	}
	is_begin = (*text == '\n') ? 1 : 0;
	++text;
    }

    if (last_accept) {
	if (ms != NULL && me != NULL) {
	    *ms = text;
	    *me = last_accept;
	}
	return(1);
    } else {
	if (ms != NULL && me != NULL) {
	    *ms = *me = NULL;
	}
	return(0);
    }
}

struct regex_defs* regex_defs_alloc()
{
    struct regex_defs* ret;

    ret = LISP_ALLOC(struct regex_defs, 1);
    ret->name = ret->regex = NULL;
    ret->next = NULL;
    return(ret);
}

void regex_defs_free(struct regex_defs* rd)
{
    if (rd) {
	LISP_FREE(rd->name);
	LISP_FREE(rd->regex);
	rd->name = rd->regex = NULL;
	rd->next = NULL;
	LISP_FREE(rd);
    }
}

void regex_free(struct nfa_work* wk)
{
    if (wk) {
	LISP_FREE(wk->states);
	fsi_set_free(wk->st_old);
	fsi_set_free(wk->st_new);
	LISP_FREE(wk);
    }
}
