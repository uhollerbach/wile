// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "wile.h"
#include "alloc.h"
#define ULEX_PRIVATE_DEFS
#include "ulexlib.h"

#define BUFFER_SIZE		65536
#define DANGER_ZONE		16

/* Match routine: accept a pointer to a context struct, a pointer to
   user data, and an optional place to store the matched text if any,
   and return whether or not some portion of the text matches. */

unsigned int ulex_generic(const struct ulex_scanner_spec* scan_spec,
			  struct ulex_context* scan_ctx,
			  void* user_data,
			  unsigned char** match_ret,
			  void* yucc_lexval)
{
    unsigned int state, next_s, last_tag, act, user_ret;
    unsigned char c, *tp, *last_acc, *match_txt;
    size_t otext, otp, ola, match_len;
    const struct ulex_dfa_state* ss;
    const unsigned char* cc;

    if (scan_ctx->save_p) {
	*(scan_ctx->save_p) = scan_ctx->save_c;
	scan_ctx->save_p = NULL;
    }
    if (scan_ctx->start_condition >= scan_spec->n_start_conditions) {
	fprintf(stderr,
		"ulex internal error, start condition out of bounds!\n");
	exit(1);
    }
    if (scan_ctx->is_begin) {
	ss = scan_spec->a_states[scan_ctx->start_condition];
	cc = scan_spec->a_cc[scan_ctx->start_condition];
	if (ss == NULL || cc == NULL) {
	    ss = scan_spec->u_states[scan_ctx->start_condition];
	    cc = scan_spec->u_cc[scan_ctx->start_condition];
	}
    } else {
	ss = scan_spec->u_states[scan_ctx->start_condition];
	cc = scan_spec->u_cc[scan_ctx->start_condition];
    }
    if (scan_ctx->text && *(scan_ctx->text) == '\0') {
	scan_ctx->text = NULL;
    }
    ola = 0;	/* silence a compiler warning */

    /* for each position in the input text, check for a match starting
       at that position */
    while (scan_ctx->text) {
	tp = scan_ctx->text;
	last_acc = NULL;
	last_tag = 0;
	state = 0;
	while (1) {
	    if (!scan_ctx->got_eof &&
		(tp - scan_ctx->buf) > (scan_ctx->buf_data - DANGER_ZONE)) {
		otext = scan_ctx->text - scan_ctx->buf;
		otp = tp - scan_ctx->text;
		if (last_acc) {
		    ola = last_acc - scan_ctx->text;
		}
		scan_ctx->buf_data -= otext;
		if (2*scan_ctx->buf_data > scan_ctx->buf_size) {
		    scan_ctx->buf_size *= 2;
		    match_txt = LISP_ALLOC(unsigned char, scan_ctx->buf_size);
		    memmove(match_txt, scan_ctx->text, scan_ctx->buf_data);
		    LISP_FREE(scan_ctx->buf);
		    scan_ctx->buf = match_txt;
		} else {
		    memmove(scan_ctx->buf, scan_ctx->text, scan_ctx->buf_data);
		}		    
		scan_ctx->text = scan_ctx->buf;
		tp = scan_ctx->text + otp;
		if (last_acc) {
		    last_acc = scan_ctx->text + ola;
		}

		ola = scan_ctx->buf_size - scan_ctx->buf_data;
		otp = fread(scan_ctx->buf + scan_ctx->buf_data, 1,
				ola, scan_ctx->input_src);
		scan_ctx->buf_data += otp;
		if (otp < ola) {
		    scan_ctx->got_eof = feof(scan_ctx->input_src);
		    scan_ctx->buf[scan_ctx->buf_data] = '\0';
		}
	    }
	    c = *tp++;
	    if (c == '\0') {
		break;
	    }

	    c = cc[c];
	    next_s = ss[state].trans[c];
	    if (next_s == 0) {
		break;
	    }
	    --next_s;
	    if (ss[next_s].tag1 > 0) {
		if (!(ss[next_s].anchor & DA_END) ||
		    *tp == '\n' || *tp == '\0') {
		    last_acc = tp - 1;
		    last_tag = ss[next_s].tag1;
		} else if (ss[next_s].tag2 > 0) {
		    last_acc = tp - 1;
		    last_tag = ss[next_s].tag2;
		}
	    }
	    state = next_s;	    
	}
	if (last_acc) {
	    tp = match_txt = scan_ctx->text;
	    match_len = last_acc + 1 - match_txt;
	    if (match_ret != NULL) {
		*match_ret = match_txt;
	    }
	    scan_ctx->save_p = scan_ctx->text = last_acc + 1;
	    scan_ctx->is_begin = (*last_acc == '\n') ? 1 : 0;
	    while (tp != scan_ctx->save_p) {
		if (*tp++ == '\n') {
		    ++scan_ctx->line_no;
		}
	    }
	    scan_ctx->save_c = *(scan_ctx->save_p);
	    *(scan_ctx->save_p) = '\0';
	    scan_ctx->partial = match_len;
	    act = scan_spec->actions(&user_ret, last_tag, scan_ctx,
				     match_len, match_txt, user_data,
				     yucc_lexval);

	    if (scan_ctx->partial < match_len) {
/* TODO: this potentially destroys the line-number count; fix! */
		scan_ctx->text -= match_len - scan_ctx->partial;

		*(scan_ctx->save_p) = scan_ctx->save_c;
		scan_ctx->save_p = scan_ctx->text;

		scan_ctx->save_c = *(scan_ctx->save_p);
		*(scan_ctx->save_p) = '\0';
	    }

	    if (user_ret) {
		return act;
	    }
	    *(scan_ctx->save_p) = scan_ctx->save_c;
	    scan_ctx->save_p = NULL;
	    if (scan_ctx->start_condition >= scan_spec->n_start_conditions) {
		fprintf(stderr, "ulex internal error, start condition out of bounds!\n");
		exit(1);
	    }
	    if (scan_ctx->is_begin) {
		ss = scan_spec->a_states[scan_ctx->start_condition];
		cc = scan_spec->a_cc[scan_ctx->start_condition];
		if (ss == NULL || cc == NULL) {
		    ss = scan_spec->u_states[scan_ctx->start_condition];
		    cc = scan_spec->u_cc[scan_ctx->start_condition];
		}
	    } else {
		ss = scan_spec->u_states[scan_ctx->start_condition];
		cc = scan_spec->u_cc[scan_ctx->start_condition];
	    }
	} else {
	    fprintf(stderr, "ulex error: no rule matched, and scanner has no default rule!\n");
	    for (c = 0; c < 20; ++c) {
		if (scan_ctx->text[c] == '\0') {
		    break;
		}
	    }
	    scan_ctx->text[c] = '\0';
	    fprintf(stderr, "at line %zu ", scan_ctx->line_no);
	    if (c < 20) {
		fprintf(stderr, "remaining text is: %s\n", scan_ctx->text);
	    } else {
		fprintf(stderr,
			"first few chars of remaining text are: %s...\n",
			scan_ctx->text);
	    }
	    exit(1);
	}
	if (*(scan_ctx->text) == '\0') {
	    scan_ctx->text = NULL;
	}
    }
    return 0;
}

/* Lexer setup routine: we're not using static variables,
   so we need a context struct to store all this stuff. */

struct ulex_context* ulex_init(enum ulex_input_source stype, void* sptr)
{
    struct ulex_context* context;

    context = LISP_ALLOC(struct ulex_context, 1);

    /* turn on "begin" context initially */
    context->is_begin = 1;

    /* no saved character initially */
    context->save_p = NULL;
    context->save_c = '\0';

    /* at line 1 initially... this is for humans, we start counting from 1 */
    context->line_no = 1;

    context->input_src = NULL;
    context->own_input = false;
    switch (stype) {
    case ulex_TEXT:
	context->text = (unsigned char*) sptr;
	/* we don't own the input, so we won't free it later */
	context->buf = NULL;
	context->buf_size = 0;
	context->got_eof = 1;
	break;
    case ulex_FILE:
	context->input_src = fopen((char*) sptr, "r");
	context->own_input = true;
	if (context->input_src == NULL) {
	    fprintf(stderr, "error: unable to open file %s\n", (char*) sptr);
	    LISP_FREE(context);
	    return NULL;
	}
	/* fall-through desired */
    case ulex_STREAM:
	if (context->input_src == NULL) {
	    context->input_src = sptr ? (FILE*) sptr : stdin;
	}
	context->buf = LISP_ALLOC(unsigned char, BUFFER_SIZE);
	context->buf_size = BUFFER_SIZE;
	context->buf_data =
	    fread(context->buf, 1, context->buf_size, context->input_src);
	context->text = context->buf;
	context->got_eof = 0;
	break;
    }

    /* set initial start condition */
    context->start_condition = 0;

    return context;
}

/* Lexer cleanup routine */

void ulex_cleanup(struct ulex_context* context)
{
    if (context) {
	LISP_FREE(context->buf);
	if (context->own_input &&
	    context->input_src &&
	    context->input_src != stdin) {
	    fclose(context->input_src);
	}
	LISP_FREE(context);
    }
}

size_t ulex_lineno(struct ulex_context* context)
{
    return (context ? context->line_no : 0);
}
