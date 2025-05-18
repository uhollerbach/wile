#ifndef ULEXLIB_H
#define ULEXLIB_H

// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#ifdef __cplusplus
extern "C" {
#define ULEX_PRIVATE_DEFS
#endif

#include <stdlib.h>
#include <stdbool.h>

enum ulex_input_source {
    ulex_FILE = 0,			/* a file given by name */
    ulex_STREAM,			/* a file given as a FILE* */
    ulex_TEXT				/* a NUL-terminated string */
};

struct ulex_context;

struct ulex_context* ulex_init(enum ulex_input_source, void*);
void ulex_cleanup(struct ulex_context*);
size_t ulex_lineno(struct ulex_context*);

/*
   Generated foo_lex routine will have prototype

	unsigned int foo_lex(struct ulex_context* scan_ctx,
			     void* user_data,
			     unsigned char** match_ret);

   for standalone version and

	unsigned int foo_lex(struct ulex_context* scan_ctx,
			     struct YYSTYPE* yucc_val,
			     void* user_data,
			     unsigned char** match_ret);

   for yucc-aware version.
*/

/* **************************************************************** */

#ifdef ULEX_PRIVATE_DEFS
#include <stdio.h>

struct ulex_context {
    FILE* input_src;			/* input source: NULL if text string */
    bool own_input;			/* do we own the input? for cleanup */
    unsigned char* buf;			/* buffer for file contents */
    long int buf_size;			/* allocated buffer size */
    long int buf_data;			/* amount stored in buffer */
    unsigned int got_eof;		/* did we see an EOF already? */
    unsigned int is_begin;		/* flag for "begin" processing */
    unsigned char* text;		/* pointer to text-in-progress */
    size_t line_no;			/* internal line counter, should be
					   more reliable than doing it in
					   the ulex actions each time */

    unsigned char save_c, *save_p;	/* for making yytext conveniently
					   available within actions: we
					   want it to be NUL-terminated */

    unsigned int start_condition;	/* current start condition,
					   if we are using them */

    size_t partial;			/* number of characters accepted by
					   action: yyless resets this... if
					   yyless()-specified value is outside
					   the range [0 .. yyleng-1], it gets
					   clipped to the appropriate value */
};

enum ulex_dfa_anchor {
    DA_NONE = 0,
    DA_BEGIN = (1 << 0),
    DA_END = (1 << 1),
    DA_BOTH = (DA_BEGIN | DA_END)
};

struct ulex_dfa_state
{
    unsigned int tag1;
    unsigned int tag2;
    enum ulex_dfa_anchor anchor;
    const unsigned int* trans;
};

struct ulex_scanner_spec {
    unsigned int n_start_conditions;

    const unsigned char** u_cc;
    const struct ulex_dfa_state* const * u_states;

    const unsigned char** a_cc;
    const struct ulex_dfa_state* const * a_states;

    unsigned int (*actions)(unsigned int*, unsigned int,
			    struct ulex_context*, size_t,
			    unsigned char*, void*, void*);
};

unsigned int ulex_generic(const struct ulex_scanner_spec* scan_spec,
			  struct ulex_context* scan_ctx,
			  void* user_data,
			  unsigned char** match_ret,
			  void* yucc_lexval);

#endif /* ULEX_PRIVATE_DEFS */

#ifdef __cplusplus
}
#undef ULEX_PRIVATE_DEFS
#endif

#endif /* ULEXLIB_H */
