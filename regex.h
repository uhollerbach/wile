#ifndef REGEX_H
#define REGEX_H

/*
This file is part of ulex -- Uwe's lex
Copyright 2013, Uwe Hollerbach <uhollerbach@gmail.com>
License: 2clause BSD, see file 'LICENSE' for details

$Id: regex.h,v 1.20 2016/12/17 07:24:19 uwe Exp $
*/

#define REGEX_OK	0
#define REGEX_ERR	1

/* These are flags, so they have to be powers of 2 */

enum regex_options
{
    REGEX_OPTION_8BIT = 1 << 0,
    REGEX_OPTION_CRLF_EQUIVALENT = 1 << 1
};

/* Wrapper around an NFA graph to organize & collect needed info for
   regex matching and for conversion to DFA. */

struct nfa_work {
    struct nfa_state* nfa;		/* the actual NFA graph */

    unsigned int n_states;		/* number of states in the graph */
    struct nfa_state** states;		/* alternate way to access states,
					   by id rather than by pointer */
    unsigned int n_chars;		/* number of characters in the input
					   character set */

    struct fsi_set* st_old;		/* container for the "from" group */
    struct fsi_set* st_new;		/* container for the "to" group */
};

/* A key/value list of names & regexes, for {FOO} substitution */

struct regex_defs {
    char* name;			/* the name by which we look this up */
    char* regex;		/* the stuff we substitute for {NAME} */

    struct regex_defs* next;	/* the next one in the list */
};

struct regex_defs* regex_defs_alloc(void);
void regex_defs_free(struct regex_defs* rd);

/* The regular-expression parser: returns a pure NFA */

struct nfa_state* regex_parse(const char* regex, struct regex_defs* rdefs,
			      unsigned int options, unsigned int end_tag);

/* A wrapper that turns a pure NFA into an nfa_work which gets passed
   to regex_match() below or processed in some other fashion. */

struct nfa_work* regex_wrap(struct nfa_state* nfa, unsigned int options);

/* Match routine: accept text, an NFA parsed by regex_parse() above,
   and return whether or not some portion of the text matches. Return
   true if there is a match, false if no match. Additionally, if the
   pointers-to-pointers ms and me are non-NULL, they will get set to
   the beginning and end of the matching area. */

int regex_match(unsigned char* text, struct nfa_work* wk,
		unsigned char** ms, unsigned char** me);

void regex_free(struct nfa_work* wk);

#endif /* REGEX_H */
