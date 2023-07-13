/*
This file is part of ulex -- Uwe's lex
Copyright 2013, Uwe Hollerbach <uhollerbach@gmail.com>
License: 2clause BSD, see file 'LICENSE' for details

$Id: nfa.c,v 1.17 2017/03/05 20:12:53 uwe Exp $

Routines for dealing with NFA states and NFAs
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include "fsi_set.h"
#include "nfa.h"

/* Allocate & free NFA state structs */

static void wk_zero(struct nfa_state* ns)
{
    ns->type = TR_NONE;
    ns->tr_char = '\0';
    ns->tr_cc = ns->eps_closure = NULL;
    ns->next1 = ns->next2 = NULL;
    ns->id = ns->anchor = ns->mark = ns->tag = 0;
}

struct nfa_state* nfa_state_alloc(void)
{
    struct nfa_state* ret;

    ret = malloc(sizeof(struct nfa_state));
    if (ret == NULL) {
	fprintf(stderr, "NFA: memory allocation error\n");
	exit(99);
    }
    wk_zero(ret);
    return(ret);
}

void nfa_state_free(struct nfa_state* ns)
{
    if (ns != NULL) {
	fsi_set_free(ns->tr_cc);
	fsi_set_free(ns->eps_closure);
	wk_zero(ns);
	free(ns);
    }
}

/* Same as above, except we don't free internal structs */

void nfa_state_shallow_free(struct nfa_state* ns)
{
    if (ns != NULL) {
	wk_zero(ns);
	free(ns);
    }
}

/* Utility routine to clear the mark for all nodes of the NFA graph */

static void wk_clear_mark(struct nfa_state* ns)
{
    while (ns != NULL && ns->mark != 0) {
	ns->mark = 0;
	wk_clear_mark(ns->next2);
	ns = ns->next1;
    }
}

/* Free all nodes of the NFA graph */

static void wk_free(struct nfa_state* ns, struct nfa_state** fp)
{
    struct nfa_state* nn;

    while (ns != NULL && ns->mark != 1) {
	/* This state is valid and we haven't been here before... process it */
	ns->mark = 1;
	nn = ns->next1;
	wk_free(ns->next2, fp);
	ns->next1 = *fp;
	ns->next2 = NULL;
	*fp = ns;
	ns = nn;
    }
}

void nfa_free(struct nfa_state* ns)
{
    struct nfa_state *fp, *fn;

    fp = NULL;
    wk_clear_mark(ns);
    wk_free(ns, &fp);

    while (fp) {
	fn = fp->next1;
	nfa_state_free(fp);
	fp = fn;
    }
}

static void wk_foreach(struct nfa_state* ns,
		       void (*fn)(struct nfa_state*, void*),
		       void* data,
		       int traversal_order)
{
    if (traversal_order > 0) {
	/* Process each node before its children: this conveniently
	   can propagate information "downstream", ie, following
	   the next* pointers. */
	if (ns != NULL && ns->mark != 1) {
	    ns->mark = 1;
	    fn(ns, data);
	    wk_foreach(ns->next1, fn, data, 1);
	    wk_foreach(ns->next2, fn, data, 1);
	}
    } else if (traversal_order < 0) {
	/* Process each node after its children: this conveniently
	   can propagate information "upstream", ie, opposite the
	   direction of the next* pointers. */
	if (ns != NULL && ns->mark != 1) {
	    ns->mark = 1;
	    wk_foreach(ns->next1, fn, data, -1);
	    wk_foreach(ns->next2, fn, data, -1);
	    fn(ns, data);
	}
    } else {
	/* Tail-call-optimized interleaved processing: this does
	   not particularly propagate information. We choose here
	   to do the tail-recursion on the next1 pointer, since
	   every state except the end state has a non-NULL next1
	   pointer, whereas many states do not have a non-NULL
	   next2 pointer. */
	while (ns != NULL && ns->mark != 1) {
	    ns->mark = 1;
	    fn(ns, data);
	    wk_foreach(ns->next2, fn, data, 0);
	    ns = ns->next1;
	}
    }
}

/* Apply a user-specified function to each node of an NFA. The user function
   may not alter the structure of the graph, and neither may it call another
   instance of nfa_foreach: this does not nest. */

void nfa_foreach(struct nfa_state* ns,
		 void (*fn)(struct nfa_state*, void*),
		 void* data,
		 int traversal_order)
{
    wk_foreach(ns, fn, data, traversal_order);
    wk_clear_mark(ns);
}

/* Given a start state of an NFA, find the end state. This relies on
   details of the routines in regex.c; generally, both the next1 and
   next2 pointers could point backwards in a directed graph, and we
   could get into an infinite loop, but in the particular
   construction used in regex.c, that won't happen. */

/* TODO: fix the above assumption... should be straightforward, using
   the same kind of stuff as with the other traversing routines: first
   clear the mark, then traverse&detect. One small subtlety is what to
   do if multiple end states are found... */

struct nfa_state* nfa_find_end(struct nfa_state* ns)
{
    if (ns != NULL) {
	while (ns->next1 != NULL) {
	    ns = ns->next1;
	}
    }
    return(ns);
}

/* Utility routine to print out a representation of the NFA graph */

static void wk_print(struct nfa_state* ns, void* ignore)
{
    unsigned char c;

    printf("state %d: ", ns->id);
    switch (ns->anchor) {
    case NA_NONE:				break;
    case NA_BEGIN:	printf("(^) ");		break;
    case NA_END:	printf("($) ");		break;
    case NA_BOTH:	printf("(^$) ");	break;
    }
    switch (ns->type) {
    case TR_NONE:
	printf("end state");
	if (ns->tag) {
	    printf(" %u", ns->tag);
	}
	putchar('\n');
	break;
    case TR_CHAR:
	printf("char transition on ");
	if (isprint(ns->tr_char) && !isspace(ns->tr_char)) {
	    putchar(ns->tr_char);
	} else {
	    printf("0x%02x", ns->tr_char);
	}
	printf(" to %d\n", ns->next1->id);
	break;
    case TR_CC:
	printf("char-class transition to %d:", ns->next1->id);
	for (c = 0; c < ns->tr_cc->n_ints; ++c) {
	    if (fsi_set_ismember(ns->tr_cc, c)) {
		putchar(' ');
		if (isprint(c) && !isspace(c)) {
		    putchar(c);
		} else {
		    printf("0x%02x", c);
		}
	    }
	}
	putchar('\n');
	break;
    case TR_EPS1:
	printf("eps transition to %d\n", ns->next1->id);
	break;
    case TR_EPS2:
	printf("eps transitions to %d and %d\n", ns->next1->id, ns->next2->id);
	break;
    }
}

void nfa_print(struct nfa_state* ns)
{
    nfa_foreach(ns, wk_print, NULL, 0);
}

/* Label all nodes of the NFA graph */

static void wk_label(struct nfa_state* ns, void* vid)
{
    unsigned int* id = (unsigned int*) vid;
    ns->id = *id;
    ++(*id);
}

unsigned int nfa_label(struct nfa_state* ns)
{
    unsigned int id;

    id = 0;
    nfa_foreach(ns, wk_label, &id, 0);
    return(id);
}

/* Utility routine to initialize each state's epsilon-closure */

static void wk_add_ec(struct nfa_state* ns, void* vn)
{
    unsigned int n = *((int*) vn);

    fsi_set_free(ns->eps_closure);
    ns->eps_closure = fsi_set_alloc(n);
    fsi_set_insert(ns->eps_closure, ns->id);
}

/* Utility routine to propagate the epsilon-closures: "any friend of
   my friend is my friend"... except where begin-anchored regexes are
   concerned: we don't automatically propagate these */

static void wk_propagate_ec(struct nfa_state* ns, void* vd)
{
    unsigned int* d = (unsigned int*) vd;

    if (ns->type == TR_EPS1) {
	if (!(ns->next1->anchor & NA_BEGIN)) {
	    *d += fsi_set_or2(ns->eps_closure, ns->next1->eps_closure);
	}
    } else if (ns->type == TR_EPS2) {
	if (!(ns->next1->anchor & NA_BEGIN)) {
	    *d += fsi_set_or2(ns->eps_closure, ns->next1->eps_closure);
	}
	if (!(ns->next2->anchor & NA_BEGIN)) {
	    *d += fsi_set_or2(ns->eps_closure, ns->next2->eps_closure);
	}
    }
}

#if 0
/* Utility routine to print each state's epsilon-closure */

static void wk_print_eps(struct nfa_state* ns, void* ignore)
{
    printf("state %d:", ns->id);
    fsi_set_print_members(ns->eps_closure);
    putchar('\n');
}
#endif /* 0 */

/* Generate the epsilon-closure for all states in the NFA,
   return the number of states in the NFA */

unsigned int nfa_gen_epsilon_closure(struct nfa_state* ns)
{
    unsigned int i, j, n;

    n = nfa_label(ns);
    nfa_foreach(ns, wk_add_ec, &n, 0);

    i = 0;
    do {
	++i;
	j = 0;
	nfa_foreach(ns, wk_propagate_ec, &j, -1);
    } while (j != 0);
#if 0
    printf("epsilon-closures converged after %u iterations\n", i);
    nfa_foreach(ns, wk_print_eps, NULL, 0);
#endif /* 0 */
    return(n);
}

/* Add an NFA to the head of an existing list of NFAs, which may be NULL */

void chain_nfa(struct nfa_state** chain, struct nfa_state* link)
{
    struct nfa_state* n1;

    n1 = nfa_state_alloc();
    n1->next1 = link;
    n1->next2 = *chain;
    n1->type = (n1->next2 == NULL) ? TR_EPS1 : TR_EPS2;
    *chain = n1;
}
