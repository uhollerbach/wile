#ifndef NFA_H
#define NFA_H

// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

enum nfa_state_type
{
    TR_NONE,		/* no transitions out of this state */
    TR_CHAR,		/* real char transition, in next1 */
    TR_CC,		/* real char-class transition, in next1 */
    TR_EPS1,		/* one epsilon-transition, in next1 */
    TR_EPS2		/* two epsilon-transitions, in next1 and next2 */
};

enum nfa_anchor_type
{
    NA_NONE = 0,
    NA_BEGIN = (1 << 0),
    NA_END = (1 << 1),
    NA_BOTH = (NA_BEGIN | NA_END)
};

struct nfa_state
{
    unsigned int id;		/* the state's label */
    enum nfa_state_type type;	/* the state's type: outgoing transitions */
    enum nfa_anchor_type anchor;/* the state's anchored-ness: used by regex */
    char tr_char;		/* if a single-character outgoing
				   transition, store it here */
    struct fsi_set* tr_cc;	/* if a character-class outgoing
				   transition, store it here */
    struct fsi_set* eps_closure;/* the state's epsilon-closure */

    unsigned int tag;		/* tag for the end state: this is for use with
				   multiple regexes, such as ugrep -f and ulex */

    /* the following is quasi-private, for traversal of NFAs */
    struct nfa_state* next1;
    struct nfa_state* next2;
    int mark;
};

/* Allocate & free a single NFA state */

struct nfa_state* nfa_state_alloc(void);
void nfa_state_free(struct nfa_state* ns);
void nfa_state_shallow_free(struct nfa_state* ns);

/* Operations on an entire NFA graph */

/* Apply a user-specified function to each node of an NFA. The user function
   may not alter the structure of the graph, and neither may it call another
   instance of nfa_foreach (on the same graph): this does not nest.

   The traversal_order argument specifies the order in which to do the work.
   This is important if information propagates from node to node: if it
   propagates from a node to its next[12] neighbors, then traversal_order
   should be positive; if it propagates in the other direction, then it
   should be negative; and if no information propagates, then it can be 0.
   For example, in computing the epsilon-closure, information travels
   against the pointers: a node's epsilon-closure is the union of its own
   and its next[12] neighbors' epsilon-closures. With traversal_order -1,
   the regex "(((... (a?) ...)?)?)?' takes 2 iterations regardless of the
   number of parentheses, whereas with traversal_order 0 or 1, it takes
   a number of iterations that's proportional to the number of parentheses. */

void nfa_foreach(struct nfa_state* ns,
		 void (*fn)(struct nfa_state*, void*),
		 void* data,
		 int traversal_order);

/* Find the end state. This is currently done in an efficient but non-general
   way that depends on the specific construction of the NFA graph in the
   parse-regex routine. It should perhaps be replaced with a slower but more
   general call to nfa_foreach(). */

struct nfa_state* nfa_find_end(struct nfa_state* ns);

/* Utility routine to print out an NFA graph in a vaguely-readable fashion */

void nfa_print(struct nfa_state* ns);

/* Free an entire NFA graph */

void nfa_free(struct nfa_state* ns);

/* Add an integer label to each node. This is needed for & used in the
   calculation of the epsilon-closure and in the regex_match routine. */

unsigned int nfa_label(struct nfa_state* ns);

/* Compute the epsilon-closure of each node in the NFA graph,
   and return the number of states in the NFA. */

unsigned int nfa_gen_epsilon_closure(struct nfa_state* ns);

/* Add an NFA to the head of an existing list of NFAs, which may be NULL */

void chain_nfa(struct nfa_state** chain, struct nfa_state* link);

#endif /* NFA_H */
