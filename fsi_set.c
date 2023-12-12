// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

// This file contains routines to manipulate finite sets of
// generally-small integers. Sets are represented as bit-sets stored in
// unsigned char, with <n>-present-in-set represented as a 1-bit, and
// <n>-not-present represented as a 0 bit, in the <n>th location in the
// bit-set.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "wile.h"
#include "fsi_set.h"
#include "alloc.h"

/* Allocate a fsi_set, specifying the number of integers which it can hold */

struct fsi_set* fsi_set_alloc(unsigned int ni)
{
    unsigned int nb;
    struct fsi_set* set;

    set = LISP_ALLOC(struct fsi_set, 1);
    nb = 1 + ni/CHAR_BIT;
    set->n_ints = CHAR_BIT*nb;
    set->bits = LISP_ALLOC(unsigned char, nb);
    memset(set->bits, 0, nb);
    return(set);
}

/* Copy an fsi_set */

struct fsi_set* fsi_set_copy(struct fsi_set* set)
{
    struct fsi_set* copy;

    if (set) {
	copy = LISP_ALLOC(struct fsi_set, 1);
	copy->n_ints = set->n_ints;
	copy->bits = LISP_ALLOC(unsigned char, copy->n_ints/CHAR_BIT);
	memcpy(copy->bits, set->bits, copy->n_ints/CHAR_BIT);
	return(copy);
    } else {
	return(NULL);
    }
}

/* Free an fsi_set */

void fsi_set_free(struct fsi_set* set)
{
    if (set) {
	LISP_FREE(set->bits);
	LISP_FREE(set);
    }
}

/* Dump an fsi_set in some vaguely-readable format */

void fsi_set_print(struct fsi_set* set)
{
    unsigned int i, nb;

    if (set) {
	printf("# %u\n", set->n_ints);
	nb = set->n_ints/CHAR_BIT;
	for (i = 0; i < nb; ++i) {
	    printf(" %02x", set->bits[i]);
	    if (i%25 == 24) {
		putchar('\n');
	    }
	}
	if (i%25 != 0) {
	    putchar('\n');
	}
    }
}

/* Write out only the members of the set */

void fsi_set_print_members(struct fsi_set* set)
{
    unsigned int i;

    if (set) {
	for (i = 0; i < set->n_ints; ++i) {
	    if (fsi_set_ismember(set, i)) {
		printf(" %u", i);
	    }
	}
    }
}

/* Clear all bits in an fsi_set */

void fsi_set_clear(struct fsi_set* set)
{
    if (set) {
	memset(set->bits, 0, set->n_ints/CHAR_BIT);
    }
}

/* Insert an integer into an fsi_set */

void fsi_set_insert(struct fsi_set* set, unsigned int n)
{
    unsigned int i, j;

    if (set != NULL && n < set->n_ints) {
	i = n/CHAR_BIT;
	j = n%CHAR_BIT;
	set->bits[i] |= 1 << j;
    } else {
	fprintf(stderr, "error: null set or %u too large to insert!\n", n);
    }
}

/* Remove an integer from an fsi_set */

void fsi_set_remove(struct fsi_set* set, unsigned int n)
{
    unsigned int i, j;

    if (set != NULL && n < set->n_ints) {
	i = n/CHAR_BIT;
	j = n%CHAR_BIT;
	set->bits[i] &= ~(1 << j);
    } else {
	fprintf(stderr, "error: null set or %u too large to remove!\n", n);
    }
}

/* Toggle an integer in an fsi_set: add it if it's not there,
   remove it if it is */

void fsi_set_toggle(struct fsi_set* set, unsigned int n)
{
    unsigned int i, j;

    if (set != NULL && n < set->n_ints) {
	i = n/CHAR_BIT;
	j = n%CHAR_BIT;
	set->bits[i] ^= 1 << j;
    } else {
	fprintf(stderr, "error: null set or %u too large to toggle!\n", n);
    }
}

/* Check if an integer is in an fsi_set */

int fsi_set_ismember(struct fsi_set* set, unsigned int n)
{
    unsigned int i, j;

    if (set != NULL && n < set->n_ints) {
	i = n/CHAR_BIT;
	j = n%CHAR_BIT;
	return(set->bits[i] & (1 << j) ? 1 : 0);
    } else {
	return(0);
    }
}

/* Compare two fsi_sets for equality */

int fsi_set_equal(struct fsi_set* set1, struct fsi_set* set2)
{
    unsigned int i, nb;

    if (set1 && set2) {
	if (set1->n_ints == set2->n_ints) {
	    nb = set1->n_ints/CHAR_BIT;
	    for (i = 0; i < nb; ++i) {
		if (set1->bits[i] != set2->bits[i]) {
		    return(0);
		}
	    }
	    return(1);
	} else {
	    return(0);
	}
    } else {
	return(0);
    }
}

/* Compute the OR of two fsi_sets */

void fsi_set_or(struct fsi_set* set1, struct fsi_set* set2)
{
    unsigned int i, nb1, nb2;

    if (set1 && set2) {
	nb1 = set1->n_ints/CHAR_BIT;
	nb2 = set2->n_ints/CHAR_BIT;
	if (nb2 < nb1) {
	    nb1 = nb2;
	}
	for (i = 0; i < nb1; ++i) {
	    set1->bits[i] |= set2->bits[i];
	}
    }
}

/* Version 2: compute the OR of two fsi_sets and return a count of diffs
   (of the stored bytes, not bits) */

unsigned int fsi_set_or2(struct fsi_set* set1, struct fsi_set* set2)
{
    unsigned int i, nb1, nb2, d;
    unsigned char o;

    d = 0;
    if (set1 && set2) {
	nb1 = set1->n_ints/CHAR_BIT;
	nb2 = set2->n_ints/CHAR_BIT;
	if (nb2 < nb1) {
	    nb1 = nb2;
	}
	for (i = 0; i < nb1; ++i) {
	    o = set1->bits[i] | set2->bits[i];
	    if (o != set1->bits[i]) {
		set1->bits[i] = o;
		++d;
	    }
	}
    }
    return(d);
}

/* Compute the XOR of two fsi_sets */

void fsi_set_xor(struct fsi_set* set1, struct fsi_set* set2)
{
    unsigned int i, nb1, nb2;

    if (set1 && set2) {
	nb1 = set1->n_ints/CHAR_BIT;
	nb2 = set2->n_ints/CHAR_BIT;
	if (nb2 < nb1) {
	    nb1 = nb2;
	}
	for (i = 0; i < nb1; ++i) {
	    set1->bits[i] ^= set2->bits[i];
	}
    }
}

/* Compute the AND of two fsi_sets */

void fsi_set_and(struct fsi_set* set1, struct fsi_set* set2)
{
    unsigned int i, nb1, nb2;

    if (set1 && set2) {
	nb1 = set1->n_ints/CHAR_BIT;
	nb2 = set2->n_ints/CHAR_BIT;
	if (nb2 < nb1) {
	    i = nb1;
	    nb1 = nb2;
	    nb2 = i;
	}
	for (i = 0; i < nb1; ++i) {
	    set1->bits[i] &= set2->bits[i];
	}
	while (i < nb2) {
	    set1->bits[i++] = 0;
	}
    } else if (set1) {
	nb1 = set1->n_ints/CHAR_BIT;
	for (i = 0; i < nb1; ++i) {
	    set1->bits[i] = 0;
	}
    }
}

/* Compute the ANDNOT of two fsi_sets */

void fsi_set_andnot(struct fsi_set* set1, struct fsi_set* set2)
{
    unsigned int i, nb1, nb2;

    if (set1 && set2) {
	nb1 = set1->n_ints/CHAR_BIT;
	nb2 = set2->n_ints/CHAR_BIT;
	if (nb2 < nb1) {
	    i = nb1;
	    nb1 = nb2;
	    nb2 = i;
	}
	for (i = 0; i < nb1; ++i) {
	    set1->bits[i] &= ~set2->bits[i];
	}
    }
}

/* Invert an fsi_set */

void fsi_set_not(struct fsi_set* set)
{
    unsigned int i, nb;

    if (set) {
	nb = set->n_ints/CHAR_BIT;
	for (i = 0; i < nb; ++i) {
	    set->bits[i] = ~set->bits[i];
	}
    }
}

/* Copy information from one fsi_set to another already-existing fsi_set.
   If the source fsi_set is NULL, this zeroes out the destination fsi_set. */

void fsi_set_xfer(struct fsi_set* set1, struct fsi_set* set2)
{
    unsigned int i, nb1, nb2;

    if (set1 && set2) {
	nb1 = set1->n_ints/CHAR_BIT;
	nb2 = set2->n_ints/CHAR_BIT;
	if (nb2 < nb1) {
	    for (i = 0; i < nb2; ++i) {
		set1->bits[i] = set2->bits[i];
	    }
	    for (i = nb2; i < nb1; ++i) {
		set1->bits[i] = 0;
	    }
	} else {
	    for (i = 0; i < nb1; ++i) {
		set1->bits[i] = set2->bits[i];
	    }
	}
    } else if (set1) {
	nb1 = set1->n_ints/CHAR_BIT;
	for (i = 0; i < nb1; ++i) {
	    set1->bits[i] = 0;
	}
    }

}

#if 0

int main(int argc, char** argv)
{
    int i;
    struct fsi_set* set;

    set = fsi_set_alloc(390);
    fsi_set_print(set);
    fsi_set_free(set);
    set = fsi_set_alloc(403);
    fsi_set_print(set);
    fsi_set_free(set);

    set = fsi_set_alloc(400);
    fsi_set_print(set);

    printf("insert\n");
    for (i = 0; i < 30; ++i) {
	fsi_set_insert(set, i);
	fsi_set_print(set);
    }
    printf("remove\n");
    for (i = 0; i < 15; ++i) {
	fsi_set_remove(set, i);
	fsi_set_print(set);
    }
    printf("toggle\n");
    for (i = 0; i < 30; i += 2) {
	fsi_set_toggle(set, i);
	fsi_set_print(set);
    }

    return(0);
}
#endif /* 0 */
