#ifndef FSI_SET_H
#define FSI_SET_H

/*
This file is part of ulex -- Uwe's lex
Copyright 2013, Uwe Hollerbach <uhollerbach@gmail.com>
License: 2clause BSD, see file 'LICENSE' for details

$Id: fsi_set.h,v 1.9 2013/09/09 13:11:42 uwe Exp $

Routines to manipulate finite sets of generally-small integers.
Sets are represented as bit-sets stored in unsigned char, with
<n>-present-in-set represented as a 1-bit, and <n>-not-present
represented as a 0 bit, in the <n>th location in the bit-set.

These routines, where applicable, make the "closed-world"
assumption: the given bits are all there is; thus, for example,
inverting a set does not add an infinite number of 1-bits.
*/

struct fsi_set {
    unsigned int n_ints;
    unsigned char* bits;
};

struct fsi_set* fsi_set_alloc(unsigned int ni);
struct fsi_set* fsi_set_copy(struct fsi_set* set);
void fsi_set_free(struct fsi_set* set);
void fsi_set_clear(struct fsi_set* set);

void fsi_set_print(struct fsi_set* set);
void fsi_set_print_members(struct fsi_set* set);

void fsi_set_insert(struct fsi_set* set, unsigned int n);
void fsi_set_remove(struct fsi_set* set, unsigned int n);
void fsi_set_toggle(struct fsi_set* set, unsigned int n);
int fsi_set_ismember(struct fsi_set* set, unsigned int n);

/* These, like the above, are in-place:
   set1 |= set2, set1 &= set2, etc */

void fsi_set_or(struct fsi_set* set1, struct fsi_set* set2);
void fsi_set_xor(struct fsi_set* set1, struct fsi_set* set2);
void fsi_set_and(struct fsi_set* set1, struct fsi_set* set2);
void fsi_set_andnot(struct fsi_set* set1, struct fsi_set* set2);
void fsi_set_not(struct fsi_set* set);
void fsi_set_xfer(struct fsi_set* set1, struct fsi_set* set2);

/* Version 2 of or: compute the OR of two fsi_sets and return a count
   of diffs (of the stored bytes, not bits) */

unsigned int fsi_set_or2(struct fsi_set* set1, struct fsi_set* set2);

/* Compare two fsi_sets: return 1 if they are equal, 0 if unequal.
   They are unequal if they have different sizes, even if all the
   bits that are present in one are zero. */

int fsi_set_equal(struct fsi_set* set1, struct fsi_set* set2);

#endif /* FSI_SET_H */
