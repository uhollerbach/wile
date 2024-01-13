// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


// Yes, an lval* is an lptr. Remember that this is an array of lvals,
// not a single lptr. This routine is not intended for zero-length lists;
// if 0 items are passed in, a NULL pointer dereference will result
// and the program will crash.

lval wile_gen_list(size_t nitems, lval* items, lval* tail)
{
    lptr p1, list = NULL;

    if (tail) {
	list = new_lv(LV_NIL);
	*list = *tail;
    }
    while (nitems > 0) {
	p1 = new_lv(LV_NIL);
	*p1 = items[--nitems];
	list = new_pair(p1, list);
    }
    LISP_ASSERT(list != NULL);
    return *list;
}

