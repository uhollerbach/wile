// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "wile.h"
#include "alloc.h"
#include "lib-macros.h"

// Munge file names and line numbers together into one integer-y "location",
// a pair of bit-fields of which one denotes the file and the other the line
// number. Allocate 10 bits for the file and 22 bits for the line number.

#define BITS_FILE	10
#define BITS_LINE	22
#define N_FILES		(1 << BITS_FILE)
#define N_LINES		(1 << BITS_LINE)
#define FILE_MASK	((lisp_loc_t) (N_FILES - 1))
#define LINE_MASK	((lisp_loc_t) (N_LINES - 1))

static lisp_loc_t cur_file = FILE_MASK;
static lisp_loc_t n_files = 0;

static char* file_names[N_FILES];

// convert a line number into a lisp_loc_t, assuming that cur_file
// is the right file number - line numbers greater than 4 million (N_LINES
// above) will wrap around and start over at 0.

lisp_loc_t encode_line_loc(size_t lineno)
{
    return (cur_file << BITS_LINE) | (((lisp_loc_t) lineno) & LINE_MASK);
}

// convert a lisp_loc_t into a string "FILE:LINE"

char* decode_line_loc(lisp_loc_t lloc)
{
    char buf[1024];
    lisp_loc_t fno, lno;

    lno = lloc & LINE_MASK;
    fno = (lloc >> BITS_LINE) & FILE_MASK;
    if (fno < n_files) {
	snprintf(buf, sizeof(buf), "%s:%lu",
		 file_names[fno], (unsigned long) lno);
    } else if (fno == N_FILES - 1) {
	snprintf(buf, sizeof(buf), "<STREAM>:%lu", (unsigned long) lno);
    } else {
	snprintf(buf, sizeof(buf), "<UNKNOWN>:%lu", (unsigned long) lno);
    }
    return LISP_STRDUP(buf);
}

// set file name

void set_lisp_loc_file(const char* fname)
{
    lisp_loc_t i;

    if (fname == NULL) {
	cur_file = FILE_MASK;
    } else {
	for (i = 0; i < n_files; ++i) {
	    if (strcmp(file_names[i], fname) == 0) {
		cur_file = i;
		return;
	    }
	}
	if (i < N_FILES - 1) {
	    n_files = i + 1;
	} else {
	    fname = "<OVERFLOW>";
	}
	file_names[i] = LISP_STRDUP(fname);
	cur_file = i;
    }
}

lisp_loc_t get_lisp_loc(lptr vp)
{
    size_t i;
    lisp_loc_t o;

    if (vp) {
	switch (vp->vt) {
	case LV_SYMBOL:
	case LV_BOOL:
	case LV_CHAR:
	case LV_STRING:
	case LV_INT:
	case LV_RAT:
	case LV_REAL:
	case LV_CMPLX:
	case LV_FILE_PORT:
	case LV_PIPE_PORT:
	case LV_SOCK_PORT:
	case LV_STR_PORT:
	case LV_SQLITE_PORT:
	case LV_SQLITE_STMT:
	case LV_LAMBDA:
	case LV_CONT:
	case LV_BVECTOR:		// TODO: this one could be tricky
	    return vp->origin;

	case LV_PAIR:
	recurse:
	    o = get_lisp_loc(CAR(vp));
	    if (o > 0) {
		return o;
	    } else {
		if (CDR(vp)) {
		    if (IS_PAIR(CDR(vp))) {
			vp = CDR(vp);
			goto recurse;
		    } else {
			return get_lisp_loc(CDR(vp));
		    }
		}
	    }
	    return 0;			// should be unreachable

	case LV_VECTOR:
	    o = vp->origin;
	    for (i = 0; i < vp->v.vec.capa; ++i) {
		if (o > 0) {
		    return o;
		}
		o = get_lisp_loc(vp->v.vec.arr[i]);
	    }
	    return o;

	case LV_PROMISE:
	    FATAL("<get_lisp_loc>", "promises are not implemented yet!");

	default:
	    FATAL("<get_lisp_loc>", "bad type %d", vp->vt);
	}
    } else {
	return 0;
    }
}
