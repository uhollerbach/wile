#ifndef ALLOC_H
#define ALLOC_H

// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include <stdlib.h>
#include <stdbool.h>

#define _STRINGIFY(x)		#x
#define LISP_STRING(x)		_STRINGIFY(x)
#define LISP_WHENCE		(__FILE__ ":" LISP_STRING(__LINE__))

#ifdef WILE_USES_GC

#include <gc.h>

#define LISP_ALLOC(t,c)		((t*) GC_malloc((c)*sizeof(t)))
#define LISP_REALLOC(t,o,c)	((t*) GC_realloc(o, (c)*sizeof(t)))
#define LISP_FREE(p)		GC_free(p)
#define LISP_FREE_STR(s)	GC_free(s)
#define LISP_FREE_LV(v)		/**/
#define LISP_STRDUP(s)		lisp_strdup(s)

#else

#define LISP_ALLOC(t,c)		((t*) malloc((c)*sizeof(t)))
#define LISP_REALLOC(t,o,c)	((t*) realloc(o, (c)*sizeof(t)))
#define LISP_FREE(p)		free(p)
#define LISP_FREE_STR(s)	free(s)
#define LISP_FREE_LV(v)		/**/
#define LISP_STRDUP(s)		lisp_strdup(s)

#endif // WILE_USES_GC

char* lisp_strdup(const char* str)		WILE_ATTR((malloc));

lptr new_lv(enum val_type vt)			WILE_ATTR((malloc));
lptr new_pair(lptr car, lptr cdr)		WILE_ATTR((malloc));
lptr new_bool(bool val)				WILE_ATTR((malloc));
lptr new_char(char val)				WILE_ATTR((malloc));
lptr new_int(lisp_int_t val)			WILE_ATTR((malloc));
lptr new_rat1(lisp_rat_t val)			WILE_ATTR((malloc));
lptr new_rat2(lisp_int_t num, lisp_int_t den)	WILE_ATTR((malloc));
lptr new_real(lisp_real_t val)			WILE_ATTR((malloc));
lptr new_cmplx1(lisp_cmplx_t cv)		WILE_ATTR((malloc));
lptr new_cmplx2(lisp_real_t rv, lisp_real_t iv)	WILE_ATTR((malloc));
lptr new_string_empty(size_t len)		WILE_ATTR((malloc));
lptr new_string(const char* str)		WILE_ATTR((malloc));
lptr new_vec(size_t capa)			WILE_ATTR((malloc));
lptr new_bvec(size_t capa)			WILE_ATTR((malloc));

lptr lst2vec(lptr lst);
lptr lst2bvec(lptr lst);

lptr new_spine(lptr lp);

#endif // ALLOC_H
