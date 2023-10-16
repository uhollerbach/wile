// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#ifndef LIB_MACROS_H
#define LIB_MACROS_H

#define FATAL(fname, ...)						\
    do {								\
	fprintf(stderr, "fatal error: '%s' ", fname);			\
	fprintf(stderr, __VA_ARGS__);					\
	fputc('\n', stderr);						\
	exit(1);							\
    } while (0)

#define ERR(fname, ...)							\
    err_print(fname, 0, LISP_WHENCE, __VA_ARGS__)

#define WARN(fname, ...)						\
    do {								\
	fprintf(stderr, "warning: '%s' ", fname);			\
	fprintf(stderr, __VA_ARGS__);					\
	fputc('\n', stderr);						\
    } while (0)

#define IS_PAIR(a)		((a) != NULL && (a)->vt == LV_PAIR)
#define IS_SYMBOL(a)		((a) != NULL && (a)->vt == LV_SYMBOL)
#define IS_STRING(a)		((a) != NULL && (a)->vt == LV_STRING)
#define IS_INT(a)		((a) != NULL && (a)->vt == LV_INT)

#define IS_CLAMBDA(a)		((a) != NULL && (a)->vt == LV_CLAMBDA)
#define IS_ILAMBDA(a)		((a) != NULL && (a)->vt == LV_ILAMBDA)
#define IS_CONT(a)		((a) != NULL && (a)->vt == LV_CONT)
#define IS_FPORT(a)		((a) != NULL && (a)->vt == LV_FILE_PORT)
#define IS_PPORT(a)		((a) != NULL && (a)->vt == LV_PIPE_PORT)
#define IS_SOCKPORT(a)		((a) != NULL && (a)->vt == LV_SOCK_PORT)

#define IS_NUMERIC(a)						\
    ((a)->vt == LV_INT || (a)->vt == LV_RAT ||			\
     (a)->vt == LV_REAL || (a)->vt == LV_CMPLX)

#define CAN_RAT(n, d)							\
    do {								\
	lisp_int_t ag = lgcd(n, d);					\
	if (ag != 0) {							\
	    n /= ag;							\
	    d /= ag;							\
	}								\
	if (d < 0) {							\
	    d = -d;							\
	    n = -n;							\
	}								\
    } while (0)

#define REVERSE_LIST_INPLACE(lst)					\
    do {								\
	lptr RLIP_tmp1 = NULL;						\
	while (lst) {							\
	    lptr RLIP_tmp2 = CDR(lst);					\
	    CDR(lst) = RLIP_tmp1;					\
	    RLIP_tmp1 = (lst);						\
	    (lst) = RLIP_tmp2;						\
	}								\
	(lst) = RLIP_tmp1;						\
    } while (0)

#endif // LIB_MACROS_H
