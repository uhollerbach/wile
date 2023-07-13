#ifndef WILE_RUNTIME_H
#define WILE_RUNTIME_H

// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <math.h>
#include <unistd.h>
#include <signal.h>
#include <limits.h>
#include <dirent.h>
#include <errno.h>
#include <netdb.h>
#include <pwd.h>
#include <grp.h>
#include <ctype.h>
#include <getopt.h>

#ifdef __linux__
#include <linux/limits.h>
#endif

#include "nfa.h"
#include "fsi_set.h"
#include "regex.h"
#include "wile.h"
#include "alloc.h"
#include "lib-macros.h"

#define LVI_NIL()		((lval) { .vt = LV_NIL})

#define LVI_BOOL(q)		((lval) { .v.bv = (q), .vt = LV_BOOL})

#define LVI_CHAR(q)		((lval) { .v.chr = (q), .vt = LV_CHAR})

#define LVI_INT(q)		((lval) { .v.iv = (q), .vt = LV_INT})

#define LVI_RAT(qn,qd)		((lval) { .v.irv.num = (qn),		\
					  .v.irv.den = (qd),		\
					  .vt = LV_RAT})

#define LVI_REAL(q)		((lval) { .v.rv = (q), .vt = LV_REAL})

#define LVI_CMPLX1(qc)		((lval) { .v.cv = (qc), .vt = LV_CMPLX})

#define LVI_CMPLX2(qr,qi)	((lval) { .v.cv = (qr) + (qi)*I,	\
					  .vt = LV_CMPLX})

#define LVI_STRING(q)		((lval) { .v.str = LISP_STRDUP(q),	\
					  .vt = LV_STRING})

#define LVI_SYMBOL(q)		((lval) { .v.str = LISP_STRDUP(q),	\
					  .vt = LV_SYMBOL})

#define LVI_PAIR(qa,qd)		((lval) { .v.pair.car = (qa),		\
					  .v.pair.cdr = (qd),		\
					  .vt = LV_PAIR})

#define LVI_FPORT(q)		((lval) { .v.fp = (q), .vt = LV_FILE_PORT})

#define LVI_PPORT(q)		((lval) { .v.fp = (q), .vt = LV_PIPE_PORT})

#define LVI_SPORT(q)		((lval) { .v.fp = (q), .vt = LV_SOCK_PORT})

#define LVI_PROC(qf,qc,qa)	((lval) { .v.lambda.fn = (qf),		\
					  .v.lambda.closure = (qc),	\
					  .v.lambda.arity = (qa),	\
					  .vt = LV_LAMBDA})

#define LV_IS_FALSE(q)		((q).vt == LV_BOOL && (q).v.bv == false)

#define LV_RAT2REAL(q)	(((lisp_real_t) (q).v.irv.num)/((lisp_real_t) (q).v.irv.den))

#define WILE_ABS(v)		(((v) < 0) ? -(v) : (v))
#define WILE_SIGN(v)		((v) > 0 ? 1 : ((v) < 0 ? -1 : 0))

// clang recognizes attribute musttail, which enables TCO

#ifdef __clang__
#define TAIL_CALL		__attribute__((musttail)) return
#else
#define TAIL_CALL		return
#endif

uint16_t wile_binfo(void);

void wile_stack_trace_minimal(int fd);

void wile_exception(const char* fname, const char* fmt, ...)
    WILE_ATTR((noreturn,format(printf,2,3)));

// updated version which includes file/line# info;
// don't want to convert everything all at once.
// TODO: eventually use this everywhere and remove the above

void wile_exception2(const char* func_name, const char* file_name,
		     int line_no, const char* fmt, ...)
    WILE_ATTR((noreturn,format(printf,4,5)));

#define WILE_EX(fname, ...) \
    wile_exception2(fname, __FILE__, __LINE__, __VA_ARGS__)

void display(lval val, FILE* fp);
lval register_display_proc(const char* sym, lval proc,
			   const char* fname, int lno);
lval get_gensym(void);
lval run_system_command(lval cmd, const char* fname, int lno);
lval run_pipe_command(lval cmd, const char* rw, const char* fname, int lno);
lval wile_temp_file(lptr* clos, lptr args);
lval string2num(lval str, const char* fname, int lno);
lval num2string(lval num, int base, int prec, const char* fname, int lno);

lisp_int_t powi(lisp_int_t a, lisp_int_t b);

lisp_real_t pcheby1(int n, lisp_real_t x);
lisp_real_t pcheby2(int n, lisp_real_t x);
lisp_real_t plegendre(int n, lisp_real_t x);
lisp_real_t plaguerre(int n, lisp_real_t x);
lisp_real_t phermite1(int n, lisp_real_t x);
lisp_real_t phermite2(int n, lisp_real_t x);

void floor_qr(lisp_int_t n1, lisp_int_t n2, lisp_int_t* nq, lisp_int_t* nr);
void trunc_qr(lisp_int_t n1, lisp_int_t n2, lisp_int_t* nq, lisp_int_t* nr);
void ceil_qr(lisp_int_t n1, lisp_int_t n2, lisp_int_t* nq, lisp_int_t* nr);

// Yes, an lval* is an lptr. Remember that this is an array of lvals,
// not a single lptr. This routine is not intended for zero-length lists;
// if 0 items are passed in, a NULL pointer dereference will result
// and the program will crash. The tail is for generating improper lists;
// passing NULL in makes a proper list.

lval gen_list(size_t nitems, lval* items, lval* tail);

bool do_eqv(lptr arg1, lptr arg2);

lval wile_read_line(lptr* clos, lptr args);

lval parse_string(lptr* clos, lptr args);
lval parse_file(lptr* clos, lptr args);

lval wile_regex_match(lptr* clos, lptr args);
lval wile_apply_function(lptr args, const char* file_name, int line_no);
lval read_directory(lptr* clos, lptr args);
lval wile_char2string(lptr* clos, lptr args);
lval wile_listen_port(lptr* clos, lptr args);
lval wile_accept_connection(lptr* clos, lptr args);
lval wile_connect_to(lptr* clos, lptr args);
lval wile_gethostname(lptr* clos, lptr args);
lval wile_getdomainname(lptr* clos, lptr args);
lval wile_getcwd(lptr* clos, lptr args);
lval wile_cputime(lptr* clos, lptr args);
lval wile_filestat(lptr* clos, lptr args);
lval wile_symlinkstat(lptr* clos, lptr args);
lval wile_getuserinfo(lptr* clos, lptr args);
lval wile_localtime(lptr* clos, lptr args);
lval wile_gmtime(lptr* clos, lptr args);
lval wile_closeport(lptr* clos, lptr args);
lval wile_flushport(lptr* clos, lptr args);
lval wile_setlinebuffering(lptr* clos, lptr args);
lval wile_setnobuffering(lptr* clos, lptr args);
lval wile_setfilepos2(lptr* clos, lptr args);
lval wile_setfilepos3(lptr* clos, lptr args);
lval wile_string_reverse(lptr* clos, lptr args);
lval wile_string_hash_32(lptr*, lptr);
lval wile_string_hash_64(lptr*, lptr);
lval wile_string_ci_hash_32(lptr*, lptr);
lval wile_string_ci_hash_64(lptr*, lptr);

// Log of Gamma function

lisp_cmplx_t logamma(lisp_cmplx_t z);

// Digamma function: logarithmic derivative of Gamma(x) = Gamma'(x)/Gamma(x)

lisp_cmplx_t digamma(lisp_cmplx_t z);

// Arithmetic-geometric mean

lisp_cmplx_t arith_geo_mean(lisp_cmplx_t a, lisp_cmplx_t g);

//  Compute the complete elliptic integrals
//
//  K(z) = integral_0^{pi/2} (1 - z*sin^2(t))^(-1/2) dt
//  E(z) = integral_0^{pi/2} (1 - z*sin^2(t))^(+1/2) dt
//
//  Note that the z is /not/ squared inside the integrand; this usage follows
//  Mathematica, but not the NIST Digital Library of Mathematical Functions.

lisp_cmplx_t elliptic_k(lisp_cmplx_t k);
lisp_cmplx_t elliptic_e(lisp_cmplx_t k);

// The two real branches of the Lambert W function: wp and wn.
// Initial guesses are from various expansions taken from Corless et al,
// "On the Lambert W Function"; the constants are wet-eyeball guesses.

// For non-negative x, the only real value of the Lambert W function;
// for -1/e < x < 0, the more-positive branch.

lisp_real_t lambert_wp_fn(lisp_real_t xl);

// For -1/e < x < 0, the more-negative branch of the Lambert W function

lisp_real_t lambert_wn_fn(lisp_real_t xl);

// All complex-valued branches of the Lambert W function, for all
// complex z; this includes as special values the two branches which
// happen to be real, above

lisp_cmplx_t lambert_wc_fn(int k, lisp_cmplx_t zl);

double sine_integral(double x);
double cosine_integral(double x);

lval wile_expt(lval* a, lval* b);

lval wile_rand_normal_pair(lisp_real_t m, lisp_real_t s);

lval wile_cfft_good_n(lptr* clos, lptr args);
lval wile_cfft(lptr* clos, lptr args);

// sqlite interface

lval wile_sql_version(lptr* clos, lptr args);

lval wile_sql_open(const char* fname, int mode,
		   const char* file_name, int line_no);
#ifdef WILE_USES_SQLITE
// have to hide this one because of the first argument:
// sqlite3* is unknown if we're not using sqlite
lval wile_sql_run(sqlite3* sqlite_conn, const char* cmd,
		  const char* file_name, int line_no);
#endif // WILE_USES_SQLITE
lval wile_sql_stmt_prep(lptr* clos, lptr args);
lval wile_sql_stmt_clean(lptr* clos, lptr args);
lval wile_sql_stmt_info(lptr* clos, lptr args);
lval wile_sql_stmt_bind(lptr* clos, lptr args);
lval wile_sql_stmt_run(lptr* clos, lptr args);

// the infamous call-with-current-continuation

lval wile_call_cc(lptr* clos, lptr args);

lval scheme_main(int argc, char** argv);

struct wile_profile_t {
    uint64_t count;
    char* name;
};

#endif // WILE_RUNTIME_H
