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

#include "config.h"

// prepare for autotools-based build

#ifndef WILE_USES_GC
#if HAVE_GC_H
#define WILE_USES_GC
#endif // HAVE_GC_H
#endif // WILE_USES_GC

#ifndef WILE_USES_SQLITE
#if HAVE_SQLITE3_H
#define WILE_USES_SQLITE
#endif // HAVE_SQLITE3_H
#endif // WILE_USES_SQLITE

#if !defined(WILE_USES_QUAD_DOUBLE) && !defined(WILE_USES_LONG_DOUBLE) && !defined(WILE_USES_DOUBLE)
#if HAVE_QUADMATH_H
#define WILE_USES_QUAD_DOUBLE
#else
#define WILE_USES_LONG_DOUBLE
#endif // HAVE_QUADMATH_H
#endif // !defined(double stuff) etc

#if !defined(WILE_USES_LONG_INT) && !defined(WILE_USES_INT128) && !defined(WILE_USES_BIGINT)
#if HAVE_INT128
#define WILE_USES_INT128
#else
#define WILE_USES_LONG_INT
#endif // HAVE_INT128
#endif // !defined(int stuff) etc

// end of prep for autotools

#include "wile.h"
#include "nfa.h"
#include "fsi_set.h"
#include "regex.h"
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

#define LVI_STRING_NOCPY(q)	((lval) { .v.str = (q),			\
					  .vt = LV_STRING})

#define LVI_PAIR(qa,qd)		((lval) { .v.pair.car = (qa),		\
					  .v.pair.cdr = (qd),		\
					  .vt = LV_PAIR})

#define LVI_FPORT(q)		((lval) { .v.fp = (q), .vt = LV_FILE_PORT})

#define LVI_PPORT(q)		((lval) { .v.fp = (q), .vt = LV_PIPE_PORT})

#define LVI_SPORT(q)		((lval) { .v.fp = (q), .vt = LV_SOCK_PORT})

#define LVI_PROC(qf,qc,qa)	((lval) { .v.clambda.fn = (qf),		\
					  .v.clambda.closure = (qc),	\
					  .v.clambda.arity = (qa),	\
					  .vt = LV_CLAMBDA})

#define LVI_IPROC(qargs,qari,qb,qe,qf)					\
    ((lval) { .v.ilambda.args = (qargs),				\
	      .v.ilambda.arity = (qari),				\
	      .v.ilambda.body = (qb),					\
	      .v.ilambda.env = (qe),					\
	      .v.ilambda.macro = (qf),					\
	      .vt = LV_ILAMBDA})

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

#define MK_CLOS(n,s)	lptr* n = LISP_ALLOC(lptr, s); LISP_ASSERT(n != NULL)
#define P_CLOS(n,i)	(n[i])
#define V_CLOS(n,i)	(*((P_CLOS(n,i)->vt == VT_UNINIT) \
			   ? P_CLOS(n,i)->v.pair.car : P_CLOS(n,i)))

uint16_t wile_binfo(void);
lval wile_sql_version(lptr* clos, lptr args, const char* loc);
lval wile_gc_version(lptr*, lptr, const char*);
lval wile_os_name(void);
lval wile_arch_name(void);

void wile_rand_seed(long int seed);
double wile_rand_dbl(void);

void wile_stack_trace_minimal(int fd);

void wile_exception(const char* func_name, const char* loc,
		    const char* fmt, ...)
    WILE_ATTR((noreturn,format(printf,3,4)));

#define WILE_EX(fname, ...) \
    wile_exception(fname, LISP_WHENCE, __VA_ARGS__)

lval wile_register_display_proc(const char* sym, lval proc, const char* loc);
lval wile_get_gensym(lisp_loc_t);
lval wile_run_system_command(lval cmd, const char* loc);
lval wile_run_pipe_command(lval cmd, const char* rw, const char* loc);
lval wile_temp_file(lptr* clos, lptr args, const char* loc);
lval wile_string2num(lval str, const char* loc);
lval wile_num2string(lval num, int base, int prec, const char* loc);

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

lval wile_gen_list(size_t nitems, lval* items, lval* tail);

bool wile_do_eqv(lptr arg1, lptr arg2);

lval wile_read_line(lptr* clos, lptr args, const char* loc);

lval wile_parse_string(lptr* clos, lptr args, const char* loc);
lval wile_parse_file(lptr* clos, lptr args, const char* loc);

lval wile_regex_match(lptr* clos, lptr args, const char* loc);
lval wile_apply_function(lptr args, const char* loc);
lval wile_read_directory(lptr* clos, lptr args, const char* loc);
lval wile_char2string(lptr* clos, lptr args, const char* loc);
lval wile_listen_port(lptr* clos, lptr args, const char* loc);
lval wile_accept_connection(lptr* clos, lptr args, const char* loc);
lval wile_connect_to(lptr* clos, lptr args, const char* loc);
lval wile_gethostname(lptr* clos, lptr args, const char* loc);
lval wile_getdomainname(lptr* clos, lptr args, const char* loc);
lval wile_getcwd(lptr* clos, lptr args, const char* loc);
lval wile_cputime(lptr* clos, lptr args, const char* loc);
lval wile_filestat(lptr* clos, lptr args, const char* loc);
lval wile_symlinkstat(lptr* clos, lptr args, const char* loc);
lval wile_getuserinfo(lptr* clos, lptr args, const char* loc);
lval wile_getalluserinfo(lptr*, lptr args, const char* loc);
lval wile_getgroupinfo(lptr* clos, lptr args, const char* loc);
lval wile_getallgroupinfo(lptr*, lptr args, const char* loc);
lval wile_localtime(lptr* clos, lptr args, const char* loc);
lval wile_gmtime(lptr* clos, lptr args, const char* loc);
lval wile_closeport(lptr* clos, lptr args, const char* loc);
lval wile_flushport(lptr* clos, lptr args, const char* loc);
lval wile_setlinebuffering(lptr* clos, lptr args, const char* loc);
lval wile_setnobuffering(lptr* clos, lptr args, const char* loc);
lval wile_setfilepos2(lptr* clos, lptr args, const char* loc);
lval wile_setfilepos3(lptr* clos, lptr args, const char* loc);
lval wile_string_reverse(lptr* clos, lptr args, const char* loc);
lval wile_string_hash_32(lptr*, lptr, const char* loc);
lval wile_string_hash_64(lptr*, lptr, const char* loc);
lval wile_string_ci_hash_32(lptr*, lptr, const char* loc);
lval wile_string_ci_hash_64(lptr*, lptr, const char* loc);

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

lisp_int_t lgcd(lisp_int_t p, lisp_int_t q);

void show_kv_pair(const void* vp, FILE* fp, lisp_bytevector_t* bvp);

lptr clear_display_hook(const char* sym);
lptr get_display_hook(const char* sym);
lptr set_display_hook(const char* sym, lptr hook);

void wile_print_lisp_val(lptr vp, FILE* fp, const char* loc);
void wile_sprint_lisp_num(char* buf, size_t bsize, lptr num,
			  int base, int prec, bool psign);

lisp_loc_t wile_encode_line_loc(size_t lineno);
char* wile_decode_line_loc(lisp_loc_t lloc);
void wile_set_lisp_loc_file(const char* fname);
lisp_loc_t wile_get_lisp_loc(lptr vp);

void err_print(const char* fname, lisp_loc_t l_whence,
	       const char* c_whence, const char* fmt, ...)
    WILE_ATTR((noreturn,format(printf,4,5)));

const char* typename(enum val_type vt);

// continuation stuff

int stack_check(int verbose);
void stack_base(void* st_bptr);
void wile_invoke_continuation(lptr cc, lptr args)
    WILE_ATTR((noreturn));

// wile low-level CFFT routines

void wilec_cfft_init(void);
bool wilec_cfft_good_n(lisp_int_t n);
lisp_cmplx_t* wilec_cfft(int si, size_t n, size_t nc,
			 lisp_cmplx_t* a1, lisp_cmplx_t* a2);

lval wile_expt(lval* a, lval* b);

lval wile_rand_normal_pair(lisp_real_t m, lisp_real_t s);

lval wile_cfft_good_n(lptr* clos, lptr args, const char* loc);
lval wile_cfft(lptr* clos, lptr args, const char* loc);

lval wile_sha256_wrap(bool is_256, lval input);
lval wile_sha256_init(bool is_256);
lval wile_sha256_update(lptr*, lptr, const char*);
lval wile_sha256_finish(lptr*, lptr args, const char* loc);

lval wile_waitpid(int pid, int opts);

// sqlite interface

lval wile_sql_open(const char* fname, int mode, const char* loc);
#ifdef WILE_USES_SQLITE
// have to hide this one because of the first argument:
// sqlite3* is unknown if we're not using sqlite
lval wile_sql_run(sqlite3* sqlite_conn, const char* cmd, const char* loc);
#endif // WILE_USES_SQLITE
lval wile_sql_stmt_prep(lptr* clos, lptr args, const char* loc);
lval wile_sql_stmt_clean(lptr* clos, lptr args, const char* loc);
lval wile_sql_stmt_info(lptr* clos, lptr args, const char* loc);
lval wile_sql_stmt_bind(lptr* clos, lptr args, const char* loc);
lval wile_sql_stmt_run(lptr* clos, lptr args, const char* loc);

// the infamous call-with-current-continuation

lval wile_call_cc(lptr* clos, lptr args, const char* loc);

lval wile_main(int argc, char** argv);

struct wile_profile_t {
    uint64_t count;
    char* name;
};

// This stuff is for checking that the configuration of the library is
// the same as the configuration of the main program: the library will
// contain a do-nothing routine WILE_CONFIG_SYM<N>, which is its
// configuration, and the main will call a routine WILE_CONFIG_SYM<N>
// which is its configuration. If they match, we're good

#define CAT(s1,s2)	s1 ## _ ## s2
#define XCAT(s1,s2)	CAT(s1,s2)

#if defined(WILE_USES_DOUBLE)
#define WILE_REAL_SYM		real_D
#elif  defined(WILE_USES_LONG_DOUBLE)
#define WILE_REAL_SYM		real_LD
#elif  defined(WILE_USES_QUAD_DOUBLE)
#define WILE_REAL_SYM		real_QD
#else
#error "wile real format was not specified!"
#endif // WILE_USES_DOUBLE

#define WILE_CONFIG_SYM1	XCAT(wile_config, WILE_REAL_SYM)

#if defined(WILE_USES_LONG_INT)
#define WILE_INT_SYM		int_LI
#elif  defined(WILE_USES_INT128)
#define WILE_INT_SYM		int_I128
#elif  defined(WILE_USES_BIGINT)
#define WILE_INT_SYM		int_BI
#else
#error "wile int format was not specified!"
#endif // WILE_USES_LONG_INT

#define WILE_CONFIG_SYM2	XCAT(WILE_CONFIG_SYM1, WILE_INT_SYM)

#if defined(WILE_USES_GC)
#define WILE_GC_SYM		gc_Y
#else
#define WILE_GC_SYM		gc_N
#endif // WILE_USES_GC

#define WILE_CONFIG_SYM3	XCAT(WILE_CONFIG_SYM2, WILE_GC_SYM)

#if defined(WILE_USES_SQLITE)
#define WILE_SQLITE_SYM		sqlite_Y
#else
#define WILE_SQLITE_SYM		sqlite_N
#endif // WILE_USES_SQLITE

#define WILE_CONFIG_SYM4	XCAT(WILE_CONFIG_SYM3, WILE_SQLITE_SYM)

// Not used yet
#if 0
#if defined(WILE_USES_RC4)
#define WILE_RC4_SYM		rc4_Y
#else
#define WILE_RC4_SYM		rc4_N
#endif // WILE_USES_RC4

#define WILE_CONFIG_SYM5	XCAT(WILE_CONFIG_SYM4, WILE_RC4_SYM)
#endif // 0

void WILE_CONFIG_SYM4(void);

#endif // WILE_RUNTIME_H
