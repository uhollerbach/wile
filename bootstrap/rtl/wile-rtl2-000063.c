// Generated by wile, the extremely stable scheming genius compiler

#include "wile-rtl1.h"
#include "wile-rtl2.h"

// declarations

extern lval var_argv;
extern lval var_cmd_name;
extern lval var_stdin;
extern lval var_stdout;
extern lval var_stderr;
extern lval var_pi;
extern lval var_euler_gamma;
extern lval var_show_sign;
extern lval var_int_base;
extern lval var_flt_base;
extern lval var_flt_precision;
#include "wile-rtl2.h"

// definitions

// @@@ (is-leap-year? y) @@@ bld-rtl-dir/wile-rtl2-000063.scm:15 @@@ wile_is_leap_year @@@
lval wile_is_leap_year(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
var_4 = LVI_BOOL(true);
do {
lval var_5;
var_5 = LVI_INT(4);
lval var_6;
{
lisp_int_t nq, nr;
trunc_qr(var_2[0].v.iv, var_5.v.iv, &nq, &nr);
var_6 = LVI_INT(nr);
}
lval var_7;
switch (var_6.vt) {
case LV_REAL:
var_7 = LVI_BOOL(var_6.v.rv == 0.0);
break;
case LV_RAT:
var_7 = LVI_BOOL((var_6.v.irv.num == 0 && var_6.v.irv.den != 0));
break;
case LV_INT:
var_7 = LVI_BOOL(var_6.v.iv == 0);
break;
case LV_CMPLX:
var_7 = LVI_BOOL(CREAL(var_6.v.cv) == 0.0 && CIMAG(var_6.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
var_4 = var_7;
if (LV_IS_FALSE(var_4)) { break; }
lval var_8;
var_8 = LVI_BOOL(false);
do {
lval var_9;
var_9 = LVI_INT(100);
lval var_10;
{
lisp_int_t nq, nr;
trunc_qr(var_2[0].v.iv, var_9.v.iv, &nq, &nr);
var_10 = LVI_INT(nr);
}
lval var_11;
switch (var_10.vt) {
case LV_REAL:
var_11 = LVI_BOOL(var_10.v.rv == 0.0);
break;
case LV_RAT:
var_11 = LVI_BOOL((var_10.v.irv.num == 0 && var_10.v.irv.den != 0));
break;
case LV_INT:
var_11 = LVI_BOOL(var_10.v.iv == 0);
break;
case LV_CMPLX:
var_11 = LVI_BOOL(CREAL(var_10.v.cv) == 0.0 && CIMAG(var_10.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
lval var_12;
var_12 = LVI_BOOL(LV_IS_FALSE(var_11));
var_8 = var_12;
if (!LV_IS_FALSE(var_8)) { break; }
lval var_13;
var_13 = LVI_INT(400);
lval var_14;
{
lisp_int_t nq, nr;
trunc_qr(var_2[0].v.iv, var_13.v.iv, &nq, &nr);
var_14 = LVI_INT(nr);
}
lval var_15;
switch (var_14.vt) {
case LV_REAL:
var_15 = LVI_BOOL(var_14.v.rv == 0.0);
break;
case LV_RAT:
var_15 = LVI_BOOL((var_14.v.irv.num == 0 && var_14.v.irv.den != 0));
break;
case LV_INT:
var_15 = LVI_BOOL(var_14.v.iv == 0);
break;
case LV_CMPLX:
var_15 = LVI_BOOL(CREAL(var_14.v.cv) == 0.0 && CIMAG(var_14.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
var_8 = var_15;
if (!LV_IS_FALSE(var_8)) { break; }
} while (0);
var_4 = var_8;
if (LV_IS_FALSE(var_4)) { break; }
} while (0);
return var_4;
}
// end of function wile_is_leap_year
