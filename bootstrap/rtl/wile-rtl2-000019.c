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

// @@@ (expmod a n m) @@@ bld-rtl-dir/wile-rtl2-000019.scm:13 @@@ wile_expmod @@@
lval wile_expmod(lptr* var_1, lptr var_2)
{
lbl_3:;
lval var_4;
do {
lval var_5;
switch (var_2[1].vt) {
case LV_REAL:
var_5 = LVI_BOOL(var_2[1].v.rv < 0.0);
break;
case LV_RAT:
var_5 = LVI_BOOL((var_2[1].v.irv.num < 0 && var_2[1].v.irv.den >= 0) || (var_2[1].v.irv.num > 0 && var_2[1].v.irv.den < 0));
break;
case LV_INT:
var_5 = LVI_BOOL(var_2[1].v.iv < 0);
break;
default:
WILE_EX("negative?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_5)) {
lval var_6;
var_6 = LVI_BOOL(false);
var_4 = var_6;
break;
}
lval var_7;
switch (var_2[1].vt) {
case LV_REAL:
var_7 = LVI_BOOL(var_2[1].v.rv == 0.0);
break;
case LV_RAT:
var_7 = LVI_BOOL((var_2[1].v.irv.num == 0 && var_2[1].v.irv.den != 0));
break;
case LV_INT:
var_7 = LVI_BOOL(var_2[1].v.iv == 0);
break;
case LV_CMPLX:
var_7 = LVI_BOOL(CREAL(var_2[1].v.cv) == 0.0 && CIMAG(var_2[1].v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_7)) {
lval var_8;
var_8 = LVI_INT(1);
lval var_9;
{
lisp_int_t nq, nr;
floor_qr(var_8.v.iv, var_2[2].v.iv, &nq, &nr);
var_9 = LVI_INT(nr);
}
var_4 = var_9;
break;
}
lval var_10;
var_10 = LVI_BOOL((var_2[1].v.iv)%2 == 0);
if (!LV_IS_FALSE(var_10)) {
lval var_11;
var_11 = LVI_INT(var_2[0].v.iv * var_2[0].v.iv);
lval var_12;
{
lisp_int_t nq, nr;
floor_qr(var_11.v.iv, var_2[2].v.iv, &nq, &nr);
var_12 = LVI_INT(nr);
}
lval var_13;
var_13 = LVI_INT(-1);
lval var_14;
var_14 = LVI_INT((var_13.v.iv >= 0) ? (var_2[1].v.iv << var_13.v.iv) : (var_2[1].v.iv >> -var_13.v.iv));
lval var_17[8];
var_17[0] = var_12;
var_17[1] = var_14;
var_17[2] = var_2[2];
var_2[0] = var_17[0];
var_2[1] = var_17[1];
var_2[2] = var_17[2];
goto lbl_3;	// selfie
}
lval var_18;
var_18 = LVI_INT(1);
lval var_19;
var_19 = LVI_INT(var_2[1].v.iv - var_18.v.iv);
lval var_20;
lval var_21[8];
var_21[0] = var_2[0];
var_21[1] = var_19;
var_21[2] = var_2[2];
var_20 = wile_expmod(NULL, var_21);
lval var_23;
var_23 = LVI_INT(var_2[0].v.iv * var_20.v.iv);
lval var_24;
{
lisp_int_t nq, nr;
floor_qr(var_23.v.iv, var_2[2].v.iv, &nq, &nr);
var_24 = LVI_INT(nr);
}
var_4 = var_24;
} while (0);
return var_4;
}
// end of function wile_expmod