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

// (julian-day year month day)
lval wile_julian_day(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_INT(14);
lval var_6;
var_6 = LVI_INT(var_5.v.iv - var_2[1].v.iv);
lval var_7;
var_7 = LVI_INT(12);
lval var_8;
{
lisp_int_t nq, nr;
trunc_qr(var_6.v.iv, var_7.v.iv, &nq, &nr);
var_8 = LVI_INT(nq);
}
var_4 = var_8;
lval var_9;
lval var_10;
var_10 = LVI_INT(4800);
lval var_11;
switch (var_4.vt) {
case LV_INT:
var_11 = LVI_INT(-var_4.v.iv);
break;
case LV_RAT:
if (var_4.v.irv.den >= 0) {
var_11 = LVI_RAT(-var_4.v.irv.num, var_4.v.irv.den);
} else {
var_11 = LVI_RAT(var_4.v.irv.num, -var_4.v.irv.den);
}
break;
case LV_REAL:
var_11 = LVI_REAL(-var_4.v.rv);
break;
case LV_CMPLX:
var_11 = LVI_CMPLX2(-CREAL(var_4.v.cv), -CIMAG(var_4.v.cv));
break;
default:
WILE_EX("negative", "got a non-numeric argument");
}
lval var_12;
var_12 = LVI_INT(var_2[0].v.iv + var_10.v.iv + var_11.v.iv);
var_9 = var_12;
lval var_13;
lval var_14;
var_14 = LVI_INT(12);
lval var_15;
var_15 = LVI_INT(var_14.v.iv * var_4.v.iv);
lval var_16;
var_16 = LVI_INT(-3);
lval var_17;
var_17 = LVI_INT(var_2[1].v.iv + var_15.v.iv + var_16.v.iv);
var_13 = var_17;
lval var_18;
var_18 = LVI_INT(153);
lval var_19;
var_19 = LVI_INT(var_18.v.iv * var_13.v.iv);
lval var_20;
var_20 = LVI_INT(2);
lval var_21;
var_21 = LVI_INT(var_19.v.iv + var_20.v.iv);
lval var_22;
var_22 = LVI_INT(5);
lval var_23;
{
lisp_int_t nq, nr;
trunc_qr(var_21.v.iv, var_22.v.iv, &nq, &nr);
var_23 = LVI_INT(nq);
}
lval var_24;
var_24 = LVI_INT(365);
lval var_25;
var_25 = LVI_INT(var_24.v.iv * var_9.v.iv);
lval var_26;
var_26 = LVI_INT(4);
lval var_27;
{
lisp_int_t nq, nr;
trunc_qr(var_9.v.iv, var_26.v.iv, &nq, &nr);
var_27 = LVI_INT(nq);
}
lval var_28;
var_28 = LVI_INT(-100);
lval var_29;
{
lisp_int_t nq, nr;
trunc_qr(var_9.v.iv, var_28.v.iv, &nq, &nr);
var_29 = LVI_INT(nq);
}
lval var_30;
var_30 = LVI_INT(400);
lval var_31;
{
lisp_int_t nq, nr;
trunc_qr(var_9.v.iv, var_30.v.iv, &nq, &nr);
var_31 = LVI_INT(nq);
}
lval var_32;
var_32 = LVI_INT(-32045);
lval var_33;
var_33 = LVI_INT(var_2[2].v.iv + var_23.v.iv + var_25.v.iv + var_27.v.iv + var_29.v.iv + var_31.v.iv + var_32.v.iv);
return var_33;
}
// end of function wile_julian_day
