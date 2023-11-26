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

// @@@ (gregorian-date jday) @@@ bld-rtl-dir/wile-rtl2-000058.scm:18 @@@ wile_gregorian_date @@@
lval wile_gregorian_date(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
var_5 = LVI_INT(68569);
lval var_6;
var_6 = LVI_INT(var_2[0].v.iv + var_5.v.iv);
var_4 = var_6;
lval var_7;
lval var_8;
var_8 = LVI_INT(4);
lval var_9;
var_9 = LVI_INT(var_8.v.iv * var_4.v.iv);
lval var_10;
var_10 = LVI_INT(146097);
lval var_11;
{
lisp_int_t nq, nr;
trunc_qr(var_9.v.iv, var_10.v.iv, &nq, &nr);
var_11 = LVI_INT(nq);
}
var_7 = var_11;
lval var_12;
lval var_13;
var_13 = LVI_INT(146097);
lval var_14;
var_14 = LVI_INT(var_13.v.iv * var_7.v.iv);
lval var_15;
var_15 = LVI_INT(3);
lval var_16;
var_16 = LVI_INT(var_14.v.iv + var_15.v.iv);
lval var_17;
var_17 = LVI_INT(4);
lval var_18;
{
lisp_int_t nq, nr;
trunc_qr(var_16.v.iv, var_17.v.iv, &nq, &nr);
var_18 = LVI_INT(nq);
}
lval var_19;
var_19 = LVI_INT(var_4.v.iv - var_18.v.iv);
var_12 = var_19;
lval var_20;
lval var_21;
var_21 = LVI_INT(4000);
lval var_22;
var_22 = LVI_INT(1);
lval var_23;
var_23 = LVI_INT(var_12.v.iv + var_22.v.iv);
lval var_24;
var_24 = LVI_INT(var_21.v.iv * var_23.v.iv);
lval var_25;
var_25 = LVI_INT(1461001);
lval var_26;
{
lisp_int_t nq, nr;
trunc_qr(var_24.v.iv, var_25.v.iv, &nq, &nr);
var_26 = LVI_INT(nq);
}
var_20 = var_26;
lval var_27;
lval var_28;
var_28 = LVI_INT(31);
lval var_29;
var_29 = LVI_INT(1461);
lval var_30;
var_30 = LVI_INT(var_29.v.iv * var_20.v.iv);
lval var_31;
var_31 = LVI_INT(4);
lval var_32;
{
lisp_int_t nq, nr;
trunc_qr(var_30.v.iv, var_31.v.iv, &nq, &nr);
var_32 = LVI_INT(nq);
}
lval var_33;
var_33 = LVI_INT(var_28.v.iv - var_32.v.iv);
lval var_34;
var_34 = LVI_INT(var_12.v.iv + var_33.v.iv);
var_27 = var_34;
lval var_35;
lval var_36;
var_36 = LVI_INT(80);
lval var_37;
var_37 = LVI_INT(var_36.v.iv * var_27.v.iv);
lval var_38;
var_38 = LVI_INT(2447);
lval var_39;
{
lisp_int_t nq, nr;
trunc_qr(var_37.v.iv, var_38.v.iv, &nq, &nr);
var_39 = LVI_INT(nq);
}
var_35 = var_39;
lval var_40;
lval var_41;
var_41 = LVI_INT(2447);
lval var_42;
var_42 = LVI_INT(var_41.v.iv * var_35.v.iv);
lval var_43;
var_43 = LVI_INT(80);
lval var_44;
{
lisp_int_t nq, nr;
trunc_qr(var_42.v.iv, var_43.v.iv, &nq, &nr);
var_44 = LVI_INT(nq);
}
lval var_45;
var_45 = LVI_INT(var_27.v.iv - var_44.v.iv);
var_40 = var_45;
lval var_46;
lval var_47;
var_47 = LVI_INT(11);
lval var_48;
{
lisp_int_t nq, nr;
trunc_qr(var_35.v.iv, var_47.v.iv, &nq, &nr);
var_48 = LVI_INT(nq);
}
var_46 = var_48;
lval var_49;
lval var_50;
var_50 = LVI_INT(2);
lval var_51;
var_51 = LVI_INT(12);
lval var_52;
var_52 = LVI_INT(var_51.v.iv * var_46.v.iv);
lval var_53;
switch (var_52.vt) {
case LV_INT:
var_53 = LVI_INT(-var_52.v.iv);
break;
case LV_RAT:
if (var_52.v.irv.den >= 0) {
var_53 = LVI_RAT(-var_52.v.irv.num, var_52.v.irv.den);
} else {
var_53 = LVI_RAT(var_52.v.irv.num, -var_52.v.irv.den);
}
break;
case LV_REAL:
var_53 = LVI_REAL(-var_52.v.rv);
break;
case LV_CMPLX:
var_53 = LVI_CMPLX2(-CREAL(var_52.v.cv), -CIMAG(var_52.v.cv));
break;
default:
WILE_EX("negative", "got a non-numeric argument");
}
lval var_54;
var_54 = LVI_INT(var_35.v.iv + var_50.v.iv + var_53.v.iv);
var_49 = var_54;
lval var_55;
lval var_56;
var_56 = LVI_INT(100);
lval var_57;
var_57 = LVI_INT(49);
lval var_58;
var_58 = LVI_INT(var_7.v.iv - var_57.v.iv);
lval var_59;
var_59 = LVI_INT(var_56.v.iv * var_58.v.iv);
lval var_60;
var_60 = LVI_INT(var_59.v.iv + var_20.v.iv + var_46.v.iv);
var_55 = var_60;
lval var_61;
{
lval var_62[3];
var_62[0] = var_55;
var_62[1] = var_49;
var_62[2] = var_40;
var_61 = wile_gen_list(3, var_62, NULL);
}
return var_61;
}
// end of function wile_gregorian_date
