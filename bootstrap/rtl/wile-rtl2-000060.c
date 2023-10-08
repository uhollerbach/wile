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

// @@@ (julian-day-of-easter year) @@@ bld-rtl-dir/wile-rtl2-000060.scm:16 @@@ wile_julian_day_of_easter @@@
lval wile_julian_day_of_easter(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_INT(19);
lval var_6;
{
lisp_int_t nq, nr;
floor_qr(var_2[0].v.iv, var_5.v.iv, &nq, &nr);
var_6 = LVI_INT(nr);
}
var_4 = var_6;
lval var_7;
lval var_8;
var_8 = LVI_INT(100);
lval var_9;
{
lisp_int_t nq, nr;
trunc_qr(var_2[0].v.iv, var_8.v.iv, &nq, &nr);
var_9 = LVI_INT(nq);
}
var_7 = var_9;
lval var_10;
lval var_11;
var_11 = LVI_INT(100);
lval var_12;
{
lisp_int_t nq, nr;
floor_qr(var_2[0].v.iv, var_11.v.iv, &nq, &nr);
var_12 = LVI_INT(nr);
}
var_10 = var_12;
lval var_13;
lval var_14;
var_14 = LVI_INT(4);
lval var_15;
{
lisp_int_t nq, nr;
trunc_qr(var_7.v.iv, var_14.v.iv, &nq, &nr);
var_15 = LVI_INT(nq);
}
var_13 = var_15;
lval var_16;
lval var_17;
var_17 = LVI_INT(4);
lval var_18;
{
lisp_int_t nq, nr;
floor_qr(var_7.v.iv, var_17.v.iv, &nq, &nr);
var_18 = LVI_INT(nr);
}
var_16 = var_18;
lval var_19;
lval var_20;
var_20 = LVI_INT(8);
lval var_21;
var_21 = LVI_INT(var_7.v.iv + var_20.v.iv);
lval var_22;
var_22 = LVI_INT(25);
lval var_23;
{
lisp_int_t nq, nr;
trunc_qr(var_21.v.iv, var_22.v.iv, &nq, &nr);
var_23 = LVI_INT(nq);
}
var_19 = var_23;
lval var_24;
lval var_25;
var_25 = LVI_INT(var_7.v.iv - var_19.v.iv);
lval var_26;
var_26 = LVI_INT(1);
lval var_27;
var_27 = LVI_INT(var_25.v.iv + var_26.v.iv);
lval var_28;
var_28 = LVI_INT(3);
lval var_29;
{
lisp_int_t nq, nr;
trunc_qr(var_27.v.iv, var_28.v.iv, &nq, &nr);
var_29 = LVI_INT(nq);
}
var_24 = var_29;
lval var_30;
lval var_31;
var_31 = LVI_INT(19);
lval var_32;
var_32 = LVI_INT(var_31.v.iv * var_4.v.iv);
lval var_33;
var_33 = LVI_INT(15);
lval var_34;
var_34 = LVI_INT(var_32.v.iv + var_7.v.iv + var_33.v.iv);
lval var_35;
var_35 = LVI_INT(var_34.v.iv - var_13.v.iv - var_24.v.iv);
lval var_36;
var_36 = LVI_INT(30);
lval var_37;
{
lisp_int_t nq, nr;
floor_qr(var_35.v.iv, var_36.v.iv, &nq, &nr);
var_37 = LVI_INT(nr);
}
var_30 = var_37;
lval var_38;
lval var_39;
var_39 = LVI_INT(4);
lval var_40;
{
lisp_int_t nq, nr;
trunc_qr(var_10.v.iv, var_39.v.iv, &nq, &nr);
var_40 = LVI_INT(nq);
}
var_38 = var_40;
lval var_41;
lval var_42;
var_42 = LVI_INT(4);
lval var_43;
{
lisp_int_t nq, nr;
floor_qr(var_10.v.iv, var_42.v.iv, &nq, &nr);
var_43 = LVI_INT(nr);
}
var_41 = var_43;
lval var_44;
lval var_45;
var_45 = LVI_INT(32);
lval var_46;
var_46 = LVI_INT(2);
lval var_47;
var_47 = LVI_INT(var_46.v.iv * var_16.v.iv);
lval var_48;
var_48 = LVI_INT(2);
lval var_49;
var_49 = LVI_INT(var_48.v.iv * var_38.v.iv);
lval var_50;
var_50 = LVI_INT(var_45.v.iv + var_47.v.iv + var_49.v.iv);
lval var_51;
var_51 = LVI_INT(var_50.v.iv - var_30.v.iv - var_41.v.iv);
lval var_52;
var_52 = LVI_INT(7);
lval var_53;
{
lisp_int_t nq, nr;
floor_qr(var_51.v.iv, var_52.v.iv, &nq, &nr);
var_53 = LVI_INT(nr);
}
var_44 = var_53;
lval var_54;
lval var_55;
var_55 = LVI_INT(11);
lval var_56;
var_56 = LVI_INT(var_55.v.iv * var_30.v.iv);
lval var_57;
var_57 = LVI_INT(22);
lval var_58;
var_58 = LVI_INT(var_57.v.iv * var_44.v.iv);
lval var_59;
var_59 = LVI_INT(var_4.v.iv + var_56.v.iv + var_58.v.iv);
lval var_60;
var_60 = LVI_INT(451);
lval var_61;
{
lisp_int_t nq, nr;
trunc_qr(var_59.v.iv, var_60.v.iv, &nq, &nr);
var_61 = LVI_INT(nq);
}
var_54 = var_61;
lval var_62;
lval var_63;
var_63 = LVI_INT(114);
lval var_64;
var_64 = LVI_INT(var_30.v.iv + var_44.v.iv + var_63.v.iv);
lval var_65;
var_65 = LVI_INT(7);
lval var_66;
var_66 = LVI_INT(var_65.v.iv * var_54.v.iv);
lval var_67;
var_67 = LVI_INT(var_64.v.iv - var_66.v.iv);
lval var_68;
var_68 = LVI_INT(31);
lval var_69;
{
lval vs[2];
lisp_int_t nq, nr;
trunc_qr(var_67.v.iv, var_68.v.iv, &nq, &nr);
vs[0] = LVI_INT(nq);
vs[1] = LVI_INT(nr);
var_69 = gen_list(2, vs, NULL);
}
var_62 = var_69;
lval var_70;
lval var_71;
if (var_62.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_71 = (var_62.v.pair.car ? *(var_62.v.pair.car) : LVI_NIL());
var_70 = var_71;
lval var_72;
lval var_73;
var_73 = LVI_STRING("cadr");
lval var_74;
{
char* cp = strchr(var_73.v.str, 'r');
var_74 = var_62;
while (*(--cp) != 'c') {
if (var_74.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_74 = (var_74.v.pair.car ? *(var_74.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_74 = (var_74.v.pair.cdr ? *(var_74.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_73.v.str);
}
}
}
lval var_75;
var_75 = LVI_INT(1);
lval var_76;
var_76 = LVI_INT(var_74.v.iv + var_75.v.iv);
var_72 = var_76;
lval var_77;
{
lval vs[8];
vs[0] = var_2[0];
vs[1] = var_70;
vs[2] = var_72;
var_77 = wile_julian_day(NULL, vs);
}
return var_77;
}
// end of function wile_julian_day_of_easter