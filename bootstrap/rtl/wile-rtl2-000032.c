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

// @@@ (* . vs) @@@ bld-rtl-dir/wile-rtl2-000032.scm:13 @@@ wile_multiply @@@
lval wile_multiply(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
var_5 = LVI_BOOL(var_2[0].vt == LV_NIL);
if (LV_IS_FALSE(var_5)) {
lval var_6;
lval var_7;
var_7 = LVI_INT(0);
var_6 = var_7;
lval var_8;
var_8 = var_2[0];
lval var_10;
lval var_11;
lval var_12;
var_12 = LVI_INT(0);
var_10 = var_12;
lptr var_13 = new_lv(VT_UNINIT);
var_13->v.pair.car = &(var_10);
do {
lval var_14;
var_14 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_14)) {
break;
}
lval var_15;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000032.scm:19", "input is not a pair!");
}
var_15 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_16;
switch (var_15.vt) {
case LV_INT:
var_16 = LVI_INT(0);
break;
case LV_RAT:
var_16 = LVI_INT(1);
break;
case LV_REAL:
var_16 = LVI_INT(2);
break;
case LV_CMPLX:
var_16 = LVI_INT(3);
break;
default:
var_16 = LVI_INT(4);
break;
}
lval var_17;
var_17 = LVI_INT((var_6.v.iv > var_16.v.iv) ? var_6.v.iv : var_16.v.iv);
var_6 = var_17;
lval var_18;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000032.scm:20", "input is not a pair!");
}
var_18 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_18;
lval var_19;
var_19 = LVI_INT(1);
lval var_20;
var_20 = LVI_INT(var_10.v.iv + var_19.v.iv);
var_11 = var_20;
var_10 = var_11;
} while (1);
*var_13 = var_10;
var_8 = var_2[0];
lval var_21;
if (var_6.vt != LV_INT) {
wile_exception("case", "bld-rtl-dir/wile-rtl2-000032.scm:22", "case-value type does not match case type");
}
switch (var_6.v.iv) {
case 0:
{
lval var_22;
lval var_23;
var_23 = LVI_INT(1);
var_22 = var_23;
lval var_25;
lval var_26;
lval var_27;
var_27 = LVI_INT(0);
var_25 = var_27;
lptr var_28 = new_lv(VT_UNINIT);
var_28->v.pair.car = &(var_25);
do {
lval var_29;
var_29 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_29)) {
break;
}
lval var_30;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000032.scm:25", "input is not a pair!");
}
var_30 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_31;
var_31 = LVI_INT(var_22.v.iv * var_30.v.iv);
var_22 = var_31;
lval var_32;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000032.scm:26", "input is not a pair!");
}
var_32 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_32;
lval var_33;
var_33 = LVI_INT(1);
lval var_34;
var_34 = LVI_INT(var_25.v.iv + var_33.v.iv);
var_26 = var_34;
var_25 = var_26;
} while (1);
*var_28 = var_25;
var_21 = var_22;
break;
}
case 1:
{
lval var_35;
lval var_36;
var_36 = LVI_INT(1);
lval var_37;
if (var_36.vt == LV_INT) {
var_37 = LVI_RAT(var_36.v.iv, 1);
} else {
var_37 = var_36;
}
var_35 = var_37;
lval var_39;
lval var_40;
lval var_41;
var_41 = LVI_INT(0);
var_39 = var_41;
lptr var_42 = new_lv(VT_UNINIT);
var_42->v.pair.car = &(var_39);
do {
lval var_43;
var_43 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_43)) {
break;
}
lval var_44;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000032.scm:30", "input is not a pair!");
}
var_44 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_45;
if (var_44.vt == LV_INT) {
var_45 = LVI_RAT(var_44.v.iv, 1);
} else {
var_45 = var_44;
}
lval var_46;
{
lisp_int_t n, d, g;
n = var_35.v.irv.num * var_45.v.irv.num;
d = var_35.v.irv.den * var_45.v.irv.den;
g = lgcd(n, d);
n /= g;
d /= g;
var_46 = LVI_RAT(n, d);
}
var_35 = var_46;
lval var_47;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000032.scm:31", "input is not a pair!");
}
var_47 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_47;
lval var_48;
var_48 = LVI_INT(1);
lval var_49;
var_49 = LVI_INT(var_39.v.iv + var_48.v.iv);
var_40 = var_49;
var_39 = var_40;
} while (1);
*var_42 = var_39;
lval var_50;
lval var_51;
var_51 = LVI_INT(var_35.v.irv.den);
lval var_52;
var_52 = LVI_INT(1);
lval var_53;
switch (TYPE_COMBO(var_51.vt,var_52.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_53 = LVI_BOOL(var_51.v.iv == var_52.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_53 = LVI_BOOL(var_51.v.iv * var_52.v.irv.den == var_52.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_53 = LVI_BOOL(var_51.v.iv == var_52.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_53 = LVI_BOOL(var_51.v.irv.num == var_52.v.iv * var_51.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_53 = LVI_BOOL(var_51.v.irv.num * var_52.v.irv.den == var_52.v.irv.num * var_51.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_53 = LVI_BOOL(var_51.v.irv.num == var_52.v.rv * var_51.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_53 = LVI_BOOL(var_51.v.rv == var_52.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_53 = LVI_BOOL(var_51.v.rv * var_52.v.irv.den == var_52.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_53 = LVI_BOOL(var_51.v.rv == var_52.v.rv);
break;
default:
wile_exception("==", "bld-rtl-dir/wile-rtl2-000032.scm:32", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_53)) {
var_50 = var_35;
} else {
lval var_54;
var_54 = LVI_INT(var_35.v.irv.num);
var_50 = var_54;
}
var_21 = var_50;
break;
}
case 2:
{
lval var_55;
lval var_56;
var_56 = LVI_REAL(1.00000000000000000000000000000000000e+00Q);
var_55 = var_56;
lval var_58;
lval var_59;
lval var_60;
var_60 = LVI_INT(0);
var_58 = var_60;
lptr var_61 = new_lv(VT_UNINIT);
var_61->v.pair.car = &(var_58);
do {
lval var_62;
var_62 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_62)) {
break;
}
lval var_63;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000032.scm:37", "input is not a pair!");
}
var_63 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_64;
if (var_63.vt == LV_INT) {
var_64 = LVI_REAL((lisp_real_t) var_63.v.iv);
} else if (var_63.vt == LV_RAT) {
var_64 = LVI_REAL(LV_RAT2REAL(var_63));
} else {
var_64 = var_63;
}
lval var_65;
var_65 = LVI_REAL(var_55.v.rv * var_64.v.rv);
var_55 = var_65;
lval var_66;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000032.scm:38", "input is not a pair!");
}
var_66 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_66;
lval var_67;
var_67 = LVI_INT(1);
lval var_68;
var_68 = LVI_INT(var_58.v.iv + var_67.v.iv);
var_59 = var_68;
var_58 = var_59;
} while (1);
*var_61 = var_58;
var_21 = var_55;
break;
}
case 3:
{
lval var_69;
lval var_70;
var_70 = LVI_REAL(1.00000000000000000000000000000000000e+00Q);
lval var_71;
var_71 = LVI_REAL(0.00000000000000000000000000000000000e+00Q);
lval var_73;
if (var_70.vt == LV_INT) {
var_73 = LVI_REAL((lisp_real_t) var_70.v.iv);
} else if (var_70.vt == LV_RAT) {
var_73 = LVI_REAL(LV_RAT2REAL(var_70));
} else if (var_70.vt == LV_REAL) {
var_73 = var_70;
} else {
wile_exception("cmplx", "bld-rtl-dir/wile-rtl2-000032.scm:40", "expects a real-valued input");
}
lval var_74;
if (var_71.vt == LV_INT) {
var_74 = LVI_REAL((lisp_real_t) var_71.v.iv);
} else if (var_71.vt == LV_RAT) {
var_74 = LVI_REAL(LV_RAT2REAL(var_71));
} else if (var_71.vt == LV_REAL) {
var_74 = var_71;
} else {
wile_exception("cmplx", "bld-rtl-dir/wile-rtl2-000032.scm:40", "expects a real-valued input");
}
lval var_72;
var_72 = LVI_CMPLX2(var_73.v.rv, var_74.v.rv);
var_69 = var_72;
lval var_76;
lval var_77;
lval var_78;
var_78 = LVI_INT(0);
var_76 = var_78;
lptr var_79 = new_lv(VT_UNINIT);
var_79->v.pair.car = &(var_76);
do {
lval var_80;
var_80 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_80)) {
break;
}
lval var_81;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000032.scm:42", "input is not a pair!");
}
var_81 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_82;
switch (var_81.vt) {
case LV_INT:
var_82 = LVI_CMPLX2((lisp_real_t) var_81.v.iv, 0);
break;
case LV_RAT:
var_82 = LVI_CMPLX2(LV_RAT2REAL(var_81), 0);
break;
case LV_REAL:
var_82 = LVI_CMPLX2(var_81.v.rv, 0);
break;
default:
var_82 = var_81;
break;
}
lval var_83;
var_83 = LVI_CMPLX1(var_69.v.cv * var_82.v.cv);
var_69 = var_83;
lval var_84;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000032.scm:43", "input is not a pair!");
}
var_84 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_84;
lval var_85;
var_85 = LVI_INT(1);
lval var_86;
var_86 = LVI_INT(var_76.v.iv + var_85.v.iv);
var_77 = var_86;
var_76 = var_77;
} while (1);
*var_79 = var_76;
var_21 = var_69;
break;
}
default:
{
lval var_87;
var_87 = LVI_STRING("\'*\' got a non-numeric argument");
lval var_88;
{
lval var_89[1];
var_89[0] = var_87;
var_88 = wile_gen_list(1, var_89, NULL);
}
if (var_88.vt == LV_PAIR && (var_88.v.pair.cdr == NULL || var_88.v.pair.cdr->vt == LV_NIL)) {
var_88 = (var_88.v.pair.car ? *(var_88.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_88;
cachalot->whence = "bld-rtl-dir/wile-rtl2-000032.scm:45";
longjmp(cachalot->cenv, 1);
var_21 = var_88;
break;
}
}
var_4 = var_21;
} else {
lval var_90;
var_90 = LVI_INT(1);
var_4 = var_90;
}
return var_4;
}
// end of function wile_multiply
