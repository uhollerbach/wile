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

// @@@ (- . vs) @@@ bld-rtl-dir/wile-rtl2-000032.scm:13 @@@ wile_subtract @@@
lval wile_subtract(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_BOOL(var_2[0].vt == LV_NIL);
if (LV_IS_FALSE(var_5)) {
lval var_6;
lval var_7;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_7 = (var_2[0].v.pair.cdr ? *(var_2[0].v.pair.cdr) : LVI_NIL());
lval var_8;
var_8 = LVI_BOOL(var_7.vt == LV_NIL);
if (LV_IS_FALSE(var_8)) {
lval var_9;
lval var_10;
var_10 = LVI_INT(0);
var_9 = var_10;
lval var_11;
var_11 = var_2[0];
lval var_13;
lval var_14;
lval var_15;
var_15 = LVI_INT(0);
var_13 = var_15;
do {
lval var_16;
var_16 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_16)) {
break;
}
lval var_17;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_17 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_18;
switch (var_17.vt) {
case LV_INT:
var_18 = LVI_INT(0);
break;
case LV_RAT:
var_18 = LVI_INT(1);
break;
case LV_REAL:
var_18 = LVI_INT(2);
break;
case LV_CMPLX:
var_18 = LVI_INT(3);
break;
default:
var_18 = LVI_INT(4);
break;
}
lval var_19;
var_19 = LVI_INT((var_9.v.iv > var_18.v.iv) ? var_9.v.iv : var_18.v.iv);
var_9 = var_19;
lval var_20;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_20 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_20;
lval var_21;
var_21 = LVI_INT(1);
lval var_22;
var_22 = LVI_INT(var_13.v.iv + var_21.v.iv);
var_14 = var_22;
var_13 = var_14;
} while (1);
var_11 = var_2[0];
lval var_23;
if (var_9.vt != LV_INT) {
wile_exception2("case", __FILE__, __LINE__, "case-value type does not match case type");
}
switch (var_9.v.iv) {
case 0:
{
lval var_24;
lval var_25;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_25 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
var_24 = var_25;
lval var_26;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_26 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_26;
lval var_28;
lval var_29;
lval var_30;
var_30 = LVI_INT(0);
var_28 = var_30;
do {
lval var_31;
var_31 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_31)) {
break;
}
lval var_32;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_32 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_33;
var_33 = LVI_INT(var_24.v.iv - var_32.v.iv);
var_24 = var_33;
lval var_34;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_34 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_34;
lval var_35;
var_35 = LVI_INT(1);
lval var_36;
var_36 = LVI_INT(var_28.v.iv + var_35.v.iv);
var_29 = var_36;
var_28 = var_29;
} while (1);
var_23 = var_24;
break;
}
case 1:
{
lval var_37;
lval var_38;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_38 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_39;
if (var_38.vt == LV_INT) {
var_39 = LVI_RAT(var_38.v.iv, 1);
} else {
var_39 = var_38;
}
var_37 = var_39;
lval var_40;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_40 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_40;
lval var_42;
lval var_43;
lval var_44;
var_44 = LVI_INT(0);
var_42 = var_44;
do {
lval var_45;
var_45 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_45)) {
break;
}
lval var_46;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_46 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_47;
if (var_46.vt == LV_INT) {
var_47 = LVI_RAT(var_46.v.iv, 1);
} else {
var_47 = var_46;
}
lval var_48;
{
lisp_int_t n, d, g;
n = var_37.v.irv.num * var_47.v.irv.den - var_47.v.irv.num * var_37.v.irv.den;
d = var_37.v.irv.den * var_47.v.irv.den;
g = lgcd(n, d);
n /= g;
d /= g;
var_48 = LVI_RAT(n, d);
}
var_37 = var_48;
lval var_49;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_49 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_49;
lval var_50;
var_50 = LVI_INT(1);
lval var_51;
var_51 = LVI_INT(var_42.v.iv + var_50.v.iv);
var_43 = var_51;
var_42 = var_43;
} while (1);
lval var_52;
lval var_53;
var_53 = LVI_INT(var_37.v.irv.den);
lval var_54;
var_54 = LVI_INT(1);
lval var_55;
switch (TYPE_COMBO(var_53.vt,var_54.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_55 = LVI_BOOL(var_53.v.iv == var_54.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_55 = LVI_BOOL(var_53.v.iv * var_54.v.irv.den == var_54.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_55 = LVI_BOOL(var_53.v.iv == var_54.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_55 = LVI_BOOL(var_53.v.irv.num == var_54.v.iv * var_53.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_55 = LVI_BOOL(var_53.v.irv.num * var_54.v.irv.den == var_54.v.irv.num * var_53.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_55 = LVI_BOOL(var_53.v.irv.num == var_54.v.rv * var_53.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_55 = LVI_BOOL(var_53.v.rv == var_54.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_55 = LVI_BOOL(var_53.v.rv * var_54.v.irv.den == var_54.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_55 = LVI_BOOL(var_53.v.rv == var_54.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_55)) {
var_52 = var_37;
} else {
lval var_56;
var_56 = LVI_INT(var_37.v.irv.num);
var_52 = var_56;
}
var_23 = var_52;
break;
}
case 2:
{
lval var_57;
lval var_58;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_58 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_59;
if (var_58.vt == LV_INT) {
var_59 = LVI_REAL((lisp_real_t) var_58.v.iv);
} else if (var_58.vt == LV_RAT) {
var_59 = LVI_REAL(LV_RAT2REAL(var_58));
} else {
var_59 = var_58;
}
var_57 = var_59;
lval var_60;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_60 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_60;
lval var_62;
lval var_63;
lval var_64;
var_64 = LVI_INT(0);
var_62 = var_64;
do {
lval var_65;
var_65 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_65)) {
break;
}
lval var_66;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_66 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_67;
if (var_66.vt == LV_INT) {
var_67 = LVI_REAL((lisp_real_t) var_66.v.iv);
} else if (var_66.vt == LV_RAT) {
var_67 = LVI_REAL(LV_RAT2REAL(var_66));
} else {
var_67 = var_66;
}
lval var_68;
var_68 = LVI_REAL(var_57.v.rv - var_67.v.rv);
var_57 = var_68;
lval var_69;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_69 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_69;
lval var_70;
var_70 = LVI_INT(1);
lval var_71;
var_71 = LVI_INT(var_62.v.iv + var_70.v.iv);
var_63 = var_71;
var_62 = var_63;
} while (1);
var_23 = var_57;
break;
}
case 3:
{
lval var_72;
lval var_73;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_73 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_74;
switch (var_73.vt) {
case LV_INT:
var_74 = LVI_CMPLX2((lisp_real_t) var_73.v.iv, 0);
break;
case LV_RAT:
var_74 = LVI_CMPLX2(LV_RAT2REAL(var_73), 0);
break;
case LV_REAL:
var_74 = LVI_CMPLX2(var_73.v.rv, 0);
break;
default:
var_74 = var_73;
break;
}
var_72 = var_74;
lval var_75;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_75 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_75;
lval var_77;
lval var_78;
lval var_79;
var_79 = LVI_INT(0);
var_77 = var_79;
do {
lval var_80;
var_80 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_80)) {
break;
}
lval var_81;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_81 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
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
var_83 = LVI_CMPLX1(var_72.v.cv - var_82.v.cv);
var_72 = var_83;
lval var_84;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_84 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_84;
lval var_85;
var_85 = LVI_INT(1);
lval var_86;
var_86 = LVI_INT(var_77.v.iv + var_85.v.iv);
var_78 = var_86;
var_77 = var_78;
} while (1);
var_23 = var_72;
break;
}
default:
{
lval var_87;
var_87 = LVI_STRING("\'-\' got a non-numeric argument");
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
cachalot->l_whence = 0;
cachalot->c_whence = LISP_WHENCE;
longjmp(cachalot->cenv, 1);
var_23 = var_88;
break;
}
}
var_6 = var_23;
} else {
lval var_90;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_90 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
lval var_91;
switch (var_90.vt) {
case LV_INT:
var_91 = LVI_INT(-var_90.v.iv);
break;
case LV_RAT:
if (var_90.v.irv.den >= 0) {
var_91 = LVI_RAT(-var_90.v.irv.num, var_90.v.irv.den);
} else {
var_91 = LVI_RAT(var_90.v.irv.num, -var_90.v.irv.den);
}
break;
case LV_REAL:
var_91 = LVI_REAL(-var_90.v.rv);
break;
case LV_CMPLX:
var_91 = LVI_CMPLX2(-CREAL(var_90.v.cv), -CIMAG(var_90.v.cv));
break;
default:
WILE_EX("negative", "got a non-numeric argument");
}
var_6 = var_91;
}
var_4 = var_6;
} else {
lval var_92;
var_92 = LVI_INT(0);
var_4 = var_92;
}
return var_4;
}
// end of function wile_subtract
