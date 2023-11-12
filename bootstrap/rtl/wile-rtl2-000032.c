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
lptr var_16 = new_lv(VT_UNINIT);
var_16->v.pair.car = &(var_13); //  symbol.3
do {
lval var_17;
var_17 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_17)) {
break;
}
lval var_18;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_18 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_19;
switch (var_18.vt) {
case LV_INT:
var_19 = LVI_INT(0);
break;
case LV_RAT:
var_19 = LVI_INT(1);
break;
case LV_REAL:
var_19 = LVI_INT(2);
break;
case LV_CMPLX:
var_19 = LVI_INT(3);
break;
default:
var_19 = LVI_INT(4);
break;
}
lval var_20;
var_20 = LVI_INT((var_9.v.iv > var_19.v.iv) ? var_9.v.iv : var_19.v.iv);
var_9 = var_20;
lval var_21;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_21 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_21;
lval var_22;
var_22 = LVI_INT(1);
lval var_23;
var_23 = LVI_INT(var_13.v.iv + var_22.v.iv);
var_14 = var_23;
var_13 = var_14;
} while (1);
*var_16 = var_13;
var_11 = var_2[0];
lval var_24;
if (var_9.vt != LV_INT) {
wile_exception2("case", __FILE__, __LINE__, "case-value type does not match case type");
}
switch (var_9.v.iv) {
case 0:
{
lval var_25;
lval var_26;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_26 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
var_25 = var_26;
lval var_27;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_27 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_27;
lval var_29;
lval var_30;
lval var_31;
var_31 = LVI_INT(0);
var_29 = var_31;
lptr var_32 = new_lv(VT_UNINIT);
var_32->v.pair.car = &(var_29); //  symbol.4
do {
lval var_33;
var_33 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_33)) {
break;
}
lval var_34;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_34 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_35;
var_35 = LVI_INT(var_25.v.iv - var_34.v.iv);
var_25 = var_35;
lval var_36;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_36 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_36;
lval var_37;
var_37 = LVI_INT(1);
lval var_38;
var_38 = LVI_INT(var_29.v.iv + var_37.v.iv);
var_30 = var_38;
var_29 = var_30;
} while (1);
*var_32 = var_29;
var_24 = var_25;
break;
}
case 1:
{
lval var_39;
lval var_40;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_40 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_41;
if (var_40.vt == LV_INT) {
var_41 = LVI_RAT(var_40.v.iv, 1);
} else {
var_41 = var_40;
}
var_39 = var_41;
lval var_42;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_42 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_42;
lval var_44;
lval var_45;
lval var_46;
var_46 = LVI_INT(0);
var_44 = var_46;
lptr var_47 = new_lv(VT_UNINIT);
var_47->v.pair.car = &(var_44); //  symbol.5
do {
lval var_48;
var_48 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_48)) {
break;
}
lval var_49;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_49 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_50;
if (var_49.vt == LV_INT) {
var_50 = LVI_RAT(var_49.v.iv, 1);
} else {
var_50 = var_49;
}
lval var_51;
{
lisp_int_t n, d, g;
n = var_39.v.irv.num * var_50.v.irv.den - var_50.v.irv.num * var_39.v.irv.den;
d = var_39.v.irv.den * var_50.v.irv.den;
g = lgcd(n, d);
n /= g;
d /= g;
var_51 = LVI_RAT(n, d);
}
var_39 = var_51;
lval var_52;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_52 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_52;
lval var_53;
var_53 = LVI_INT(1);
lval var_54;
var_54 = LVI_INT(var_44.v.iv + var_53.v.iv);
var_45 = var_54;
var_44 = var_45;
} while (1);
*var_47 = var_44;
lval var_55;
lval var_56;
var_56 = LVI_INT(var_39.v.irv.den);
lval var_57;
var_57 = LVI_INT(1);
lval var_58;
switch (TYPE_COMBO(var_56.vt,var_57.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_58 = LVI_BOOL(var_56.v.iv == var_57.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_58 = LVI_BOOL(var_56.v.iv * var_57.v.irv.den == var_57.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_58 = LVI_BOOL(var_56.v.iv == var_57.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_58 = LVI_BOOL(var_56.v.irv.num == var_57.v.iv * var_56.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_58 = LVI_BOOL(var_56.v.irv.num * var_57.v.irv.den == var_57.v.irv.num * var_56.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_58 = LVI_BOOL(var_56.v.irv.num == var_57.v.rv * var_56.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_58 = LVI_BOOL(var_56.v.rv == var_57.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_58 = LVI_BOOL(var_56.v.rv * var_57.v.irv.den == var_57.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_58 = LVI_BOOL(var_56.v.rv == var_57.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_58)) {
var_55 = var_39;
} else {
lval var_59;
var_59 = LVI_INT(var_39.v.irv.num);
var_55 = var_59;
}
var_24 = var_55;
break;
}
case 2:
{
lval var_60;
lval var_61;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_61 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_62;
if (var_61.vt == LV_INT) {
var_62 = LVI_REAL((lisp_real_t) var_61.v.iv);
} else if (var_61.vt == LV_RAT) {
var_62 = LVI_REAL(LV_RAT2REAL(var_61));
} else {
var_62 = var_61;
}
var_60 = var_62;
lval var_63;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_63 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_63;
lval var_65;
lval var_66;
lval var_67;
var_67 = LVI_INT(0);
var_65 = var_67;
lptr var_68 = new_lv(VT_UNINIT);
var_68->v.pair.car = &(var_65); //  symbol.6
do {
lval var_69;
var_69 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_69)) {
break;
}
lval var_70;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_70 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_71;
if (var_70.vt == LV_INT) {
var_71 = LVI_REAL((lisp_real_t) var_70.v.iv);
} else if (var_70.vt == LV_RAT) {
var_71 = LVI_REAL(LV_RAT2REAL(var_70));
} else {
var_71 = var_70;
}
lval var_72;
var_72 = LVI_REAL(var_60.v.rv - var_71.v.rv);
var_60 = var_72;
lval var_73;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_73 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_73;
lval var_74;
var_74 = LVI_INT(1);
lval var_75;
var_75 = LVI_INT(var_65.v.iv + var_74.v.iv);
var_66 = var_75;
var_65 = var_66;
} while (1);
*var_68 = var_65;
var_24 = var_60;
break;
}
case 3:
{
lval var_76;
lval var_77;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_77 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_78;
switch (var_77.vt) {
case LV_INT:
var_78 = LVI_CMPLX2((lisp_real_t) var_77.v.iv, 0);
break;
case LV_RAT:
var_78 = LVI_CMPLX2(LV_RAT2REAL(var_77), 0);
break;
case LV_REAL:
var_78 = LVI_CMPLX2(var_77.v.rv, 0);
break;
default:
var_78 = var_77;
break;
}
var_76 = var_78;
lval var_79;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_79 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_79;
lval var_81;
lval var_82;
lval var_83;
var_83 = LVI_INT(0);
var_81 = var_83;
lptr var_84 = new_lv(VT_UNINIT);
var_84->v.pair.car = &(var_81); //  symbol.7
do {
lval var_85;
var_85 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_85)) {
break;
}
lval var_86;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_86 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_87;
switch (var_86.vt) {
case LV_INT:
var_87 = LVI_CMPLX2((lisp_real_t) var_86.v.iv, 0);
break;
case LV_RAT:
var_87 = LVI_CMPLX2(LV_RAT2REAL(var_86), 0);
break;
case LV_REAL:
var_87 = LVI_CMPLX2(var_86.v.rv, 0);
break;
default:
var_87 = var_86;
break;
}
lval var_88;
var_88 = LVI_CMPLX1(var_76.v.cv - var_87.v.cv);
var_76 = var_88;
lval var_89;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_89 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_89;
lval var_90;
var_90 = LVI_INT(1);
lval var_91;
var_91 = LVI_INT(var_81.v.iv + var_90.v.iv);
var_82 = var_91;
var_81 = var_82;
} while (1);
*var_84 = var_81;
var_24 = var_76;
break;
}
default:
{
lval var_92;
var_92 = LVI_STRING("\'-\' got a non-numeric argument");
lval var_93;
{
lval var_94[1];
var_94[0] = var_92;
var_93 = wile_gen_list(1, var_94, NULL);
}
if (var_93.vt == LV_PAIR && (var_93.v.pair.cdr == NULL || var_93.v.pair.cdr->vt == LV_NIL)) {
var_93 = (var_93.v.pair.car ? *(var_93.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_93;
cachalot->l_whence = 0;
cachalot->c_whence = LISP_WHENCE;
longjmp(cachalot->cenv, 1);
var_24 = var_93;
break;
}
}
var_6 = var_24;
} else {
lval var_95;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_95 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
lval var_96;
switch (var_95.vt) {
case LV_INT:
var_96 = LVI_INT(-var_95.v.iv);
break;
case LV_RAT:
if (var_95.v.irv.den >= 0) {
var_96 = LVI_RAT(-var_95.v.irv.num, var_95.v.irv.den);
} else {
var_96 = LVI_RAT(var_95.v.irv.num, -var_95.v.irv.den);
}
break;
case LV_REAL:
var_96 = LVI_REAL(-var_95.v.rv);
break;
case LV_CMPLX:
var_96 = LVI_CMPLX2(-CREAL(var_95.v.cv), -CIMAG(var_95.v.cv));
break;
default:
WILE_EX("negative", "got a non-numeric argument");
}
var_6 = var_96;
}
var_4 = var_6;
} else {
lval var_97;
var_97 = LVI_INT(0);
var_4 = var_97;
}
return var_4;
}
// end of function wile_subtract
