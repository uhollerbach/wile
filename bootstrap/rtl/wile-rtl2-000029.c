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

// (/ . vs)
lval wile_divide(lptr* var_1, lptr var_2)
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
lval var_26;
if (var_25.vt == LV_INT) {
var_26 = LVI_RAT(var_25.v.iv, 1);
} else {
var_26 = var_25;
}
var_24 = var_26;
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
do {
lval var_32;
var_32 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_32)) {
break;
}
lval var_33;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_33 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_34;
if (var_33.vt == LV_INT) {
var_34 = LVI_RAT(var_33.v.iv, 1);
} else {
var_34 = var_33;
}
lval var_35;
{
lisp_int_t n, d, g;
n = var_24.v.irv.num * var_34.v.irv.den;
d = var_24.v.irv.den * var_34.v.irv.num;
g = lgcd(n, d);
if (d < 0) {
g = -g;
}
n /= g;
d /= g;
var_35 = LVI_RAT(n, d);
}
var_24 = var_35;
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
lval var_39;
lval var_40;
var_40 = LVI_INT(var_24.v.irv.den);
lval var_41;
var_41 = LVI_INT(1);
lval var_42;
switch (TYPE_COMBO(var_40.vt,var_41.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_42 = LVI_BOOL(var_40.v.iv == var_41.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_42 = LVI_BOOL(var_40.v.iv * var_41.v.irv.den == var_41.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_42 = LVI_BOOL(var_40.v.iv == var_41.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_42 = LVI_BOOL(var_40.v.irv.num == var_41.v.iv * var_40.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_42 = LVI_BOOL(var_40.v.irv.num * var_41.v.irv.den == var_41.v.irv.num * var_40.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_42 = LVI_BOOL(var_40.v.irv.num == var_41.v.rv * var_40.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_42 = LVI_BOOL(var_40.v.rv == var_41.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_42 = LVI_BOOL(var_40.v.rv * var_41.v.irv.den == var_41.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_42 = LVI_BOOL(var_40.v.rv == var_41.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_42)) {
var_39 = var_24;
} else {
lval var_43;
var_43 = LVI_INT(var_24.v.irv.num);
var_39 = var_43;
}
var_23 = var_39;
}
break;
case 1:
{
lval var_44;
lval var_45;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_45 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_46;
if (var_45.vt == LV_INT) {
var_46 = LVI_RAT(var_45.v.iv, 1);
} else {
var_46 = var_45;
}
var_44 = var_46;
lval var_47;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_47 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_47;
lval var_49;
lval var_50;
lval var_51;
var_51 = LVI_INT(0);
var_49 = var_51;
do {
lval var_52;
var_52 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_52)) {
break;
}
lval var_53;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_53 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_54;
if (var_53.vt == LV_INT) {
var_54 = LVI_RAT(var_53.v.iv, 1);
} else {
var_54 = var_53;
}
lval var_55;
{
lisp_int_t n, d, g;
n = var_44.v.irv.num * var_54.v.irv.den;
d = var_44.v.irv.den * var_54.v.irv.num;
g = lgcd(n, d);
if (d < 0) {
g = -g;
}
n /= g;
d /= g;
var_55 = LVI_RAT(n, d);
}
var_44 = var_55;
lval var_56;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_56 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_56;
lval var_57;
var_57 = LVI_INT(1);
lval var_58;
var_58 = LVI_INT(var_49.v.iv + var_57.v.iv);
var_50 = var_58;
var_49 = var_50;
} while (1);
lval var_59;
lval var_60;
var_60 = LVI_INT(var_44.v.irv.den);
lval var_61;
var_61 = LVI_INT(1);
lval var_62;
switch (TYPE_COMBO(var_60.vt,var_61.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_62 = LVI_BOOL(var_60.v.iv == var_61.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_62 = LVI_BOOL(var_60.v.iv * var_61.v.irv.den == var_61.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_62 = LVI_BOOL(var_60.v.iv == var_61.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_62 = LVI_BOOL(var_60.v.irv.num == var_61.v.iv * var_60.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_62 = LVI_BOOL(var_60.v.irv.num * var_61.v.irv.den == var_61.v.irv.num * var_60.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_62 = LVI_BOOL(var_60.v.irv.num == var_61.v.rv * var_60.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_62 = LVI_BOOL(var_60.v.rv == var_61.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_62 = LVI_BOOL(var_60.v.rv * var_61.v.irv.den == var_61.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_62 = LVI_BOOL(var_60.v.rv == var_61.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_62)) {
var_59 = var_44;
} else {
lval var_63;
var_63 = LVI_INT(var_44.v.irv.num);
var_59 = var_63;
}
var_23 = var_59;
}
break;
case 2:
{
lval var_64;
lval var_65;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_65 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_66;
if (var_65.vt == LV_INT) {
var_66 = LVI_REAL((lisp_real_t) var_65.v.iv);
} else if (var_65.vt == LV_RAT) {
var_66 = LVI_REAL(LV_RAT2REAL(var_65));
} else {
var_66 = var_65;
}
var_64 = var_66;
lval var_67;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_67 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_67;
lval var_69;
lval var_70;
lval var_71;
var_71 = LVI_INT(0);
var_69 = var_71;
do {
lval var_72;
var_72 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_72)) {
break;
}
lval var_73;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_73 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_74;
if (var_73.vt == LV_INT) {
var_74 = LVI_REAL((lisp_real_t) var_73.v.iv);
} else if (var_73.vt == LV_RAT) {
var_74 = LVI_REAL(LV_RAT2REAL(var_73));
} else {
var_74 = var_73;
}
lval var_75;
var_75 = LVI_REAL(var_64.v.rv / var_74.v.rv);
var_64 = var_75;
lval var_76;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_76 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_76;
lval var_77;
var_77 = LVI_INT(1);
lval var_78;
var_78 = LVI_INT(var_69.v.iv + var_77.v.iv);
var_70 = var_78;
var_69 = var_70;
} while (1);
var_23 = var_64;
}
break;
case 3:
{
lval var_79;
lval var_80;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_80 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_81;
switch (var_80.vt) {
case LV_INT:
var_81 = LVI_CMPLX2((lisp_real_t) var_80.v.iv, 0);
break;
case LV_RAT:
var_81 = LVI_CMPLX2(LV_RAT2REAL(var_80), 0);
break;
case LV_REAL:
var_81 = LVI_CMPLX2(var_80.v.rv, 0);
break;
default:
var_81 = var_80;
break;
}
var_79 = var_81;
lval var_82;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_82 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_82;
lval var_84;
lval var_85;
lval var_86;
var_86 = LVI_INT(0);
var_84 = var_86;
do {
lval var_87;
var_87 = LVI_BOOL(var_11.vt == LV_NIL);
if (!LV_IS_FALSE(var_87)) {
break;
}
lval var_88;
if (var_11.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_88 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
lval var_89;
switch (var_88.vt) {
case LV_INT:
var_89 = LVI_CMPLX2((lisp_real_t) var_88.v.iv, 0);
break;
case LV_RAT:
var_89 = LVI_CMPLX2(LV_RAT2REAL(var_88), 0);
break;
case LV_REAL:
var_89 = LVI_CMPLX2(var_88.v.rv, 0);
break;
default:
var_89 = var_88;
break;
}
lval var_90;
var_90 = LVI_CMPLX1(var_79.v.cv / var_89.v.cv);
var_79 = var_90;
lval var_91;
if (var_11.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_91 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_11 = var_91;
lval var_92;
var_92 = LVI_INT(1);
lval var_93;
var_93 = LVI_INT(var_84.v.iv + var_92.v.iv);
var_85 = var_93;
var_84 = var_85;
} while (1);
var_23 = var_79;
}
break;
default:
{
lval var_94;
var_94 = LVI_STRING("\'/\' got a non-numeric argument");
lval var_95;
{
lval vs[1];
vs[0] = var_94;
var_95 = gen_list(1, vs, NULL);
}
if (var_95.vt == LV_PAIR && (var_95.v.pair.cdr == NULL || var_95.v.pair.cdr->vt == LV_NIL)) {
var_95 = (var_95.v.pair.car ? *(var_95.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_95;
cachalot->l_whence = 0;
cachalot->c_whence = LISP_WHENCE;
longjmp(cachalot->cenv, 1);
var_23 = var_95;
}
break;
}
var_6 = var_23;
} else {
lval var_96;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_96 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
lval var_97;
switch (var_96.vt) {
case LV_INT:
if (var_96.v.iv < 0) {
var_97 = LVI_RAT(-1, -var_96.v.iv);
} else {
var_97 = LVI_RAT(1, var_96.v.iv);
}
break;
case LV_RAT:
if (var_96.v.irv.num < 0) {
var_97 = LVI_RAT(-var_96.v.irv.den, -var_96.v.irv.num);
} else {
var_97 = LVI_RAT(var_96.v.irv.den, var_96.v.irv.num);
}
break;
case LV_REAL:
var_97 = LVI_REAL(1.0/var_96.v.rv);
break;
case LV_CMPLX:
var_97 = LVI_CMPLX1(1.0/var_96.v.cv);
break;
default:
WILE_EX("reciprocal", "got a non-numeric argument");
}
var_6 = var_97;
}
var_4 = var_6;
} else {
lval var_98;
var_98 = LVI_INT(1);
var_4 = var_98;
}
return var_4;
}
// end of function wile_divide
