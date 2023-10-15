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
static lval fn_8(lptr*, lptr);
static lval fn_25(lptr*, lptr);

// definitions

// @@@ lambda (f n acc) @@@ bld-rtl-dir/wile-rtl2-000045.scm:15 @@@ fn_8 @@@
static lval fn_8(lptr* var_9, lptr var_10)
{
lbl_11:;
lval var_12;
lval var_13;
switch (TYPE_COMBO(var_10[1].vt,var_10[0].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_13 = LVI_BOOL(var_10[1].v.iv < var_10[0].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_13 = LVI_BOOL(var_10[1].v.iv * var_10[0].v.irv.den < var_10[0].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_13 = LVI_BOOL(var_10[1].v.iv < var_10[0].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_13 = LVI_BOOL(var_10[1].v.irv.num < var_10[0].v.iv * var_10[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_13 = LVI_BOOL(var_10[1].v.irv.num * var_10[0].v.irv.den < var_10[0].v.irv.num * var_10[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_13 = LVI_BOOL(var_10[1].v.irv.num < var_10[0].v.rv * var_10[1].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_13 = LVI_BOOL(var_10[1].v.rv < var_10[0].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_13 = LVI_BOOL(var_10[1].v.rv * var_10[0].v.irv.den < var_10[0].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_13 = LVI_BOOL(var_10[1].v.rv < var_10[0].v.rv);
break;
default:
WILE_EX("<", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_13)) {
lval var_14;
var_14 = LVI_INT(1);
lval var_15;
var_15 = LVI_INT(var_10[1].v.iv - var_14.v.iv);
lval var_16;
{
lptr p1 = NULL, p2 = NULL;
if (var_10[1].vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_10[1];
}
if (var_10[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_10[2];
}
var_16 = LVI_PAIR(p1, p2);
}
lval var_19[8];
var_19[0] = var_10[0];
var_19[1] = var_15;
var_19[2] = var_16;
var_10[0] = var_19[0];
var_10[1] = var_19[1];
var_10[2] = var_19[2];
goto lbl_11;	// selfie
} else {
var_12 = var_10[2];
}
return var_12;
}
// end of lambda fn_8

// @@@ lambda (f n acc) @@@ bld-rtl-dir/wile-rtl2-000045.scm:17 @@@ fn_25 @@@
static lval fn_25(lptr* var_26, lptr var_27)
{
lbl_28:;
lval var_29;
lval var_30;
switch (TYPE_COMBO(var_27[1].vt,var_27[0].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_30 = LVI_BOOL(var_27[1].v.iv > var_27[0].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_30 = LVI_BOOL(var_27[1].v.iv * var_27[0].v.irv.den > var_27[0].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_30 = LVI_BOOL(var_27[1].v.iv > var_27[0].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_30 = LVI_BOOL(var_27[1].v.irv.num > var_27[0].v.iv * var_27[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_30 = LVI_BOOL(var_27[1].v.irv.num * var_27[0].v.irv.den > var_27[0].v.irv.num * var_27[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_30 = LVI_BOOL(var_27[1].v.irv.num > var_27[0].v.rv * var_27[1].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_30 = LVI_BOOL(var_27[1].v.rv > var_27[0].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_30 = LVI_BOOL(var_27[1].v.rv * var_27[0].v.irv.den > var_27[0].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_30 = LVI_BOOL(var_27[1].v.rv > var_27[0].v.rv);
break;
default:
WILE_EX(">", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_30)) {
lval var_31;
var_31 = LVI_INT(1);
lval var_32;
var_32 = LVI_INT(var_27[1].v.iv + var_31.v.iv);
lval var_33;
{
lptr p1 = NULL, p2 = NULL;
if (var_27[1].vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_27[1];
}
if (var_27[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_27[2];
}
var_33 = LVI_PAIR(p1, p2);
}
lval var_36[8];
var_36[0] = var_27[0];
var_36[1] = var_32;
var_36[2] = var_33;
var_27[0] = var_36[0];
var_27[1] = var_36[1];
var_27[2] = var_36[2];
goto lbl_28;	// selfie
} else {
var_29 = var_27[2];
}
return var_29;
}
// end of lambda fn_25

// @@@ (fromto f l) @@@ bld-rtl-dir/wile-rtl2-000045.scm:13 @@@ wile_fromto @@@
lval wile_fromto(lptr* var_1, lptr var_2)
{
lval var_4;
do {
lval var_5;
switch (TYPE_COMBO(var_2[0].vt,var_2[1].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_5 = LVI_BOOL(var_2[0].v.iv == var_2[1].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_5 = LVI_BOOL(var_2[0].v.iv * var_2[1].v.irv.den == var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_5 = LVI_BOOL(var_2[0].v.iv == var_2[1].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_5 = LVI_BOOL(var_2[0].v.irv.num == var_2[1].v.iv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_5 = LVI_BOOL(var_2[0].v.irv.num * var_2[1].v.irv.den == var_2[1].v.irv.num * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_5 = LVI_BOOL(var_2[0].v.irv.num == var_2[1].v.rv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_5 = LVI_BOOL(var_2[0].v.rv == var_2[1].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_5 = LVI_BOOL(var_2[0].v.rv * var_2[1].v.irv.den == var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_5 = LVI_BOOL(var_2[0].v.rv == var_2[1].v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_5)) {
lval var_6;
{
lval vs[1];
vs[0] = var_2[0];
var_6 = gen_list(1, vs, NULL);
}
var_4 = var_6;
break;
}
lval var_7;
switch (TYPE_COMBO(var_2[0].vt,var_2[1].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_7 = LVI_BOOL(var_2[0].v.iv < var_2[1].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_7 = LVI_BOOL(var_2[0].v.iv * var_2[1].v.irv.den < var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_7 = LVI_BOOL(var_2[0].v.iv < var_2[1].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_7 = LVI_BOOL(var_2[0].v.irv.num < var_2[1].v.iv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_7 = LVI_BOOL(var_2[0].v.irv.num * var_2[1].v.irv.den < var_2[1].v.irv.num * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_7 = LVI_BOOL(var_2[0].v.irv.num < var_2[1].v.rv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_7 = LVI_BOOL(var_2[0].v.rv < var_2[1].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_7 = LVI_BOOL(var_2[0].v.rv * var_2[1].v.irv.den < var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_7 = LVI_BOOL(var_2[0].v.rv < var_2[1].v.rv);
break;
default:
WILE_EX("<", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_7)) {
MK_CLOS(var_9,0);
lval var_20;
var_20 = LVI_NIL();
lval var_21;
lval var_22[8];
var_22[0] = var_2[0];
var_22[1] = var_2[1];
var_22[2] = var_20;
var_21 = fn_8(var_9, var_22);
var_4 = var_21;
break;
}
lval var_24;
switch (TYPE_COMBO(var_2[0].vt,var_2[1].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_24 = LVI_BOOL(var_2[0].v.iv > var_2[1].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_24 = LVI_BOOL(var_2[0].v.iv * var_2[1].v.irv.den > var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_24 = LVI_BOOL(var_2[0].v.iv > var_2[1].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_24 = LVI_BOOL(var_2[0].v.irv.num > var_2[1].v.iv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_24 = LVI_BOOL(var_2[0].v.irv.num * var_2[1].v.irv.den > var_2[1].v.irv.num * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_24 = LVI_BOOL(var_2[0].v.irv.num > var_2[1].v.rv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_24 = LVI_BOOL(var_2[0].v.rv > var_2[1].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_24 = LVI_BOOL(var_2[0].v.rv * var_2[1].v.irv.den > var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_24 = LVI_BOOL(var_2[0].v.rv > var_2[1].v.rv);
break;
default:
WILE_EX(">", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_24)) {
MK_CLOS(var_26,0);
lval var_37;
var_37 = LVI_NIL();
lval var_38;
lval var_39[8];
var_39[0] = var_2[0];
var_39[1] = var_2[1];
var_39[2] = var_37;
var_38 = fn_25(var_26, var_39);
var_4 = var_38;
break;
}
var_4 = LVI_BOOL(false);
} while (0);
return var_4;
}
// end of function wile_fromto
