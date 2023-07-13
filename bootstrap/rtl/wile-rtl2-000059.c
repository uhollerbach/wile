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

// (day-of-year y m d)
lval wile_day_of_year(lptr* var_1, lptr var_2)
{
lval var_4;
var_4 = LVI_INT(0);
lval var_5;
var_5 = LVI_INT(31);
lval var_6;
var_6 = LVI_INT(59);
lval var_7;
var_7 = LVI_INT(90);
lval var_8;
var_8 = LVI_INT(120);
lval var_9;
var_9 = LVI_INT(151);
lval var_10;
var_10 = LVI_INT(181);
lval var_11;
var_11 = LVI_INT(212);
lval var_12;
var_12 = LVI_INT(243);
lval var_13;
var_13 = LVI_INT(273);
lval var_14;
var_14 = LVI_INT(304);
lval var_15;
var_15 = LVI_INT(334);
lval var_16;
{
lval vs[12];
vs[0] = var_4;
vs[1] = var_5;
vs[2] = var_6;
vs[3] = var_7;
vs[4] = var_8;
vs[5] = var_9;
vs[6] = var_10;
vs[7] = var_11;
vs[8] = var_12;
vs[9] = var_13;
vs[10] = var_14;
vs[11] = var_15;
var_16 = gen_list(12, vs, NULL);
}
{
lval vs[6];
vs[0] = var_16;
var_16 = wile_list2vector(NULL, vs);
}
lval var_17;
var_17 = LVI_INT(1);
lval var_18;
var_18 = LVI_INT(var_2[1].v.iv - var_17.v.iv);
lval var_19;
{
if (var_16.vt != LV_VECTOR) {
WILE_EX("vector-ref", "input is not a vector");
}
if (var_18.vt != LV_INT || var_18.v.iv < 0 || (size_t) var_18.v.iv >= var_16.v.vec.capa) {
WILE_EX("vector-ref", "got bad index value");
}
var_19 = var_16.v.vec.arr[var_18.v.iv] ? *(var_16.v.vec.arr[var_18.v.iv]) : LVI_NIL();
}
lval var_20;
lval var_21;
var_21 = LVI_BOOL(true);
do {
lval var_22;
var_22 = LVI_INT(2);
lval var_23;
switch (TYPE_COMBO(var_2[1].vt,var_22.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_23 = LVI_BOOL(var_2[1].v.iv > var_22.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_23 = LVI_BOOL(var_2[1].v.iv * var_22.v.irv.den > var_22.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_23 = LVI_BOOL(var_2[1].v.iv > var_22.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_23 = LVI_BOOL(var_2[1].v.irv.num > var_22.v.iv * var_2[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_23 = LVI_BOOL(var_2[1].v.irv.num * var_22.v.irv.den > var_22.v.irv.num * var_2[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_23 = LVI_BOOL(var_2[1].v.irv.num > var_22.v.rv * var_2[1].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_23 = LVI_BOOL(var_2[1].v.rv > var_22.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_23 = LVI_BOOL(var_2[1].v.rv * var_22.v.irv.den > var_22.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_23 = LVI_BOOL(var_2[1].v.rv > var_22.v.rv);
break;
default:
WILE_EX(">", "inputs are not real-valued numbers");
break;
}
var_21 = var_23;
if (LV_IS_FALSE(var_21)) { break; }
lval var_24;
{
lval vs[6];
vs[0] = var_2[0];
var_24 = wile_is_leap_year(NULL, vs);
}
var_21 = var_24;
if (LV_IS_FALSE(var_21)) { break; }
} while (0);
if (LV_IS_FALSE(var_21)) {
lval var_25;
var_25 = LVI_INT(0);
var_20 = var_25;
} else {
lval var_26;
var_26 = LVI_INT(1);
var_20 = var_26;
}
lval var_27;
var_27 = LVI_INT(var_19.v.iv + var_2[2].v.iv + var_20.v.iv);
return var_27;
}
// end of function wile_day_of_year
