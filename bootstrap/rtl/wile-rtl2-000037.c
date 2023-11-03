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

// @@@ (replicate c n) @@@ bld-rtl-dir/wile-rtl2-000037.scm:13 @@@ wile_replicate @@@
lval wile_replicate(lptr* var_1, lptr var_2)
{
lval var_5;
lval var_7;
lval var_9;
var_9 = LVI_INT(0);
var_5 = var_9;
lval var_6;
lval var_8;
lval var_10;
var_10 = LVI_NIL();
var_6 = var_10;
lval var_4;
do {
lval var_11;
switch (TYPE_COMBO(var_5.vt,var_2[1].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_11 = LVI_BOOL(var_5.v.iv >= var_2[1].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_11 = LVI_BOOL(var_5.v.iv * var_2[1].v.irv.den >= var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_11 = LVI_BOOL(var_5.v.iv >= var_2[1].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_11 = LVI_BOOL(var_5.v.irv.num >= var_2[1].v.iv * var_5.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_11 = LVI_BOOL(var_5.v.irv.num * var_2[1].v.irv.den >= var_2[1].v.irv.num * var_5.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_11 = LVI_BOOL(var_5.v.irv.num >= var_2[1].v.rv * var_5.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_11 = LVI_BOOL(var_5.v.rv >= var_2[1].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_11 = LVI_BOOL(var_5.v.rv * var_2[1].v.irv.den >= var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_11 = LVI_BOOL(var_5.v.rv >= var_2[1].v.rv);
break;
default:
WILE_EX(">=", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_11)) {
var_4 = var_6;
break;
}
lval var_12;
var_12 = LVI_INT(1);
lval var_13;
var_13 = LVI_INT(var_5.v.iv + var_12.v.iv);
var_7 = var_13;
lval var_14;
{
lptr p1 = NULL, p2 = NULL;
if (var_2[0].vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_2[0];
}
if (var_6.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6;
}
var_14 = LVI_PAIR(p1, p2);
}
var_8 = var_14;
var_5 = var_7;
var_6 = var_8;
} while (1);
return var_4;
}
// end of function wile_replicate
