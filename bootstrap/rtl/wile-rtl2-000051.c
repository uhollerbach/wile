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
static lval fn_4(lptr*, lptr);

// definitions

// @@@ lambda (s n acc) @@@ bld-rtl-dir/wile-rtl2-000051.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_5, lptr var_6)
{
lbl_7:;
lval var_8;
lval var_9;
switch (TYPE_COMBO(var_6[1].vt,var_6[0].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_9 = LVI_BOOL(var_6[1].v.iv < var_6[0].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_9 = LVI_BOOL(var_6[1].v.iv * var_6[0].v.irv.den < var_6[0].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_9 = LVI_BOOL(var_6[1].v.iv < var_6[0].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_9 = LVI_BOOL(var_6[1].v.irv.num < var_6[0].v.iv * var_6[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_9 = LVI_BOOL(var_6[1].v.irv.num * var_6[0].v.irv.den < var_6[0].v.irv.num * var_6[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_9 = LVI_BOOL(var_6[1].v.irv.num < var_6[0].v.rv * var_6[1].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_9 = LVI_BOOL(var_6[1].v.rv < var_6[0].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_9 = LVI_BOOL(var_6[1].v.rv * var_6[0].v.irv.den < var_6[0].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_9 = LVI_BOOL(var_6[1].v.rv < var_6[0].v.rv);
break;
default:
WILE_EX("<", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_9)) {
lval var_10;
var_10 = LVI_INT(1);
lval var_11;
var_11 = LVI_INT(var_6[1].v.iv - var_10.v.iv);
lval var_12;
{
lptr p1 = NULL, p2 = NULL;
if (var_6[1].vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_6[1];
}
if (var_6[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6[2];
}
var_12 = LVI_PAIR(p1, p2);
}
lval var_15[8];
var_15[0] = var_6[0];
var_15[1] = var_11;
var_15[2] = var_12;
var_6[0] = var_15[0];
var_6[1] = var_15[1];
var_6[2] = var_15[2];
goto lbl_7;	// selfie
} else {
var_8 = var_6[2];
}
return var_8;
}
// end of lambda fn_4

// @@@ (upfrom s n0) @@@ bld-rtl-dir/wile-rtl2-000051.scm:13 @@@ wile_upfrom @@@
lval wile_upfrom(lptr* var_1, lptr var_2)
{
MK_CLOS(var_5,0);
lval var_16;
var_16 = LVI_INT(var_2[0].v.iv + var_2[1].v.iv);
lval var_17;
var_17 = LVI_INT(1);
lval var_18;
var_18 = LVI_INT(var_16.v.iv - var_17.v.iv);
lval var_19;
var_19 = LVI_NIL();
lval var_20;
lval var_21[8];
var_21[0] = var_2[0];
var_21[1] = var_18;
var_21[2] = var_19;
var_20 = fn_4(var_5, var_21);
return var_20;
}
// end of function wile_upfrom
