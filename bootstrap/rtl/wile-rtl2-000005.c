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

// (list-head lst n)
lval wile_list_head(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_BOOL(false);
do {
lval var_6;
var_6 = LVI_BOOL(var_2[0].vt == LV_NIL);
var_5 = var_6;
if (!LV_IS_FALSE(var_5)) { break; }
lval var_7;
switch (var_2[1].vt) {
case LV_REAL:
var_7 = LVI_BOOL(var_2[1].v.rv == 0.0);
break;
case LV_RAT:
var_7 = LVI_BOOL((var_2[1].v.irv.num == 0 && var_2[1].v.irv.den != 0));
break;
case LV_INT:
var_7 = LVI_BOOL(var_2[1].v.iv == 0);
break;
case LV_CMPLX:
var_7 = LVI_BOOL(CREAL(var_2[1].v.cv) == 0.0 && CIMAG(var_2[1].v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
var_5 = var_7;
if (!LV_IS_FALSE(var_5)) { break; }
lval var_8;
switch (var_2[1].vt) {
case LV_REAL:
var_8 = LVI_BOOL(var_2[1].v.rv < 0.0);
break;
case LV_RAT:
var_8 = LVI_BOOL((var_2[1].v.irv.num < 0 && var_2[1].v.irv.den >= 0) || (var_2[1].v.irv.num > 0 && var_2[1].v.irv.den < 0));
break;
case LV_INT:
var_8 = LVI_BOOL(var_2[1].v.iv < 0);
break;
default:
WILE_EX("negative?", "expects a real-valued number");
}
var_5 = var_8;
if (!LV_IS_FALSE(var_5)) { break; }
} while (0);
if (LV_IS_FALSE(var_5)) {
lval var_9;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_9 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
lval var_10;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_10 = (var_2[0].v.pair.cdr ? *(var_2[0].v.pair.cdr) : LVI_NIL());
lval var_11;
var_11 = LVI_INT(1);
lval var_12;
var_12 = LVI_INT(var_2[1].v.iv - var_11.v.iv);
lval var_13;
lval var_14[6];
var_14[0] = var_10;
var_14[1] = var_12;
var_13 = wile_list_head(NULL, var_14);
lval var_16;
{
lptr p1 = NULL, p2 = NULL;
if (var_9.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_9;
}
if (var_13.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_13;
}
var_16 = LVI_PAIR(p1, p2);
}
var_4 = var_16;
} else {
lval var_17;
var_17 = LVI_NIL();
var_4 = var_17;
}
return var_4;
}
// end of function wile_list_head
