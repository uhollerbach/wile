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

// @@@ (vector->list vec) @@@ bld-rtl-dir/wile-rtl2-000020.scm:13 @@@ wile_vector2list @@@
lval wile_vector2list(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
{
if (var_2[0].vt == LV_VECTOR) {
var_5 = LVI_INT(var_2[0].v.vec.capa);
} else if (var_2[0].vt == LV_BVECTOR) {
var_5 = LVI_INT(var_2[0].v.bvec.capa);
} else {
WILE_EX("vector-length", "input is not a vector");
}
}
var_4 = var_5;
lval var_6;
lval var_7;
var_7 = LVI_NIL();
var_6 = var_7;
lval var_9;
lval var_10;
lval var_11;
var_11 = LVI_INT(0);
var_9 = var_11;
lptr var_12 = new_lv(VT_UNINIT);
var_12->v.pair.car = &(var_9); //  symbol.3
do {
lval var_13;
switch (var_4.vt) {
case LV_REAL:
var_13 = LVI_BOOL(var_4.v.rv == 0.0);
break;
case LV_RAT:
var_13 = LVI_BOOL((var_4.v.irv.num == 0 && var_4.v.irv.den != 0));
break;
case LV_INT:
var_13 = LVI_BOOL(var_4.v.iv == 0);
break;
case LV_CMPLX:
var_13 = LVI_BOOL(CREAL(var_4.v.cv) == 0.0 && CIMAG(var_4.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_13)) {
break;
}
lval var_14;
var_14 = LVI_INT(1);
lval var_15;
var_15 = LVI_INT(var_4.v.iv - var_14.v.iv);
var_4 = var_15;
lval var_16;
{
if (var_2[0].vt != LV_VECTOR) {
WILE_EX("vector-ref", "input is not a vector");
}
if (var_4.vt != LV_INT || var_4.v.iv < 0 || (size_t) var_4.v.iv >= var_2[0].v.vec.capa) {
WILE_EX("vector-ref", "got bad index value");
}
var_16 = var_2[0].v.vec.arr[var_4.v.iv] ? *(var_2[0].v.vec.arr[var_4.v.iv]) : LVI_NIL();
}
lval var_17;
{
lptr p1 = NULL, p2 = NULL;
if (var_16.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_16;
}
if (var_6.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6;
}
var_17 = LVI_PAIR(p1, p2);
}
var_6 = var_17;
lval var_18;
var_18 = LVI_INT(1);
lval var_19;
var_19 = LVI_INT(var_9.v.iv + var_18.v.iv);
var_10 = var_19;
var_9 = var_10;
} while (1);
*var_12 = var_9;
return var_6;
}
// end of function wile_vector2list
