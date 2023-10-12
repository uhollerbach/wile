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

// @@@ (vector-map! proc vec) @@@ bld-rtl-dir/wile-rtl2-000066.scm:13 @@@ wile_vector_map_inplace @@@
lval wile_vector_map_inplace(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
{
if (var_2[1].vt != LV_VECTOR) {
WILE_EX("vector-length", "input is not a vector");
}
var_5 = LVI_INT(var_2[1].v.vec.capa);
}
var_4 = var_5;
lval var_7;
lval var_8;
lval var_9;
var_9 = LVI_INT(0);
var_7 = var_9;
lval var_6;
do {
lval var_10;
switch (TYPE_COMBO(var_7.vt,var_4.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_10 = LVI_BOOL(var_7.v.iv == var_4.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_10 = LVI_BOOL(var_7.v.iv * var_4.v.irv.den == var_4.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_10 = LVI_BOOL(var_7.v.iv == var_4.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_10 = LVI_BOOL(var_7.v.irv.num == var_4.v.iv * var_7.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_10 = LVI_BOOL(var_7.v.irv.num * var_4.v.irv.den == var_4.v.irv.num * var_7.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_10 = LVI_BOOL(var_7.v.irv.num == var_4.v.rv * var_7.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_10 = LVI_BOOL(var_7.v.rv == var_4.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_10 = LVI_BOOL(var_7.v.rv * var_4.v.irv.den == var_4.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_10 = LVI_BOOL(var_7.v.rv == var_4.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_10)) {
var_6 = var_2[1];
break;
}
lval var_11;
{
if (var_2[1].vt != LV_VECTOR) {
WILE_EX("vector-ref", "input is not a vector");
}
if (var_7.vt != LV_INT || var_7.v.iv < 0 || (size_t) var_7.v.iv >= var_2[1].v.vec.capa) {
WILE_EX("vector-ref", "got bad index value");
}
var_11 = var_2[1].v.vec.arr[var_7.v.iv] ? *(var_2[1].v.vec.arr[var_7.v.iv]) : LVI_NIL();
}
lval var_12;
{
lval vs[1];
vs[0] = var_11;
var_12 = gen_list(1, vs, NULL);
}
lval var_13;
{
lval vs[2];
vs[0] = var_2[0];
vs[1] = var_12;
var_13 = gen_list(2, vs, NULL);
}
var_13 = wile_apply_function(&(var_13), __FILE__, __LINE__);
{
if (var_2[1].vt != LV_VECTOR) {
WILE_EX("vector-set!", "input is not a vector");
}
if (var_7.vt != LV_INT || var_7.v.iv < 0 || (size_t) var_7.v.iv >= var_2[1].v.vec.capa) {
WILE_EX("vector-set!", "got bad index value");
}
var_2[1].v.vec.arr[var_7.v.iv] = new_lv(LV_NIL);
*(var_2[1].v.vec.arr[var_7.v.iv]) = var_13;
(void)
 var_2[1];
}
lval var_15;
var_15 = LVI_INT(1);
lval var_16;
{
lval vs[2];
vs[0] = var_7;
vs[1] = var_15;
var_16 = gen_list(2, vs, NULL);
}
{
lval vs[8];
vs[0] = var_16;
var_16 = wile_add(NULL, vs);
}
var_8 = var_16;
var_7 = var_8;
} while (1);
return var_6;
}
// end of function wile_vector_map_inplace
