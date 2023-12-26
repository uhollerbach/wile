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

// @@@ (vector-map! proc vec) @@@ bld-rtl-dir/wile-rtl2-000071.scm:13 @@@ wile_vector_map_inplace @@@
lval wile_vector_map_inplace(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
{
if (var_2[1].vt == LV_VECTOR) {
var_5 = LVI_INT(var_2[1].v.vec.capa);
} else if (var_2[1].vt == LV_BVECTOR) {
var_5 = LVI_INT(var_2[1].v.bvec.capa);
} else {
wile_exception("vector-length", "bld-rtl-dir/wile-rtl2-000071.scm:14", "input is not a vector");
}
}
var_4 = var_5;
lval var_7;
lval var_8;
lval var_9;
var_9 = LVI_INT(0);
var_7 = var_9;
lval var_6;
lptr var_10 = new_lv(VT_UNINIT);
var_10->v.pair.car = &(var_7); // i
do {
lval var_11;
switch (TYPE_COMBO(var_7.vt,var_4.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_11 = LVI_BOOL(var_7.v.iv == var_4.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_11 = LVI_BOOL(var_7.v.iv * var_4.v.irv.den == var_4.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_11 = LVI_BOOL(var_7.v.iv == var_4.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_11 = LVI_BOOL(var_7.v.irv.num == var_4.v.iv * var_7.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_11 = LVI_BOOL(var_7.v.irv.num * var_4.v.irv.den == var_4.v.irv.num * var_7.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_11 = LVI_BOOL(var_7.v.irv.num == var_4.v.rv * var_7.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_11 = LVI_BOOL(var_7.v.rv == var_4.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_11 = LVI_BOOL(var_7.v.rv * var_4.v.irv.den == var_4.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_11 = LVI_BOOL(var_7.v.rv == var_4.v.rv);
break;
default:
wile_exception("==", "bld-rtl-dir/wile-rtl2-000071.scm:16", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_11)) {
var_6 = var_2[1];
break;
}
lval var_12;
{
if (var_2[1].vt != LV_VECTOR) {
wile_exception("vector-ref", "bld-rtl-dir/wile-rtl2-000071.scm:17", "input is not a vector");
}
if (var_7.vt != LV_INT || var_7.v.iv < 0 || (size_t) var_7.v.iv >= var_2[1].v.vec.capa) {
wile_exception("vector-ref", "bld-rtl-dir/wile-rtl2-000071.scm:17", "got bad index value");
}
var_12 = var_2[1].v.vec.arr[var_7.v.iv] ? *(var_2[1].v.vec.arr[var_7.v.iv]) : LVI_NIL();
}
lval var_13;
{
lval var_14[1];
var_14[0] = var_12;
var_13 = wile_gen_list(1, var_14, NULL);
}
lval var_15;
{
lval var_16[2];
var_16[0] = var_2[0];
var_16[1] = var_13;
var_15 = wile_gen_list(2, var_16, NULL);
}
var_15 = wile_apply_function(&(var_15), LISP_WHENCE);
{
if (var_2[1].vt != LV_VECTOR) {
wile_exception("vector-set!", "bld-rtl-dir/wile-rtl2-000071.scm:17", "input is not a vector");
}
if (var_7.vt != LV_INT || var_7.v.iv < 0 || (size_t) var_7.v.iv >= var_2[1].v.vec.capa) {
wile_exception("vector-set!", "bld-rtl-dir/wile-rtl2-000071.scm:17", "got bad index value");
}
var_2[1].v.vec.arr[var_7.v.iv] = new_lv(LV_NIL);
*(var_2[1].v.vec.arr[var_7.v.iv]) = var_15;
(void)
 var_2[1];
}
lval var_18;
var_18 = LVI_INT(1);
lval var_19;
{
lval var_21[2];
var_21[0] = var_7;
var_21[1] = var_18;
var_19 = wile_gen_list(2, var_21, NULL);
}
{
lval var_20[8];
var_20[0] = var_19;
// bld-rtl-dir/wile-rtl2-000071.scm:15
var_19 = wile_add(NULL, var_20, "bld-rtl-dir/wile-rtl2-000071.scm:15");
}
var_8 = var_19;
var_7 = var_8;
} while (1);
*var_10 = var_7;
return var_6;
}
// end of function wile_vector_map_inplace
