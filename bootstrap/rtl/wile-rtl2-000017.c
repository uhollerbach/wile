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

// @@@ (list->vector lst) @@@ bld-rtl-dir/wile-rtl2-000017.scm:13 @@@ wile_list2vector @@@
lval wile_list2vector(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
{
lval var_6[8];
var_6[0] = var_2[0];
var_5 = wile_list_length(NULL, var_6, "bld-rtl-dir/wile-rtl2-000017.scm:14");
}
var_4 = var_5;
lval var_7;
lval var_8;
{
size_t i, capa;
if (var_4.vt != LV_INT || var_4.v.iv < 0) {
wile_exception("vector-create", "bld-rtl-dir/wile-rtl2-000017.scm:15", "expects a non-negative integer");
}
var_8.vt = LV_VECTOR;
var_8.origin = var_4.origin;
capa = var_4.v.iv;
var_8.v.vec.capa = capa;
var_8.v.vec.arr = LISP_ALLOC(lptr, (capa > 0 ? capa : 1));
for (i = 0; i < capa; ++i) {
var_8.v.vec.arr[i] = NULL;
}
}
var_7 = var_8;
lval var_10;
lval var_12;
lval var_14;
var_14 = LVI_INT(0);
var_10 = var_14;
lval var_11;
lval var_13;
var_11 = var_2[0];
lval var_9;
lptr var_15 = new_lv(VT_UNINIT);
var_15->v.pair.car = &(var_10);
lptr var_16 = new_lv(VT_UNINIT);
var_16->v.pair.car = &(var_11);
do {
lval var_17;
switch (TYPE_COMBO(var_10.vt,var_4.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_17 = LVI_BOOL(var_10.v.iv == var_4.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_17 = LVI_BOOL(var_10.v.iv * var_4.v.irv.den == var_4.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_17 = LVI_BOOL(var_10.v.iv == var_4.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_17 = LVI_BOOL(var_10.v.irv.num == var_4.v.iv * var_10.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_17 = LVI_BOOL(var_10.v.irv.num * var_4.v.irv.den == var_4.v.irv.num * var_10.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_17 = LVI_BOOL(var_10.v.irv.num == var_4.v.rv * var_10.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_17 = LVI_BOOL(var_10.v.rv == var_4.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_17 = LVI_BOOL(var_10.v.rv * var_4.v.irv.den == var_4.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_17 = LVI_BOOL(var_10.v.rv == var_4.v.rv);
break;
default:
wile_exception("==", "bld-rtl-dir/wile-rtl2-000017.scm:18", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_17)) {
var_9 = var_7;
break;
}
lval var_18;
if (var_11.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000017.scm:19", "input is not a pair!");
}
var_18 = (var_11.v.pair.car ? *(var_11.v.pair.car) : LVI_NIL());
{
if (var_7.vt != LV_VECTOR) {
wile_exception("vector-set!", "bld-rtl-dir/wile-rtl2-000017.scm:19", "input is not a vector");
}
if (var_10.vt != LV_INT || var_10.v.iv < 0 || (size_t) var_10.v.iv >= var_7.v.vec.capa) {
wile_exception("vector-set!", "bld-rtl-dir/wile-rtl2-000017.scm:19", "got bad index value");
}
var_7.v.vec.arr[var_10.v.iv] = new_lv(LV_NIL);
*(var_7.v.vec.arr[var_10.v.iv]) = var_18;
}
lval var_20;
var_20 = LVI_INT(1);
lval var_21;
var_21 = LVI_INT(var_10.v.iv + var_20.v.iv);
var_12 = var_21;
lval var_22;
if (var_11.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000017.scm:17", "input is not a pair!");
}
var_22 = (var_11.v.pair.cdr ? *(var_11.v.pair.cdr) : LVI_NIL());
var_13 = var_22;
var_10 = var_12;
var_11 = var_13;
} while (1);
*var_15 = var_10;
*var_16 = var_11;
return var_9;
}
// end of function wile_list2vector
