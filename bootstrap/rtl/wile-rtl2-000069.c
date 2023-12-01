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

// @@@ (vector-sort! is-le? vec) @@@ bld-rtl-dir/wile-rtl2-000069.scm:15 @@@ wile_vector_sort_inplace @@@
lval wile_vector_sort_inplace(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
{
if (var_2[1].vt == LV_VECTOR) {
var_5 = LVI_INT(var_2[1].v.vec.capa);
} else if (var_2[1].vt == LV_BVECTOR) {
var_5 = LVI_INT(var_2[1].v.bvec.capa);
} else {
WILE_EX("vector-length", "input is not a vector");
}
}
var_4 = var_5;
lval var_6;
var_6 = var_4;
(void)
 LVI_INT(0);
lval var_10;
lval var_11;
lval var_12;
var_12 = LVI_INT(0);
var_10 = var_12;
lval var_9;
lptr var_13 = new_lv(VT_UNINIT);
var_13->v.pair.car = &(var_10); //  symbol.3
do {
lval var_14;
var_14 = LVI_BOOL(true);
do {
lval var_15;
switch (var_10.vt) {
case LV_REAL:
var_15 = LVI_BOOL(var_10.v.rv > 0.0);
break;
case LV_RAT:
var_15 = LVI_BOOL((var_10.v.irv.num > 0 && var_10.v.irv.den >= 0) || (var_10.v.irv.num < 0 && var_10.v.irv.den < 0));
break;
case LV_INT:
var_15 = LVI_BOOL(var_10.v.iv > 0);
break;
default:
WILE_EX("positive?", "expects a real-valued number");
}
var_14 = var_15;
if (LV_IS_FALSE(var_14)) { break; }
lval var_16;
var_16 = LVI_INT(1);
lval var_17;
switch (TYPE_COMBO(var_6.vt,var_16.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_17 = LVI_BOOL(var_6.v.iv > var_16.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_17 = LVI_BOOL(var_6.v.iv * var_16.v.irv.den > var_16.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_17 = LVI_BOOL(var_6.v.iv > var_16.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_17 = LVI_BOOL(var_6.v.irv.num > var_16.v.iv * var_6.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_17 = LVI_BOOL(var_6.v.irv.num * var_16.v.irv.den > var_16.v.irv.num * var_6.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_17 = LVI_BOOL(var_6.v.irv.num > var_16.v.rv * var_6.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_17 = LVI_BOOL(var_6.v.rv > var_16.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_17 = LVI_BOOL(var_6.v.rv * var_16.v.irv.den > var_16.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_17 = LVI_BOOL(var_6.v.rv > var_16.v.rv);
break;
default:
WILE_EX(">", "inputs are not real-valued numbers");
break;
}
lval var_18;
var_18 = LVI_BOOL(LV_IS_FALSE(var_17));
var_14 = var_18;
if (LV_IS_FALSE(var_14)) { break; }
} while (0);
if (!LV_IS_FALSE(var_14)) {
var_9 = var_10;
break;
}
lval var_19;
var_19 = LVI_INT(5);
lval var_20;
var_20 = LVI_INT(var_19.v.iv * var_6.v.iv);
lval var_21;
var_21 = LVI_INT(1);
lval var_22;
var_22 = LVI_INT(var_20.v.iv - var_21.v.iv);
lval var_23;
var_23 = LVI_INT(11);
lval var_24;
{
lisp_int_t nq, nr;
trunc_qr(var_22.v.iv, var_23.v.iv, &nq, &nr);
var_24 = LVI_INT(nq);
}
lval var_25;
var_25 = LVI_INT(1);
lval var_26;
{
lval var_28[2];
var_28[0] = var_24;
var_28[1] = var_25;
var_26 = wile_gen_list(2, var_28, NULL);
}
{
lval var_27[8];
var_27[0] = var_26;
// bld-rtl-dir/wile-rtl2-000069.scm:20
var_26 = wile_max(NULL, var_27, "bld-rtl-dir/wile-rtl2-000069.scm:20");
}
var_6 = var_26;
lval var_30;
lval var_31;
var_30 = var_6;
lptr var_32 = new_lv(VT_UNINIT);
var_32->v.pair.car = &(var_30); // j
do {
lval var_33;
switch (TYPE_COMBO(var_30.vt,var_4.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_33 = LVI_BOOL(var_30.v.iv >= var_4.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_33 = LVI_BOOL(var_30.v.iv * var_4.v.irv.den >= var_4.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_33 = LVI_BOOL(var_30.v.iv >= var_4.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_33 = LVI_BOOL(var_30.v.irv.num >= var_4.v.iv * var_30.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_33 = LVI_BOOL(var_30.v.irv.num * var_4.v.irv.den >= var_4.v.irv.num * var_30.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_33 = LVI_BOOL(var_30.v.irv.num >= var_4.v.rv * var_30.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_33 = LVI_BOOL(var_30.v.rv >= var_4.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_33 = LVI_BOOL(var_30.v.rv * var_4.v.irv.den >= var_4.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_33 = LVI_BOOL(var_30.v.rv >= var_4.v.rv);
break;
default:
WILE_EX(">=", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_33)) {
(void)
 var_2[1];
break;
}
lval var_34;
lval var_35;
{
if (var_2[1].vt != LV_VECTOR) {
WILE_EX("vector-ref", "input is not a vector");
}
if (var_30.vt != LV_INT || var_30.v.iv < 0 || (size_t) var_30.v.iv >= var_2[1].v.vec.capa) {
WILE_EX("vector-ref", "got bad index value");
}
var_35 = var_2[1].v.vec.arr[var_30.v.iv] ? *(var_2[1].v.vec.arr[var_30.v.iv]) : LVI_NIL();
}
var_34 = var_35;
lval var_37;
lval var_38;
lval var_39;
var_39 = LVI_INT(var_30.v.iv - var_6.v.iv);
var_37 = var_39;
lptr var_40 = new_lv(VT_UNINIT);
var_40->v.pair.car = &(var_37); // i
do {
lval var_41;
var_41 = LVI_BOOL(false);
do {
lval var_42;
switch (var_37.vt) {
case LV_REAL:
var_42 = LVI_BOOL(var_37.v.rv < 0.0);
break;
case LV_RAT:
var_42 = LVI_BOOL((var_37.v.irv.num < 0 && var_37.v.irv.den >= 0) || (var_37.v.irv.num > 0 && var_37.v.irv.den < 0));
break;
case LV_INT:
var_42 = LVI_BOOL(var_37.v.iv < 0);
break;
default:
WILE_EX("negative?", "expects a real-valued number");
}
var_41 = var_42;
if (!LV_IS_FALSE(var_41)) { break; }
lval var_43;
{
if (var_2[1].vt != LV_VECTOR) {
WILE_EX("vector-ref", "input is not a vector");
}
if (var_37.vt != LV_INT || var_37.v.iv < 0 || (size_t) var_37.v.iv >= var_2[1].v.vec.capa) {
WILE_EX("vector-ref", "got bad index value");
}
var_43 = var_2[1].v.vec.arr[var_37.v.iv] ? *(var_2[1].v.vec.arr[var_37.v.iv]) : LVI_NIL();
}
lval var_44;
{
lval var_45[2];
var_45[0] = var_43;
var_45[1] = var_34;
var_44 = wile_gen_list(2, var_45, NULL);
}
lval var_46;
{
lval var_47[2];
var_47[0] = var_2[0];
var_47[1] = var_44;
var_46 = wile_gen_list(2, var_47, NULL);
}
var_46 = wile_apply_function(&(var_46), LISP_WHENCE);
var_41 = var_46;
if (!LV_IS_FALSE(var_41)) { break; }
} while (0);
if (!LV_IS_FALSE(var_41)) {
lval var_48;
var_48 = LVI_INT(var_37.v.iv + var_6.v.iv);
{
if (var_2[1].vt != LV_VECTOR) {
WILE_EX("vector-set!", "input is not a vector");
}
if (var_48.vt != LV_INT || var_48.v.iv < 0 || (size_t) var_48.v.iv >= var_2[1].v.vec.capa) {
WILE_EX("vector-set!", "got bad index value");
}
var_2[1].v.vec.arr[var_48.v.iv] = new_lv(LV_NIL);
*(var_2[1].v.vec.arr[var_48.v.iv]) = var_34;
(void)
 var_2[1];
}
break;
}
lval var_50;
var_50 = LVI_INT(var_37.v.iv + var_6.v.iv);
lval var_51;
{
if (var_2[1].vt != LV_VECTOR) {
WILE_EX("vector-ref", "input is not a vector");
}
if (var_37.vt != LV_INT || var_37.v.iv < 0 || (size_t) var_37.v.iv >= var_2[1].v.vec.capa) {
WILE_EX("vector-ref", "got bad index value");
}
var_51 = var_2[1].v.vec.arr[var_37.v.iv] ? *(var_2[1].v.vec.arr[var_37.v.iv]) : LVI_NIL();
}
{
if (var_2[1].vt != LV_VECTOR) {
WILE_EX("vector-set!", "input is not a vector");
}
if (var_50.vt != LV_INT || var_50.v.iv < 0 || (size_t) var_50.v.iv >= var_2[1].v.vec.capa) {
WILE_EX("vector-set!", "got bad index value");
}
var_2[1].v.vec.arr[var_50.v.iv] = new_lv(LV_NIL);
*(var_2[1].v.vec.arr[var_50.v.iv]) = var_51;
(void)
 var_2[1];
}
lval var_53;
var_53 = LVI_INT(var_37.v.iv - var_6.v.iv);
var_38 = var_53;
var_37 = var_38;
} while (1);
*var_40 = var_37;
lval var_54;
var_54 = LVI_INT(1);
lval var_55;
var_55 = LVI_INT(var_30.v.iv + var_54.v.iv);
var_31 = var_55;
var_30 = var_31;
} while (1);
*var_32 = var_30;
lval var_56;
var_56 = LVI_INT(1);
lval var_57;
var_57 = LVI_INT(var_10.v.iv + var_56.v.iv);
var_11 = var_57;
var_10 = var_11;
} while (1);
*var_13 = var_10;
return var_9;
}
// end of function wile_vector_sort_inplace
