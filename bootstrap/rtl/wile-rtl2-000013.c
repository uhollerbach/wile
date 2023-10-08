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

// @@@ (list->bytevector lst) @@@ bld-rtl-dir/wile-rtl2-000013.scm:12 @@@ wile_list2bytevector @@@
lval wile_list2bytevector(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
{
lval vs[8];
vs[0] = var_2[0];
var_5 = wile_list_length(NULL, vs);
}
var_4 = var_5;
lval var_6;
lval var_7;
{
size_t i, capa;
var_7.vt = LV_BVECTOR;
capa = var_4.v.iv;
var_7.v.bvec.capa = capa;
var_7.v.bvec.arr = LISP_ALLOC(unsigned char, (capa > 0 ? capa : 1));
LISP_ASSERT(var_7.v.bvec.arr != NULL);
for (i = 0; i < capa; ++i) {
var_7.v.bvec.arr[i] = 0;
}
}
var_6 = var_7;
lval var_9;
lval var_11;
lval var_13;
var_13 = LVI_INT(0);
var_9 = var_13;
lval var_10;
lval var_12;
var_10 = var_2[0];
lval var_8;
do {
lval var_14;
switch (TYPE_COMBO(var_9.vt,var_4.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_14 = LVI_BOOL(var_9.v.iv == var_4.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_14 = LVI_BOOL(var_9.v.iv * var_4.v.irv.den == var_4.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_14 = LVI_BOOL(var_9.v.iv == var_4.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_14 = LVI_BOOL(var_9.v.irv.num == var_4.v.iv * var_9.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_14 = LVI_BOOL(var_9.v.irv.num * var_4.v.irv.den == var_4.v.irv.num * var_9.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_14 = LVI_BOOL(var_9.v.irv.num == var_4.v.rv * var_9.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_14 = LVI_BOOL(var_9.v.rv == var_4.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_14 = LVI_BOOL(var_9.v.rv * var_4.v.irv.den == var_4.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_14 = LVI_BOOL(var_9.v.rv == var_4.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_14)) {
var_8 = var_6;
break;
}
lval var_15;
lval var_16;
if (var_10.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_16 = (var_10.v.pair.car ? *(var_10.v.pair.car) : LVI_NIL());
var_15 = var_16;
lval var_18;
var_18 = LVI_BOOL(false);
do {
lval var_19;
var_19 = LVI_BOOL(var_15.vt == LV_CHAR);
var_18 = var_19;
if (!LV_IS_FALSE(var_18)) { break; }
lval var_20;
var_20 = LVI_BOOL(true);
do {
lval var_21;
var_21 = LVI_BOOL(var_15.vt == LV_INT);
var_20 = var_21;
if (LV_IS_FALSE(var_20)) { break; }
lval var_22;
var_22 = LVI_INT(0);
lval var_23;
switch (TYPE_COMBO(var_15.vt,var_22.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_23 = LVI_BOOL(var_15.v.iv >= var_22.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_23 = LVI_BOOL(var_15.v.iv * var_22.v.irv.den >= var_22.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_23 = LVI_BOOL(var_15.v.iv >= var_22.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_23 = LVI_BOOL(var_15.v.irv.num >= var_22.v.iv * var_15.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_23 = LVI_BOOL(var_15.v.irv.num * var_22.v.irv.den >= var_22.v.irv.num * var_15.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_23 = LVI_BOOL(var_15.v.irv.num >= var_22.v.rv * var_15.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_23 = LVI_BOOL(var_15.v.rv >= var_22.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_23 = LVI_BOOL(var_15.v.rv * var_22.v.irv.den >= var_22.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_23 = LVI_BOOL(var_15.v.rv >= var_22.v.rv);
break;
default:
WILE_EX(">=", "inputs are not real-valued numbers");
break;
}
var_20 = var_23;
if (LV_IS_FALSE(var_20)) { break; }
lval var_24;
var_24 = LVI_INT(256);
lval var_25;
switch (TYPE_COMBO(var_15.vt,var_24.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_25 = LVI_BOOL(var_15.v.iv < var_24.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_25 = LVI_BOOL(var_15.v.iv * var_24.v.irv.den < var_24.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_25 = LVI_BOOL(var_15.v.iv < var_24.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_25 = LVI_BOOL(var_15.v.irv.num < var_24.v.iv * var_15.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_25 = LVI_BOOL(var_15.v.irv.num * var_24.v.irv.den < var_24.v.irv.num * var_15.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_25 = LVI_BOOL(var_15.v.irv.num < var_24.v.rv * var_15.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_25 = LVI_BOOL(var_15.v.rv < var_24.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_25 = LVI_BOOL(var_15.v.rv * var_24.v.irv.den < var_24.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_25 = LVI_BOOL(var_15.v.rv < var_24.v.rv);
break;
default:
WILE_EX("<", "inputs are not real-valued numbers");
break;
}
var_20 = var_25;
if (LV_IS_FALSE(var_20)) { break; }
} while (0);
var_18 = var_20;
if (!LV_IS_FALSE(var_18)) { break; }
} while (0);
if (LV_IS_FALSE(var_18)) {
lval var_26;
var_26 = LVI_STRING("list->bytevector got a bad value");
lval var_27;
{
lval vs[1];
vs[0] = var_26;
var_27 = gen_list(1, vs, NULL);
}
if (var_27.vt == LV_PAIR && (var_27.v.pair.cdr == NULL || var_27.v.pair.cdr->vt == LV_NIL)) {
var_27 = (var_27.v.pair.car ? *(var_27.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_27;
cachalot->l_whence = 0;
cachalot->c_whence = LISP_WHENCE;
longjmp(cachalot->cenv, 1);
} else {
{
if (var_6.vt != LV_BVECTOR) {
WILE_EX("bytevector-set!", "input is not a bytevector");
}
if (var_9.vt != LV_INT || var_9.v.iv < 0 || (size_t) var_9.v.iv >= var_6.v.bvec.capa) {
WILE_EX("bytevector-set!", "got bad index value");
}
if (!(var_15.vt == LV_CHAR || (var_15.vt == LV_INT && var_15.v.iv >= 0 && var_15.v.iv < 256))) {
WILE_EX("bytevector-set!", "got bad input value");
}
var_6.v.bvec.arr[var_9.v.iv] = (var_15.vt == LV_CHAR) ? var_15.v.chr : var_15.v.iv;
}
}
lval var_29;
var_29 = LVI_INT(1);
lval var_30;
var_30 = LVI_INT(var_9.v.iv + var_29.v.iv);
var_11 = var_30;
lval var_31;
if (var_10.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_31 = (var_10.v.pair.cdr ? *(var_10.v.pair.cdr) : LVI_NIL());
var_12 = var_31;
var_9 = var_11;
var_10 = var_12;
} while (1);
return var_8;
}
// end of function wile_list2bytevector