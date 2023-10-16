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
static lval fn_5(lptr*, lptr);

// definitions

// @@@ lambda (x) @@@ bld-rtl-dir/wile-rtl2-000036.scm:14 @@@ fn_5 @@@
static lval fn_5(lptr* var_6, lptr var_7)
{
return var_7[0];
}
// end of lambda fn_5

// @@@ promote/rat @@@ bld-rtl-dir/wile-rtl2-000036.scm:14 @@@ fn_9 @@@
static lval fn_9(lptr* var_10, lptr var_11)
{
lval var_13;
if (var_11[0].vt == LV_INT) {
var_13 = LVI_RAT(var_11[0].v.iv, 1);
} else {
var_13 = var_11[0];
}
return var_13;
}
// end of prim fn_9

// @@@ promote/real @@@ bld-rtl-dir/wile-rtl2-000036.scm:14 @@@ fn_14 @@@
static lval fn_14(lptr* var_15, lptr var_16)
{
lval var_18;
if (var_16[0].vt == LV_INT) {
var_18 = LVI_REAL((lisp_real_t) var_16[0].v.iv);
} else if (var_16[0].vt == LV_RAT) {
var_18 = LVI_REAL(LV_RAT2REAL(var_16[0]));
} else {
var_18 = var_16[0];
}
return var_18;
}
// end of prim fn_14

// @@@ max/i @@@ bld-rtl-dir/wile-rtl2-000036.scm:15 @@@ fn_21 @@@
static lval fn_21(lptr* var_22, lptr var_23)
{
lval var_25;
var_25 = LVI_INT((var_23[0].v.iv > var_23[1].v.iv) ? var_23[0].v.iv : var_23[1].v.iv);
return var_25;
}
// end of prim fn_21

// @@@ max/q @@@ bld-rtl-dir/wile-rtl2-000036.scm:15 @@@ fn_26 @@@
static lval fn_26(lptr* var_27, lptr var_28)
{
lval var_30;
if (var_28[0].v.irv.num * var_28[1].v.irv.den > var_28[1].v.irv.num * var_28[0].v.irv.den) {
var_30 = var_28[0];
} else {
var_30 = var_28[1];
}
return var_30;
}
// end of prim fn_26

// @@@ max/r @@@ bld-rtl-dir/wile-rtl2-000036.scm:15 @@@ fn_31 @@@
static lval fn_31(lptr* var_32, lptr var_33)
{
lval var_35;
var_35 = LVI_REAL((var_33[0].v.rv > var_33[1].v.rv) ? var_33[0].v.rv : var_33[1].v.rv);
return var_35;
}
// end of prim fn_31

// @@@ max/i @@@ bld-rtl-dir/wile-rtl2-000036.scm:22 @@@ fn_53 @@@
static lval fn_53(lptr* var_54, lptr var_55)
{
lval var_57;
var_57 = LVI_INT((var_55[0].v.iv > var_55[1].v.iv) ? var_55[0].v.iv : var_55[1].v.iv);
return var_57;
}
// end of prim fn_53

// @@@ number/type @@@ bld-rtl-dir/wile-rtl2-000036.scm:22 @@@ fn_59 @@@
static lval fn_59(lptr* var_60, lptr var_61)
{
lval var_63;
switch (var_61[0].vt) {
case LV_INT:
var_63 = LVI_INT(0);
break;
case LV_RAT:
var_63 = LVI_INT(1);
break;
case LV_REAL:
var_63 = LVI_INT(2);
break;
case LV_CMPLX:
var_63 = LVI_INT(3);
break;
default:
var_63 = LVI_INT(4);
break;
}
return var_63;
}
// end of prim fn_59

// @@@ (max . vs) @@@ bld-rtl-dir/wile-rtl2-000036.scm:13 @@@ wile_max @@@
lval wile_max(lptr* var_1, lptr var_2)
{
lval var_4;
MK_CLOS(var_6,0);
lval var_19;
{
lval vs[3];
vs[0] = LVI_PROC(fn_5,var_6,1);
vs[1] = LVI_PROC(fn_9,NULL,1);
vs[2] = LVI_PROC(fn_14,NULL,1);
var_19 = wile_gen_list(3, vs, NULL);
}
var_4 = var_19;
lval var_20;
lval var_36;
{
lval vs[3];
vs[0] = LVI_PROC(fn_21,NULL,2);
vs[1] = LVI_PROC(fn_26,NULL,2);
vs[2] = LVI_PROC(fn_31,NULL,2);
var_36 = wile_gen_list(3, vs, NULL);
}
var_20 = var_36;
lval var_37;
do {
lval var_38;
var_38 = LVI_BOOL(var_2[0].vt == LV_NIL);
if (!LV_IS_FALSE(var_38)) {
lval var_39;
var_39 = LVI_REAL(-1.00000000000000000000000000000000000e+00Q);
lval var_40;
var_40 = LVI_REAL(0.00000000000000000000000000000000000e+00Q);
lval var_41;
var_41 = LVI_REAL(var_39.v.rv / var_40.v.rv);
var_37 = var_41;
break;
}
lval var_42;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_42 = (var_2[0].v.pair.cdr ? *(var_2[0].v.pair.cdr) : LVI_NIL());
lval var_43;
var_43 = LVI_BOOL(var_42.vt == LV_NIL);
if (!LV_IS_FALSE(var_43)) {
lval var_44;
lval var_45;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_45 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
lval var_46;
switch (var_45.vt) {
case LV_INT:
var_46 = LVI_INT(0);
break;
case LV_RAT:
var_46 = LVI_INT(1);
break;
case LV_REAL:
var_46 = LVI_INT(2);
break;
case LV_CMPLX:
var_46 = LVI_INT(3);
break;
default:
var_46 = LVI_INT(4);
break;
}
lval var_47;
var_47 = LVI_INT(3);
lval var_48;
switch (TYPE_COMBO(var_46.vt,var_47.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_48 = LVI_BOOL(var_46.v.iv < var_47.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_48 = LVI_BOOL(var_46.v.iv * var_47.v.irv.den < var_47.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_48 = LVI_BOOL(var_46.v.iv < var_47.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_48 = LVI_BOOL(var_46.v.irv.num < var_47.v.iv * var_46.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_48 = LVI_BOOL(var_46.v.irv.num * var_47.v.irv.den < var_47.v.irv.num * var_46.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_48 = LVI_BOOL(var_46.v.irv.num < var_47.v.rv * var_46.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_48 = LVI_BOOL(var_46.v.rv < var_47.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_48 = LVI_BOOL(var_46.v.rv * var_47.v.irv.den < var_47.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_48 = LVI_BOOL(var_46.v.rv < var_47.v.rv);
break;
default:
WILE_EX("<", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_48)) {
lval var_49;
var_49 = LVI_STRING("max got a non-real argument");
lval var_50;
{
lval vs[1];
vs[0] = var_49;
var_50 = wile_gen_list(1, vs, NULL);
}
if (var_50.vt == LV_PAIR && (var_50.v.pair.cdr == NULL || var_50.v.pair.cdr->vt == LV_NIL)) {
var_50 = (var_50.v.pair.car ? *(var_50.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_50;
cachalot->l_whence = 0;
cachalot->c_whence = LISP_WHENCE;
longjmp(cachalot->cenv, 1);
var_44 = var_50;
} else {
lval var_51;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_51 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
var_44 = var_51;
}
var_37 = var_44;
break;
}
lval var_52;
lval var_58;
var_58 = LVI_INT(0);
lval var_64;
{
lval vs[8];
vs[0] = LVI_PROC(fn_59,NULL,1);
vs[1] = var_2[0];
var_64 = wile_map1(NULL, vs);
}
lval var_65;
{
lval vs[8];
vs[0] = LVI_PROC(fn_53,NULL,2);
vs[1] = var_58;
vs[2] = var_64;
var_65 = wile_foldl(NULL, vs);
}
var_52 = var_65;
lval var_66;
lval var_67;
var_67 = LVI_INT(3);
lval var_68;
switch (TYPE_COMBO(var_52.vt,var_67.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_68 = LVI_BOOL(var_52.v.iv < var_67.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_68 = LVI_BOOL(var_52.v.iv * var_67.v.irv.den < var_67.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_68 = LVI_BOOL(var_52.v.iv < var_67.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_68 = LVI_BOOL(var_52.v.irv.num < var_67.v.iv * var_52.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_68 = LVI_BOOL(var_52.v.irv.num * var_67.v.irv.den < var_67.v.irv.num * var_52.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_68 = LVI_BOOL(var_52.v.irv.num < var_67.v.rv * var_52.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_68 = LVI_BOOL(var_52.v.rv < var_67.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_68 = LVI_BOOL(var_52.v.rv * var_67.v.irv.den < var_67.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_68 = LVI_BOOL(var_52.v.rv < var_67.v.rv);
break;
default:
WILE_EX("<", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_68)) {
lval var_69;
var_69 = LVI_STRING("max got a non-real argument");
lval var_70;
{
lval vs[1];
vs[0] = var_69;
var_70 = wile_gen_list(1, vs, NULL);
}
if (var_70.vt == LV_PAIR && (var_70.v.pair.cdr == NULL || var_70.v.pair.cdr->vt == LV_NIL)) {
var_70 = (var_70.v.pair.car ? *(var_70.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_70;
cachalot->l_whence = 0;
cachalot->c_whence = LISP_WHENCE;
longjmp(cachalot->cenv, 1);
var_66 = var_70;
} else {
lval var_71;
{
lval vs[8];
vs[0] = var_20;
vs[1] = var_52;
var_71 = wile_list_ref(NULL, vs);
}
lval var_72;
{
lval vs[8];
vs[0] = var_4;
vs[1] = var_52;
var_72 = wile_list_ref(NULL, vs);
}
lval var_73;
{
lval vs[8];
vs[0] = var_72;
vs[1] = var_2[0];
var_73 = wile_map1(NULL, vs);
}
lval var_74;
{
lval vs[8];
vs[0] = var_71;
vs[1] = var_73;
var_74 = wile_foldl1(NULL, vs);
}
var_66 = var_74;
}
var_37 = var_66;
} while (0);
return var_37;
}
// end of function wile_max
