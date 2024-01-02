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
static lval fn_5(lptr*, lptr, const char*);

// definitions

// @@@ lambda (x) @@@ bld-rtl-dir/wile-rtl2-000036.scm:14 @@@ fn_5 @@@
static lval fn_5(lptr* var_6, lptr var_7, const char* cloc)
{
return var_7[0];
}
// end of lambda fn_5

// @@@ promote/rat @@@ bld-rtl-dir/wile-rtl2-000036.scm:14 @@@ fn_9 @@@
static lval fn_9(lptr* var_10, lptr var_11, const char* cloc)
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
static lval fn_14(lptr* var_15, lptr var_16, const char* cloc)
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

// @@@ max/i @@@ bld-rtl-dir/wile-rtl2-000036.scm:15 @@@ fn_22 @@@
static lval fn_22(lptr* var_23, lptr var_24, const char* cloc)
{
lval var_26;
var_26 = LVI_INT((var_24[0].v.iv > var_24[1].v.iv) ? var_24[0].v.iv : var_24[1].v.iv);
return var_26;
}
// end of prim fn_22

// @@@ max/q @@@ bld-rtl-dir/wile-rtl2-000036.scm:15 @@@ fn_27 @@@
static lval fn_27(lptr* var_28, lptr var_29, const char* cloc)
{
lval var_31;
if (var_29[0].v.irv.num * var_29[1].v.irv.den > var_29[1].v.irv.num * var_29[0].v.irv.den) {
var_31 = var_29[0];
} else {
var_31 = var_29[1];
}
return var_31;
}
// end of prim fn_27

// @@@ max/r @@@ bld-rtl-dir/wile-rtl2-000036.scm:15 @@@ fn_32 @@@
static lval fn_32(lptr* var_33, lptr var_34, const char* cloc)
{
lval var_36;
var_36 = LVI_REAL((var_34[0].v.rv > var_34[1].v.rv) ? var_34[0].v.rv : var_34[1].v.rv);
return var_36;
}
// end of prim fn_32

// @@@ number/type @@@ bld-rtl-dir/wile-rtl2-000036.scm:22 @@@ fn_57 @@@
static lval fn_57(lptr* var_58, lptr var_59, const char* cloc)
{
lval var_61;
switch (var_59[0].vt) {
case LV_INT:
var_61 = LVI_INT(0);
break;
case LV_RAT:
var_61 = LVI_INT(1);
break;
case LV_REAL:
var_61 = LVI_INT(2);
break;
case LV_CMPLX:
var_61 = LVI_INT(3);
break;
default:
var_61 = LVI_INT(4);
break;
}
return var_61;
}
// end of prim fn_57

// @@@ (max . vs) @@@ bld-rtl-dir/wile-rtl2-000036.scm:13 @@@ wile_max @@@
lval wile_max(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
MK_CLOS(var_6,0);
lval var_19;
{
lval var_20[3];
var_20[0] = LVI_PROC(fn_5,var_6,1);
var_20[1] = LVI_PROC(fn_9,NULL,1);
var_20[2] = LVI_PROC(fn_14,NULL,1);
var_19 = wile_gen_list(3, var_20, NULL);
}
var_4 = var_19;
lval var_21;
lval var_37;
{
lval var_38[3];
var_38[0] = LVI_PROC(fn_22,NULL,2);
var_38[1] = LVI_PROC(fn_27,NULL,2);
var_38[2] = LVI_PROC(fn_32,NULL,2);
var_37 = wile_gen_list(3, var_38, NULL);
}
var_21 = var_37;
lval var_39;
do {
lval var_40;
var_40 = LVI_BOOL(var_2[0].vt == LV_NIL);
if (!LV_IS_FALSE(var_40)) {
lval var_41;
var_41 = LVI_REAL(-1.00000000000000000000000000000000000e+00Q);
lval var_42;
var_42 = LVI_REAL(0.00000000000000000000000000000000000e+00Q);
lval var_43;
var_43 = LVI_REAL(var_41.v.rv / var_42.v.rv);
var_39 = var_43;
break;
}
lval var_44;
if (var_2[0].vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000036.scm:17", "input is not a pair!");
}
var_44 = (var_2[0].v.pair.cdr ? *(var_2[0].v.pair.cdr) : LVI_NIL());
lval var_45;
var_45 = LVI_BOOL(var_44.vt == LV_NIL);
if (!LV_IS_FALSE(var_45)) {
lval var_46;
lval var_47;
if (var_2[0].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000036.scm:18", "input is not a pair!");
}
var_47 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
lval var_48;
switch (var_47.vt) {
case LV_INT:
var_48 = LVI_INT(0);
break;
case LV_RAT:
var_48 = LVI_INT(1);
break;
case LV_REAL:
var_48 = LVI_INT(2);
break;
case LV_CMPLX:
var_48 = LVI_INT(3);
break;
default:
var_48 = LVI_INT(4);
break;
}
lval var_49;
var_49 = LVI_INT(3);
lval var_50;
switch (TYPE_COMBO(var_48.vt,var_49.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_50 = LVI_BOOL(var_48.v.iv < var_49.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_50 = LVI_BOOL(var_48.v.iv * var_49.v.irv.den < var_49.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_50 = LVI_BOOL(var_48.v.iv < var_49.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_50 = LVI_BOOL(var_48.v.irv.num < var_49.v.iv * var_48.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_50 = LVI_BOOL(var_48.v.irv.num * var_49.v.irv.den < var_49.v.irv.num * var_48.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_50 = LVI_BOOL(var_48.v.irv.num < var_49.v.rv * var_48.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_50 = LVI_BOOL(var_48.v.rv < var_49.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_50 = LVI_BOOL(var_48.v.rv * var_49.v.irv.den < var_49.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_50 = LVI_BOOL(var_48.v.rv < var_49.v.rv);
break;
default:
wile_exception("<", "bld-rtl-dir/wile-rtl2-000036.scm:18", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_50)) {
lval var_51;
var_51 = LVI_STRING("max got a non-real argument");
lval var_52;
{
lval var_53[1];
var_53[0] = var_51;
var_52 = wile_gen_list(1, var_53, NULL);
}
if (var_52.vt == LV_PAIR && (var_52.v.pair.cdr == NULL || var_52.v.pair.cdr->vt == LV_NIL)) {
var_52 = (var_52.v.pair.car ? *(var_52.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_52;
cachalot->whence = "bld-rtl-dir/wile-rtl2-000036.scm:20";
longjmp(cachalot->cenv, 1);
var_46 = var_52;
} else {
lval var_54;
if (var_2[0].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000036.scm:19", "input is not a pair!");
}
var_54 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
var_46 = var_54;
}
var_39 = var_46;
break;
}
lval var_55;
lval var_56;
var_56 = LVI_INT(0);
lval var_62;
var_62 = LVI_NIL();
{
lval var_63[8];
var_63[0] = LVI_PROC(fn_57,NULL,1);
var_63[1] = var_2[0];
var_63[2] = var_62;
var_62 = wile_map(NULL, var_63, "bld-rtl-dir/wile-rtl2-000036.scm:22");
}
lval var_64;
{
lval var_65[8];
var_65[0] = LVI_PROC(fn_22,NULL,2);
var_65[1] = var_56;
var_65[2] = var_62;
var_64 = wile_foldl(NULL, var_65, "bld-rtl-dir/wile-rtl2-000036.scm:22");
}
var_55 = var_64;
lval var_66;
lval var_67;
var_67 = LVI_INT(3);
lval var_68;
switch (TYPE_COMBO(var_55.vt,var_67.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_68 = LVI_BOOL(var_55.v.iv < var_67.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_68 = LVI_BOOL(var_55.v.iv * var_67.v.irv.den < var_67.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_68 = LVI_BOOL(var_55.v.iv < var_67.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_68 = LVI_BOOL(var_55.v.irv.num < var_67.v.iv * var_55.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_68 = LVI_BOOL(var_55.v.irv.num * var_67.v.irv.den < var_67.v.irv.num * var_55.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_68 = LVI_BOOL(var_55.v.irv.num < var_67.v.rv * var_55.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_68 = LVI_BOOL(var_55.v.rv < var_67.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_68 = LVI_BOOL(var_55.v.rv * var_67.v.irv.den < var_67.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_68 = LVI_BOOL(var_55.v.rv < var_67.v.rv);
break;
default:
wile_exception("<", "bld-rtl-dir/wile-rtl2-000036.scm:23", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_68)) {
lval var_69;
var_69 = LVI_STRING("max got a non-real argument");
lval var_70;
{
lval var_71[1];
var_71[0] = var_69;
var_70 = wile_gen_list(1, var_71, NULL);
}
if (var_70.vt == LV_PAIR && (var_70.v.pair.cdr == NULL || var_70.v.pair.cdr->vt == LV_NIL)) {
var_70 = (var_70.v.pair.car ? *(var_70.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_70;
cachalot->whence = "bld-rtl-dir/wile-rtl2-000036.scm:25";
longjmp(cachalot->cenv, 1);
var_66 = var_70;
} else {
lval var_72;
{
lval var_73[8];
var_73[0] = var_21;
var_73[1] = var_55;
var_72 = wile_list_ref(NULL, var_73, "bld-rtl-dir/wile-rtl2-000036.scm:24");
}
lval var_74;
{
lval var_75[8];
var_75[0] = var_4;
var_75[1] = var_55;
var_74 = wile_list_ref(NULL, var_75, "bld-rtl-dir/wile-rtl2-000036.scm:24");
}
lval var_76;
var_76 = LVI_NIL();
{
lval var_77[8];
var_77[0] = var_74;
var_77[1] = var_2[0];
var_77[2] = var_76;
var_76 = wile_map(NULL, var_77, "bld-rtl-dir/wile-rtl2-000036.scm:24");
}
lval var_78;
{
lval var_79[8];
var_79[0] = var_72;
var_79[1] = var_76;
var_78 = wile_foldl1(NULL, var_79, "bld-rtl-dir/wile-rtl2-000036.scm:24");
}
var_66 = var_78;
}
var_39 = var_66;
} while (0);
return var_39;
}
// end of function wile_max
