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
static lval fn_4(lptr*, lptr);	// (fore2 proc ls)

// definitions

// @@@ car @@@ bld-rtl-dir/wile-rtl2-000025.scm:17 @@@ fn_11 @@@
static lval fn_11(lptr* var_12, lptr var_13)
{
lval var_15;
if (var_13[0].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_15 = (var_13[0].v.pair.car ? *(var_13[0].v.pair.car) : LVI_NIL());
return var_15;
}
// end of prim fn_11

// @@@ cdr @@@ bld-rtl-dir/wile-rtl2-000025.scm:18 @@@ fn_18 @@@
static lval fn_18(lptr* var_19, lptr var_20)
{
lval var_22;
if (var_20[0].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_22 = (var_20[0].v.pair.cdr ? *(var_20[0].v.pair.cdr) : LVI_NIL());
return var_22;
}
// end of prim fn_18

// @@@ (fore2 proc ls) @@@ bld-rtl-dir/wile-rtl2-000025.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_5, lptr var_6)
{
lbl_7:;
lval var_8;
lval var_9;
if (var_6[1].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_9 = (var_6[1].v.pair.car ? *(var_6[1].v.pair.car) : LVI_NIL());
lval var_10;
var_10 = LVI_BOOL(var_9.vt == LV_NIL);
if (LV_IS_FALSE(var_10)) {
lval var_16;
{
lval vs[8];
vs[0] = LVI_PROC(fn_11,NULL,1);
vs[1] = var_6[1];
var_16 = wile_map1(NULL, vs);
}
lval var_17;
{
lval vs[2];
vs[0] = var_6[0];
vs[1] = var_16;
var_17 = gen_list(2, vs, NULL);
}
var_17 = wile_apply_function(&(var_17), __FILE__, __LINE__);
lval var_23;
{
lval vs[8];
vs[0] = LVI_PROC(fn_18,NULL,1);
vs[1] = var_6[1];
var_23 = wile_map1(NULL, vs);
}
lval var_26[8];
var_26[0] = var_6[0];
var_26[1] = var_23;
var_6[0] = var_26[0];
var_6[1] = var_26[1];
goto lbl_7;	// selfie
} else {
lval var_27;
var_27 = LVI_BOOL(true);
var_8 = var_27;
}
return var_8;
}
// end of function fn_4

// @@@ list-length @@@ bld-rtl-dir/wile-rtl2-000025.scm:20 @@@ fn_31 @@@
static lval fn_31(lptr* var_32, lptr var_33)
{
lval var_35;
{
lval vs[8];
vs[0] = var_33[0];
var_35 = wile_list_length(NULL, vs);
}
return var_35;
}
// end of prim fn_31

// @@@ min/i @@@ bld-rtl-dir/wile-rtl2-000025.scm:21 @@@ fn_38 @@@
static lval fn_38(lptr* var_39, lptr var_40)
{
lval var_42;
var_42 = LVI_INT((var_40[0].v.iv < var_40[1].v.iv) ? var_40[0].v.iv : var_40[1].v.iv);
return var_42;
}
// end of prim fn_38

// @@@ max/i @@@ bld-rtl-dir/wile-rtl2-000025.scm:21 @@@ fn_44 @@@
static lval fn_44(lptr* var_45, lptr var_46)
{
lval var_48;
var_48 = LVI_INT((var_46[0].v.iv > var_46[1].v.iv) ? var_46[0].v.iv : var_46[1].v.iv);
return var_48;
}
// end of prim fn_44

// @@@ (for-each proc lst . lsts) @@@ bld-rtl-dir/wile-rtl2-000025.scm:13 @@@ wile_for_each @@@
lval wile_for_each(lptr* var_1, lptr var_2)
{
lval var_28;
lval var_29;
{
lptr p1 = NULL, p2 = NULL;
if (var_2[1].vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_2[1];
}
if (var_2[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_2[2];
}
var_29 = LVI_PAIR(p1, p2);
}
var_28 = var_29;
lval var_30;
lval var_36;
{
lval vs[8];
vs[0] = LVI_PROC(fn_31,NULL,1);
vs[1] = var_28;
var_36 = wile_map1(NULL, vs);
}
var_30 = var_36;
lval var_37;
lval var_43;
{
lval vs[8];
vs[0] = LVI_PROC(fn_38,NULL,2);
vs[1] = var_30;
var_43 = wile_foldl1(NULL, vs);
}
lval var_49;
{
lval vs[8];
vs[0] = LVI_PROC(fn_44,NULL,2);
vs[1] = var_30;
var_49 = wile_foldl1(NULL, vs);
}
lval var_50;
switch (TYPE_COMBO(var_43.vt,var_49.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_50 = LVI_BOOL(var_43.v.iv != var_49.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_50 = LVI_BOOL(var_43.v.iv * var_49.v.irv.den != var_49.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_50 = LVI_BOOL(var_43.v.iv != var_49.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_50 = LVI_BOOL(var_43.v.irv.num != var_49.v.iv * var_43.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_50 = LVI_BOOL(var_43.v.irv.num * var_49.v.irv.den != var_49.v.irv.num * var_43.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_50 = LVI_BOOL(var_43.v.irv.num != var_49.v.rv * var_43.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_50 = LVI_BOOL(var_43.v.rv != var_49.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_50 = LVI_BOOL(var_43.v.rv * var_49.v.irv.den != var_49.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_50 = LVI_BOOL(var_43.v.rv != var_49.v.rv);
break;
default:
WILE_EX("!=", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_50)) {
lval var_53[8];
var_53[0] = var_2[0];
var_53[1] = var_28;
var_2[0] = var_53[0];
var_2[1] = var_53[1];
TAIL_CALL fn_4(NULL, var_2);
} else {
lval var_54;
var_54 = LVI_STRING("for-each got lists of different lengths");
lval var_55;
{
lval vs[1];
vs[0] = var_54;
var_55 = gen_list(1, vs, NULL);
}
if (var_55.vt == LV_PAIR && (var_55.v.pair.cdr == NULL || var_55.v.pair.cdr->vt == LV_NIL)) {
var_55 = (var_55.v.pair.car ? *(var_55.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_55;
cachalot->l_whence = 0;
cachalot->c_whence = LISP_WHENCE;
longjmp(cachalot->cenv, 1);
var_37 = var_55;
}
return var_37;
}
// end of function wile_for_each
