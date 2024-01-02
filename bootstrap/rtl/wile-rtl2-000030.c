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
static lval fn_4(lptr*, lptr, const char*);

// definitions

// @@@ car @@@ bld-rtl-dir/wile-rtl2-000030.scm:17 @@@ fn_11 @@@
static lval fn_11(lptr* var_12, lptr var_13, const char* cloc)
{
lval var_15;
if (var_13[0].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000030.scm:17", "input is not a pair!");
}
var_15 = (var_13[0].v.pair.car ? *(var_13[0].v.pair.car) : LVI_NIL());
return var_15;
}
// end of prim fn_11

// @@@ cdr @@@ bld-rtl-dir/wile-rtl2-000030.scm:18 @@@ fn_20 @@@
static lval fn_20(lptr* var_21, lptr var_22, const char* cloc)
{
lval var_24;
if (var_22[0].vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000030.scm:18", "input is not a pair!");
}
var_24 = (var_22[0].v.pair.cdr ? *(var_22[0].v.pair.cdr) : LVI_NIL());
return var_24;
}
// end of prim fn_20

// @@@ (fore2 proc ls) @@@ bld-rtl-dir/wile-rtl2-000030.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_5, lptr var_6, const char* cloc)
{
lbl_7:;
lval var_8;
lval var_9;
if (var_6[1].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000030.scm:15", "input is not a pair!");
}
var_9 = (var_6[1].v.pair.car ? *(var_6[1].v.pair.car) : LVI_NIL());
lval var_10;
var_10 = LVI_BOOL(var_9.vt == LV_NIL);
if (LV_IS_FALSE(var_10)) {
lval var_16;
var_16 = LVI_NIL();
{
lval var_17[8];
var_17[0] = LVI_PROC(fn_11,NULL,1);
var_17[1] = var_6[1];
var_17[2] = var_16;
var_16 = wile_map(NULL, var_17, "bld-rtl-dir/wile-rtl2-000030.scm:17");
}
lval var_18;
{
lval var_19[2];
var_19[0] = var_6[0];
var_19[1] = var_16;
var_18 = wile_gen_list(2, var_19, NULL);
}
var_18 = wile_apply_function(&(var_18), "bld-rtl-dir/wile-rtl2-000030.scm:17");
lval var_25;
var_25 = LVI_NIL();
{
lval var_26[8];
var_26[0] = LVI_PROC(fn_20,NULL,1);
var_26[1] = var_6[1];
var_26[2] = var_25;
var_25 = wile_map(NULL, var_26, "bld-rtl-dir/wile-rtl2-000030.scm:18");
}
lval var_29[8];
var_29[0] = var_6[0];
var_29[1] = var_25;
var_6[0] = var_29[0];
var_6[1] = var_29[1];
goto lbl_7;
} else {
lval var_30;
var_30 = LVI_BOOL(true);
var_8 = var_30;
}
return var_8;
}
// end of function fn_4

// @@@ list-length @@@ bld-rtl-dir/wile-rtl2-000030.scm:20 @@@ fn_34 @@@
static lval fn_34(lptr* var_35, lptr var_36, const char* cloc)
{
lval var_38;
{
lval var_39[8];
var_39[0] = var_36[0];
var_38 = wile_list_length(NULL, var_39, "bld-rtl-dir/wile-rtl2-000030.scm:20");
}
return var_38;
}
// end of prim fn_34

// @@@ min/i @@@ bld-rtl-dir/wile-rtl2-000030.scm:21 @@@ fn_43 @@@
static lval fn_43(lptr* var_44, lptr var_45, const char* cloc)
{
lval var_47;
var_47 = LVI_INT((var_45[0].v.iv < var_45[1].v.iv) ? var_45[0].v.iv : var_45[1].v.iv);
return var_47;
}
// end of prim fn_43

// @@@ max/i @@@ bld-rtl-dir/wile-rtl2-000030.scm:21 @@@ fn_50 @@@
static lval fn_50(lptr* var_51, lptr var_52, const char* cloc)
{
lval var_54;
var_54 = LVI_INT((var_52[0].v.iv > var_52[1].v.iv) ? var_52[0].v.iv : var_52[1].v.iv);
return var_54;
}
// end of prim fn_50

// @@@ (for-each proc lst . lsts) @@@ bld-rtl-dir/wile-rtl2-000030.scm:13 @@@ wile_for_each @@@
lval wile_for_each(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_31;
lval var_32;
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
var_32 = LVI_PAIR(p1, p2);
}
var_31 = var_32;
lval var_33;
lval var_40;
var_40 = LVI_NIL();
{
lval var_41[8];
var_41[0] = LVI_PROC(fn_34,NULL,1);
var_41[1] = var_31;
var_41[2] = var_40;
var_40 = wile_map(NULL, var_41, "bld-rtl-dir/wile-rtl2-000030.scm:20");
}
var_33 = var_40;
lval var_42;
lval var_48;
{
lval var_49[8];
var_49[0] = LVI_PROC(fn_43,NULL,2);
var_49[1] = var_33;
var_48 = wile_foldl1(NULL, var_49, "bld-rtl-dir/wile-rtl2-000030.scm:21");
}
lval var_55;
{
lval var_56[8];
var_56[0] = LVI_PROC(fn_50,NULL,2);
var_56[1] = var_33;
var_55 = wile_foldl1(NULL, var_56, "bld-rtl-dir/wile-rtl2-000030.scm:21");
}
lval var_57;
switch (TYPE_COMBO(var_48.vt,var_55.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_57 = LVI_BOOL(var_48.v.iv != var_55.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_57 = LVI_BOOL(var_48.v.iv * var_55.v.irv.den != var_55.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_57 = LVI_BOOL(var_48.v.iv != var_55.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_57 = LVI_BOOL(var_48.v.irv.num != var_55.v.iv * var_48.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_57 = LVI_BOOL(var_48.v.irv.num * var_55.v.irv.den != var_55.v.irv.num * var_48.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_57 = LVI_BOOL(var_48.v.irv.num != var_55.v.rv * var_48.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_57 = LVI_BOOL(var_48.v.rv != var_55.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_57 = LVI_BOOL(var_48.v.rv * var_55.v.irv.den != var_55.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_57 = LVI_BOOL(var_48.v.rv != var_55.v.rv);
break;
default:
wile_exception("!=", "bld-rtl-dir/wile-rtl2-000030.scm:21", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_57)) {
lval var_60[8];
var_60[0] = var_2[0];
var_60[1] = var_31;
var_2[0] = var_60[0];
var_2[1] = var_60[1];
TAIL_CALL fn_4(NULL, var_2, "bld-rtl-dir/wile-rtl2-000030.scm:23");
} else {
lval var_61;
var_61 = LVI_STRING("for-each got lists of different lengths");
lval var_62;
{
lval var_63[1];
var_63[0] = var_61;
var_62 = wile_gen_list(1, var_63, NULL);
}
if (var_62.vt == LV_PAIR && (var_62.v.pair.cdr == NULL || var_62.v.pair.cdr->vt == LV_NIL)) {
var_62 = (var_62.v.pair.car ? *(var_62.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_62;
cachalot->whence = "bld-rtl-dir/wile-rtl2-000030.scm:22";
longjmp(cachalot->cenv, 1);
var_42 = var_62;
}
return var_42;
}
// end of function wile_for_each
