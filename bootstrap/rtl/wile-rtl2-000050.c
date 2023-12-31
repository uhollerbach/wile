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
static lval fn_9(lptr*, lptr, const char*);
static lval fn_26(lptr*, lptr, const char*);

// definitions

// @@@ lambda (f n acc) @@@ bld-rtl-dir/wile-rtl2-000050.scm:15 @@@ fn_9 @@@
static lval fn_9(lptr* var_10, lptr var_11, const char* cloc)
{
lbl_12:;
lval var_13;
lval var_14;
switch (TYPE_COMBO(var_11[1].vt,var_11[0].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_14 = LVI_BOOL(var_11[1].v.iv < var_11[0].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_14 = LVI_BOOL(var_11[1].v.iv * var_11[0].v.irv.den < var_11[0].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_14 = LVI_BOOL(var_11[1].v.iv < var_11[0].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_14 = LVI_BOOL(var_11[1].v.irv.num < var_11[0].v.iv * var_11[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_14 = LVI_BOOL(var_11[1].v.irv.num * var_11[0].v.irv.den < var_11[0].v.irv.num * var_11[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_14 = LVI_BOOL(var_11[1].v.irv.num < var_11[0].v.rv * var_11[1].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_14 = LVI_BOOL(var_11[1].v.rv < var_11[0].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_14 = LVI_BOOL(var_11[1].v.rv * var_11[0].v.irv.den < var_11[0].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_14 = LVI_BOOL(var_11[1].v.rv < var_11[0].v.rv);
break;
default:
wile_exception("<", "bld-rtl-dir/wile-rtl2-000050.scm:16", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_14)) {
lval var_15;
var_15 = LVI_INT(1);
lval var_16;
var_16 = LVI_INT(var_11[1].v.iv - var_15.v.iv);
lval var_17;
{
lptr p1 = NULL, p2 = NULL;
if (var_11[1].vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_11[1];
}
if (var_11[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_11[2];
}
var_17 = LVI_PAIR(p1, p2);
}
lval var_20[8];
var_20[0] = var_11[0];
var_20[1] = var_16;
var_20[2] = var_17;
var_11[0] = var_20[0];
var_11[1] = var_20[1];
var_11[2] = var_20[2];
goto lbl_12;
} else {
var_13 = var_11[2];
}
return var_13;
}
// end of lambda fn_9

// @@@ lambda (f n acc) @@@ bld-rtl-dir/wile-rtl2-000050.scm:17 @@@ fn_26 @@@
static lval fn_26(lptr* var_27, lptr var_28, const char* cloc)
{
lbl_29:;
lval var_30;
lval var_31;
switch (TYPE_COMBO(var_28[1].vt,var_28[0].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_31 = LVI_BOOL(var_28[1].v.iv > var_28[0].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_31 = LVI_BOOL(var_28[1].v.iv * var_28[0].v.irv.den > var_28[0].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_31 = LVI_BOOL(var_28[1].v.iv > var_28[0].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_31 = LVI_BOOL(var_28[1].v.irv.num > var_28[0].v.iv * var_28[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_31 = LVI_BOOL(var_28[1].v.irv.num * var_28[0].v.irv.den > var_28[0].v.irv.num * var_28[1].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_31 = LVI_BOOL(var_28[1].v.irv.num > var_28[0].v.rv * var_28[1].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_31 = LVI_BOOL(var_28[1].v.rv > var_28[0].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_31 = LVI_BOOL(var_28[1].v.rv * var_28[0].v.irv.den > var_28[0].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_31 = LVI_BOOL(var_28[1].v.rv > var_28[0].v.rv);
break;
default:
wile_exception(">", "bld-rtl-dir/wile-rtl2-000050.scm:18", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_31)) {
lval var_32;
var_32 = LVI_INT(1);
lval var_33;
var_33 = LVI_INT(var_28[1].v.iv + var_32.v.iv);
lval var_34;
{
lptr p1 = NULL, p2 = NULL;
if (var_28[1].vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_28[1];
}
if (var_28[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_28[2];
}
var_34 = LVI_PAIR(p1, p2);
}
lval var_37[8];
var_37[0] = var_28[0];
var_37[1] = var_33;
var_37[2] = var_34;
var_28[0] = var_37[0];
var_28[1] = var_37[1];
var_28[2] = var_37[2];
goto lbl_29;
} else {
var_30 = var_28[2];
}
return var_30;
}
// end of lambda fn_26

// @@@ (fromto f l) @@@ bld-rtl-dir/wile-rtl2-000050.scm:13 @@@ wile_fromto @@@
lval wile_fromto(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
do {
lval var_5;
switch (TYPE_COMBO(var_2[0].vt,var_2[1].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_5 = LVI_BOOL(var_2[0].v.iv == var_2[1].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_5 = LVI_BOOL(var_2[0].v.iv * var_2[1].v.irv.den == var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_5 = LVI_BOOL(var_2[0].v.iv == var_2[1].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_5 = LVI_BOOL(var_2[0].v.irv.num == var_2[1].v.iv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_5 = LVI_BOOL(var_2[0].v.irv.num * var_2[1].v.irv.den == var_2[1].v.irv.num * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_5 = LVI_BOOL(var_2[0].v.irv.num == var_2[1].v.rv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_5 = LVI_BOOL(var_2[0].v.rv == var_2[1].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_5 = LVI_BOOL(var_2[0].v.rv * var_2[1].v.irv.den == var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_5 = LVI_BOOL(var_2[0].v.rv == var_2[1].v.rv);
break;
default:
wile_exception("==", "bld-rtl-dir/wile-rtl2-000050.scm:14", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_5)) {
lval var_6;
{
lval var_7[1];
var_7[0] = var_2[0];
var_6 = wile_gen_list(1, var_7, NULL);
}
var_4 = var_6;
break;
}
lval var_8;
switch (TYPE_COMBO(var_2[0].vt,var_2[1].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_8 = LVI_BOOL(var_2[0].v.iv < var_2[1].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_8 = LVI_BOOL(var_2[0].v.iv * var_2[1].v.irv.den < var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_8 = LVI_BOOL(var_2[0].v.iv < var_2[1].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_8 = LVI_BOOL(var_2[0].v.irv.num < var_2[1].v.iv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_8 = LVI_BOOL(var_2[0].v.irv.num * var_2[1].v.irv.den < var_2[1].v.irv.num * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_8 = LVI_BOOL(var_2[0].v.irv.num < var_2[1].v.rv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_8 = LVI_BOOL(var_2[0].v.rv < var_2[1].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_8 = LVI_BOOL(var_2[0].v.rv * var_2[1].v.irv.den < var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_8 = LVI_BOOL(var_2[0].v.rv < var_2[1].v.rv);
break;
default:
wile_exception("<", "bld-rtl-dir/wile-rtl2-000050.scm:15", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_8)) {
MK_CLOS(var_10,0);
lval var_21;
var_21 = LVI_NIL();
lval var_22;
lval var_23[8];
var_23[0] = var_2[0];
var_23[1] = var_2[1];
var_23[2] = var_21;
var_22 = fn_9(var_10, var_23, "bld-rtl-dir/wile-rtl2-000050.scm:15");
var_4 = var_22;
break;
}
lval var_25;
switch (TYPE_COMBO(var_2[0].vt,var_2[1].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_25 = LVI_BOOL(var_2[0].v.iv > var_2[1].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_25 = LVI_BOOL(var_2[0].v.iv * var_2[1].v.irv.den > var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_25 = LVI_BOOL(var_2[0].v.iv > var_2[1].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_25 = LVI_BOOL(var_2[0].v.irv.num > var_2[1].v.iv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_25 = LVI_BOOL(var_2[0].v.irv.num * var_2[1].v.irv.den > var_2[1].v.irv.num * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_25 = LVI_BOOL(var_2[0].v.irv.num > var_2[1].v.rv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_25 = LVI_BOOL(var_2[0].v.rv > var_2[1].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_25 = LVI_BOOL(var_2[0].v.rv * var_2[1].v.irv.den > var_2[1].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_25 = LVI_BOOL(var_2[0].v.rv > var_2[1].v.rv);
break;
default:
wile_exception(">", "bld-rtl-dir/wile-rtl2-000050.scm:17", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_25)) {
MK_CLOS(var_27,0);
lval var_38;
var_38 = LVI_NIL();
lval var_39;
lval var_40[8];
var_40[0] = var_2[0];
var_40[1] = var_2[1];
var_40[2] = var_38;
var_39 = fn_26(var_27, var_40, "bld-rtl-dir/wile-rtl2-000050.scm:17");
var_4 = var_39;
break;
}
var_4 = LVI_BOOL(false);
} while (0);
return var_4;
}
// end of function wile_fromto
