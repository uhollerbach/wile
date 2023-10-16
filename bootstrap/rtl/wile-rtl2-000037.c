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
static lval fn_4(lptr*, lptr);	// (merge1 lst acc)
static lval fn_5(lptr*, lptr);	// (merge2 is-lt? lst1 lst2 acc)
static lval fn_6(lptr*, lptr);	// (one-pass is-lt? lst acc)

// definitions

// @@@ (merge1 lst acc) @@@ bld-rtl-dir/wile-rtl2-000037.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_7, lptr var_8)
{
lval var_10;
var_10 = var_8[0];
lval var_11;
var_11 = var_8[1];
lval var_13;
lval var_14;
lval var_15;
var_15 = LVI_INT(0);
var_13 = var_15;
do {
lval var_16;
var_16 = LVI_BOOL(var_10.vt == LV_NIL);
if (!LV_IS_FALSE(var_16)) {
break;
}
lval var_17;
if (var_10.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_17 = (var_10.v.pair.car ? *(var_10.v.pair.car) : LVI_NIL());
lval var_18;
{
lptr p1 = NULL, p2 = NULL;
if (var_17.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_17;
}
if (var_11.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_11;
}
var_18 = LVI_PAIR(p1, p2);
}
var_11 = var_18;
lval var_19;
if (var_10.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_19 = (var_10.v.pair.cdr ? *(var_10.v.pair.cdr) : LVI_NIL());
var_10 = var_19;
lval var_20;
var_20 = LVI_INT(1);
lval var_21;
var_21 = LVI_INT(var_13.v.iv + var_20.v.iv);
var_14 = var_21;
var_13 = var_14;
} while (1);
return var_11;
}
// end of function fn_4

// @@@ (merge2 is-lt? lst1 lst2 acc) @@@ bld-rtl-dir/wile-rtl2-000037.scm:21 @@@ fn_5 @@@
static lval fn_5(lptr* var_22, lptr var_23)
{
lbl_24:;
lval var_25;
var_25 = var_23[1];
lval var_26;
var_26 = var_23[2];
lval var_27;
var_27 = var_23[3];
lval var_28;
do {
lval var_29;
var_29 = LVI_BOOL(var_25.vt == LV_NIL);
if (!LV_IS_FALSE(var_29)) {
lval var_32[8];
var_32[0] = var_26;
var_32[1] = var_27;
var_23[0] = var_32[0];
var_23[1] = var_32[1];
TAIL_CALL fn_4(NULL, var_23);
}
lval var_33;
var_33 = LVI_BOOL(var_26.vt == LV_NIL);
if (!LV_IS_FALSE(var_33)) {
lval var_36[8];
var_36[0] = var_25;
var_36[1] = var_27;
var_23[0] = var_36[0];
var_23[1] = var_36[1];
TAIL_CALL fn_4(NULL, var_23);
}
lval var_37;
if (var_26.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_37 = (var_26.v.pair.car ? *(var_26.v.pair.car) : LVI_NIL());
lval var_38;
if (var_25.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_38 = (var_25.v.pair.car ? *(var_25.v.pair.car) : LVI_NIL());
lval var_39;
{
lval vs[2];
vs[0] = var_37;
vs[1] = var_38;
var_39 = wile_gen_list(2, vs, NULL);
}
lval var_40;
{
lval vs[2];
vs[0] = var_23[0];
vs[1] = var_39;
var_40 = wile_gen_list(2, vs, NULL);
}
var_40 = wile_apply_function(&(var_40), __FILE__, __LINE__);
if (!LV_IS_FALSE(var_40)) {
lval var_41;
if (var_26.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_41 = (var_26.v.pair.cdr ? *(var_26.v.pair.cdr) : LVI_NIL());
lval var_42;
if (var_26.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_42 = (var_26.v.pair.car ? *(var_26.v.pair.car) : LVI_NIL());
lval var_43;
{
lptr p1 = NULL, p2 = NULL;
if (var_42.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_42;
}
if (var_27.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_27;
}
var_43 = LVI_PAIR(p1, p2);
}
lval var_46[8];
var_46[0] = var_23[0];
var_46[1] = var_25;
var_46[2] = var_41;
var_46[3] = var_43;
var_23[0] = var_46[0];
var_23[1] = var_46[1];
var_23[2] = var_46[2];
var_23[3] = var_46[3];
goto lbl_24;	// selfie
}
lval var_47;
if (var_25.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_47 = (var_25.v.pair.cdr ? *(var_25.v.pair.cdr) : LVI_NIL());
lval var_48;
if (var_25.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_48 = (var_25.v.pair.car ? *(var_25.v.pair.car) : LVI_NIL());
lval var_49;
{
lptr p1 = NULL, p2 = NULL;
if (var_48.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_48;
}
if (var_27.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_27;
}
var_49 = LVI_PAIR(p1, p2);
}
lval var_52[8];
var_52[0] = var_23[0];
var_52[1] = var_47;
var_52[2] = var_26;
var_52[3] = var_49;
var_23[0] = var_52[0];
var_23[1] = var_52[1];
var_23[2] = var_52[2];
var_23[3] = var_52[3];
goto lbl_24;	// selfie
} while (0);
return var_28;
}
// end of function fn_5

// @@@ (one-pass is-lt? lst acc) @@@ bld-rtl-dir/wile-rtl2-000037.scm:31 @@@ fn_6 @@@
static lval fn_6(lptr* var_53, lptr var_54)
{
lbl_55:;
lval var_56;
do {
lval var_57;
var_57 = LVI_BOOL(var_54[1].vt == LV_NIL);
if (!LV_IS_FALSE(var_57)) {
lval var_58;
{
lval vs[8];
vs[0] = var_54[2];
var_58 = wile_list_reverse(NULL, vs);
}
var_56 = var_58;
break;
}
lval var_59;
if (var_54[1].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_59 = (var_54[1].v.pair.cdr ? *(var_54[1].v.pair.cdr) : LVI_NIL());
lval var_60;
var_60 = LVI_BOOL(var_59.vt == LV_NIL);
if (!LV_IS_FALSE(var_60)) {
lval var_61;
if (var_54[1].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_61 = (var_54[1].v.pair.car ? *(var_54[1].v.pair.car) : LVI_NIL());
lval var_62;
{
lptr p1 = NULL, p2 = NULL;
if (var_61.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_61;
}
if (var_54[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_54[2];
}
var_62 = LVI_PAIR(p1, p2);
}
lval var_63;
{
lval vs[8];
vs[0] = var_62;
var_63 = wile_list_reverse(NULL, vs);
}
var_56 = var_63;
break;
}
lval var_64;
var_64 = LVI_STRING("cddr");
lval var_65;
{
char* cp = strchr(var_64.v.str, 'r');
var_65 = var_54[1];
while (*(--cp) != 'c') {
if (var_65.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_65 = (var_65.v.pair.car ? *(var_65.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_65 = (var_65.v.pair.cdr ? *(var_65.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_64.v.str);
}
}
}
lval var_66;
if (var_54[1].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_66 = (var_54[1].v.pair.car ? *(var_54[1].v.pair.car) : LVI_NIL());
lval var_67;
var_67 = LVI_STRING("cadr");
lval var_68;
{
char* cp = strchr(var_67.v.str, 'r');
var_68 = var_54[1];
while (*(--cp) != 'c') {
if (var_68.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_68 = (var_68.v.pair.car ? *(var_68.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_68 = (var_68.v.pair.cdr ? *(var_68.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_67.v.str);
}
}
}
lval var_69;
var_69 = LVI_NIL();
lval var_70;
lval var_71[8];
var_71[0] = var_54[0];
var_71[1] = var_66;
var_71[2] = var_68;
var_71[3] = var_69;
var_70 = fn_5(NULL, var_71);
lval var_73;
{
lval vs[8];
vs[0] = var_70;
var_73 = wile_list_reverse(NULL, vs);
}
lval var_74;
{
lptr p1 = NULL, p2 = NULL;
if (var_73.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_73;
}
if (var_54[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_54[2];
}
var_74 = LVI_PAIR(p1, p2);
}
lval var_77[8];
var_77[0] = var_54[0];
var_77[1] = var_65;
var_77[2] = var_74;
var_54[0] = var_77[0];
var_54[1] = var_77[1];
var_54[2] = var_77[2];
goto lbl_55;	// selfie
} while (0);
return var_56;
}
// end of function fn_6

// @@@ list @@@ bld-rtl-dir/wile-rtl2-000037.scm:40 @@@ fn_81 @@@
static lval fn_81(lptr* var_82, lptr var_83)
{
lval var_85;
var_85 = var_83[0];
return var_85;
}
// end of prim fn_81

// @@@ (list-sort is-lt? lst) @@@ bld-rtl-dir/wile-rtl2-000037.scm:13 @@@ wile_list_sort @@@
lval wile_list_sort(lptr* var_1, lptr var_2)
{
lval var_78;
lval var_79;
{
lval vs[8];
vs[0] = var_2[1];
var_79 = wile_list_length(NULL, vs);
}
var_78 = var_79;
lval var_80;
lval var_86;
{
lval vs[8];
vs[0] = LVI_PROC(fn_81,NULL,-1);
vs[1] = var_2[1];
var_86 = wile_map1(NULL, vs);
}
var_80 = var_86;
lval var_87;
lval var_88;
var_88 = LVI_INT(1);
var_87 = var_88;
lval var_89;
lval var_90;
switch (var_78.vt) {
case LV_REAL:
var_90 = LVI_BOOL(var_78.v.rv > 0.0);
break;
case LV_RAT:
var_90 = LVI_BOOL((var_78.v.irv.num > 0 && var_78.v.irv.den >= 0) || (var_78.v.irv.num < 0 && var_78.v.irv.den < 0));
break;
case LV_INT:
var_90 = LVI_BOOL(var_78.v.iv > 0);
break;
default:
WILE_EX("positive?", "expects a real-valued number");
}
if (LV_IS_FALSE(var_90)) {
lval var_91;
var_91 = LVI_NIL();
var_89 = var_91;
} else {
lval var_93;
lval var_94;
lval var_95;
var_95 = LVI_INT(0);
var_93 = var_95;
do {
lval var_96;
switch (TYPE_COMBO(var_87.vt,var_78.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_96 = LVI_BOOL(var_87.v.iv < var_78.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_96 = LVI_BOOL(var_87.v.iv * var_78.v.irv.den < var_78.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_96 = LVI_BOOL(var_87.v.iv < var_78.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_96 = LVI_BOOL(var_87.v.irv.num < var_78.v.iv * var_87.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_96 = LVI_BOOL(var_87.v.irv.num * var_78.v.irv.den < var_78.v.irv.num * var_87.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_96 = LVI_BOOL(var_87.v.irv.num < var_78.v.rv * var_87.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_96 = LVI_BOOL(var_87.v.rv < var_78.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_96 = LVI_BOOL(var_87.v.rv * var_78.v.irv.den < var_78.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_96 = LVI_BOOL(var_87.v.rv < var_78.v.rv);
break;
default:
WILE_EX("<", "inputs are not real-valued numbers");
break;
}
lval var_97;
var_97 = LVI_BOOL(LV_IS_FALSE(var_96));
if (!LV_IS_FALSE(var_97)) {
break;
}
lval var_98;
var_98 = LVI_INT(2);
lval var_99;
var_99 = LVI_INT(var_98.v.iv * var_87.v.iv);
var_87 = var_99;
lval var_100;
var_100 = LVI_NIL();
lval var_101;
lval var_102[8];
var_102[0] = var_2[0];
var_102[1] = var_80;
var_102[2] = var_100;
var_101 = fn_6(NULL, var_102);
var_80 = var_101;
lval var_104;
var_104 = LVI_INT(1);
lval var_105;
var_105 = LVI_INT(var_93.v.iv + var_104.v.iv);
var_94 = var_105;
var_93 = var_94;
} while (1);
lval var_106;
if (var_80.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_106 = (var_80.v.pair.car ? *(var_80.v.pair.car) : LVI_NIL());
var_89 = var_106;
}
return var_89;
}
// end of function wile_list_sort
