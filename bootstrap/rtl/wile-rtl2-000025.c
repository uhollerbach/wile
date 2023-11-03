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

// @@@ string-append @@@ bld-rtl-dir/wile-rtl2-000025.scm:20 @@@ fn_19 @@@
static lval fn_19(lptr* var_20, lptr var_21)
{
lval var_23;
var_23 = var_21[0];
{
lval var_24[8];
var_24[0] = var_23;
var_23 = wile_string_append(NULL, var_24);
}
return var_23;
}
// end of prim fn_19

// @@@ string-append @@@ bld-rtl-dir/wile-rtl2-000025.scm:27 @@@ fn_46 @@@
static lval fn_46(lptr* var_47, lptr var_48)
{
lval var_50;
var_50 = var_48[0];
{
lval var_51[8];
var_51[0] = var_50;
var_50 = wile_string_append(NULL, var_51);
}
return var_50;
}
// end of prim fn_46

// @@@ (string-join-by join . strs) @@@ bld-rtl-dir/wile-rtl2-000025.scm:13 @@@ wile_string_join_by @@@
lval wile_string_join_by(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
lval var_6;
var_6 = LVI_BOOL(true);
do {
lval var_7;
var_7 = LVI_INT(1);
lval var_8;
{
lval var_9[8];
var_9[0] = var_2[1];
var_8 = wile_list_length(NULL, var_9);
}
lval var_10;
switch (TYPE_COMBO(var_7.vt,var_8.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_10 = LVI_BOOL(var_7.v.iv == var_8.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_10 = LVI_BOOL(var_7.v.iv * var_8.v.irv.den == var_8.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_10 = LVI_BOOL(var_7.v.iv == var_8.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_10 = LVI_BOOL(var_7.v.irv.num == var_8.v.iv * var_7.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_10 = LVI_BOOL(var_7.v.irv.num * var_8.v.irv.den == var_8.v.irv.num * var_7.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_10 = LVI_BOOL(var_7.v.irv.num == var_8.v.rv * var_7.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_10 = LVI_BOOL(var_7.v.rv == var_8.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_10 = LVI_BOOL(var_7.v.rv * var_8.v.irv.den == var_8.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_10 = LVI_BOOL(var_7.v.rv == var_8.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
var_6 = var_10;
if (LV_IS_FALSE(var_6)) { break; }
lval var_11;
if (var_2[1].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_11 = (var_2[1].v.pair.car ? *(var_2[1].v.pair.car) : LVI_NIL());
lval var_12;
{
var_12 = var_11;
while (var_12.vt == LV_PAIR) {
var_12 = (var_12.v.pair.cdr ? *(var_12.v.pair.cdr) : LVI_NIL());
}
var_12 = LVI_BOOL(var_12.vt == LV_NIL);
}
var_6 = var_12;
if (LV_IS_FALSE(var_6)) { break; }
} while (0);
if (LV_IS_FALSE(var_6)) {
var_5 = var_2[1];
} else {
lval var_13;
if (var_2[1].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_13 = (var_2[1].v.pair.car ? *(var_2[1].v.pair.car) : LVI_NIL());
var_5 = var_13;
}
var_4 = var_5;
lval var_14;
do {
lval var_15;
var_15 = LVI_BOOL(var_4.vt == LV_NIL);
if (!LV_IS_FALSE(var_15)) {
lval var_16;
var_16 = LVI_STRING("");
var_14 = var_16;
break;
}
lval var_17;
var_17 = LVI_STRING("");
lval var_18;
var_18 = LVI_BOOL(strcmp(var_2[0].v.str, var_17.v.str) == 0);
if (!LV_IS_FALSE(var_18)) {
lval var_25;
{
lval var_26[2];
var_26[0] = LVI_PROC(fn_19,NULL,-1);
var_26[1] = var_4;
var_25 = wile_gen_list(2, var_26, NULL);
}
var_25 = wile_apply_function(&(var_25), __FILE__, __LINE__);
var_14 = var_25;
break;
}
lval var_27;
lval var_28;
{
lval var_29[8];
var_29[0] = var_4;
var_28 = wile_list_reverse(NULL, var_29);
}
var_27 = var_28;
lval var_30;
lval var_31;
if (var_27.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_31 = (var_27.v.pair.car ? *(var_27.v.pair.car) : LVI_NIL());
lval var_32;
var_32 = LVI_NIL();
lval var_33;
{
lptr p1 = NULL, p2 = NULL;
if (var_31.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_31;
}
if (var_32.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_32;
}
var_33 = LVI_PAIR(p1, p2);
}
var_30 = var_33;
lval var_34;
if (var_27.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_34 = (var_27.v.pair.cdr ? *(var_27.v.pair.cdr) : LVI_NIL());
var_27 = var_34;
lval var_36;
lval var_37;
lval var_38;
var_38 = LVI_INT(0);
var_36 = var_38;
do {
lval var_39;
var_39 = LVI_BOOL(var_27.vt == LV_NIL);
if (!LV_IS_FALSE(var_39)) {
break;
}
lval var_40;
if (var_27.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_40 = (var_27.v.pair.car ? *(var_27.v.pair.car) : LVI_NIL());
lval var_41;
{
lptr p1 = NULL, p2 = NULL;
if (var_2[0].vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_2[0];
}
if (var_30.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_30;
}
var_41 = LVI_PAIR(p1, p2);
}
lval var_42;
{
lptr p1 = NULL, p2 = NULL;
if (var_40.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_40;
}
if (var_41.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_41;
}
var_42 = LVI_PAIR(p1, p2);
}
var_30 = var_42;
lval var_43;
if (var_27.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_43 = (var_27.v.pair.cdr ? *(var_27.v.pair.cdr) : LVI_NIL());
var_27 = var_43;
lval var_44;
var_44 = LVI_INT(1);
lval var_45;
var_45 = LVI_INT(var_36.v.iv + var_44.v.iv);
var_37 = var_45;
var_36 = var_37;
} while (1);
lval var_52;
{
lval var_53[2];
var_53[0] = LVI_PROC(fn_46,NULL,-1);
var_53[1] = var_30;
var_52 = wile_gen_list(2, var_53, NULL);
}
var_52 = wile_apply_function(&(var_52), __FILE__, __LINE__);
var_14 = var_52;
} while (0);
return var_14;
}
// end of function wile_string_join_by
