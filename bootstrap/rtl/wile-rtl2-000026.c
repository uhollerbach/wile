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

// @@@ string-append @@@ bld-rtl-dir/wile-rtl2-000026.scm:20 @@@ fn_20 @@@
static lval fn_20(lptr* var_21, lptr var_22, const char* cloc)
{
lval var_24;
var_24 = var_22[0];
{
lval var_25[8];
var_25[0] = var_24;
var_24 = wile_string_append(NULL, var_25, "bld-rtl-dir/wile-rtl2-000026.scm:20");
}
return var_24;
}
// end of prim fn_20

// @@@ (string-join-by join . strs) @@@ bld-rtl-dir/wile-rtl2-000026.scm:13 @@@ wile_string_join_by @@@
lval wile_string_join_by(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
lval var_6;
var_6 = LVI_BOOL(true);
lval var_8;
var_8 = LVI_INT(1);
lval var_9;
{
lval var_10[8];
var_10[0] = var_8;
var_10[1] = var_2[1];
var_9 = wile_list_length_eq(NULL, var_10, "bld-rtl-dir/wile-rtl2-000026.scm:14");
}
var_6 = var_9;
if (LV_IS_FALSE(var_6)) {
goto lbl_7;
}
lval var_11;
if (var_2[1].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000026.scm:14", "input is not a pair!");
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
if (LV_IS_FALSE(var_6)) {
goto lbl_7;
}
lbl_7:;
if (LV_IS_FALSE(var_6)) {
var_5 = var_2[1];
} else {
lval var_13;
if (var_2[1].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000026.scm:15", "input is not a pair!");
}
var_13 = (var_2[1].v.pair.car ? *(var_2[1].v.pair.car) : LVI_NIL());
var_5 = var_13;
}
var_4 = var_5;
lval var_14;
lval var_16;
var_16 = LVI_BOOL(var_4.vt == LV_NIL);
if (!LV_IS_FALSE(var_16)) {
lval var_17;
var_17 = LVI_STRING("");
var_14 = var_17;
goto lbl_15;
}
lval var_18;
var_18 = LVI_STRING("");
lval var_19;
var_19 = LVI_BOOL(strcmp(var_2[0].v.str, var_18.v.str) == 0);
if (!LV_IS_FALSE(var_19)) {
lval var_26;
{
lval var_27[2];
var_27[0] = LVI_PROC(fn_20,NULL,-1);
var_27[1] = var_4;
var_26 = wile_gen_list(2, var_27, NULL);
}
var_26 = wile_apply_function(&(var_26), "bld-rtl-dir/wile-rtl2-000026.scm:20");
var_14 = var_26;
goto lbl_15;
}
lval var_28;
lval var_29;
{
lval var_30[8];
var_30[0] = var_4;
var_29 = wile_list_reverse(NULL, var_30, "bld-rtl-dir/wile-rtl2-000026.scm:21");
}
var_28 = var_29;
lval var_31;
lval var_32;
if (var_28.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000026.scm:22", "input is not a pair!");
}
var_32 = (var_28.v.pair.car ? *(var_28.v.pair.car) : LVI_NIL());
lval var_33;
var_33 = LVI_NIL();
lval var_34;
{
lptr p1 = NULL, p2 = NULL;
if (var_32.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_32;
}
if (var_33.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_33;
}
var_34 = LVI_PAIR(p1, p2);
}
var_31 = var_34;
lval var_35;
if (var_28.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000026.scm:23", "input is not a pair!");
}
var_35 = (var_28.v.pair.cdr ? *(var_28.v.pair.cdr) : LVI_NIL());
var_28 = var_35;
lval var_39;
lval var_40;
lval var_41;
var_41 = LVI_INT(0);
var_39 = var_41;
lptr var_42 = new_lv(VT_UNINIT);
var_42->v.pair.car = &(var_39);
lbl_37:
lval var_43;
var_43 = LVI_BOOL(var_28.vt == LV_NIL);
if (!LV_IS_FALSE(var_43)) {
goto lbl_38;
}
lval var_44;
if (var_28.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000026.scm:25", "input is not a pair!");
}
var_44 = (var_28.v.pair.car ? *(var_28.v.pair.car) : LVI_NIL());
lval var_45;
{
lptr p1 = NULL, p2 = NULL;
if (var_2[0].vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_2[0];
}
if (var_31.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_31;
}
var_45 = LVI_PAIR(p1, p2);
}
lval var_46;
{
lptr p1 = NULL, p2 = NULL;
if (var_44.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_44;
}
if (var_45.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_45;
}
var_46 = LVI_PAIR(p1, p2);
}
var_31 = var_46;
lval var_47;
if (var_28.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000026.scm:26", "input is not a pair!");
}
var_47 = (var_28.v.pair.cdr ? *(var_28.v.pair.cdr) : LVI_NIL());
var_28 = var_47;
lval var_48;
var_48 = LVI_INT(1);
lval var_49;
var_49 = LVI_INT(var_39.v.iv + var_48.v.iv);
var_40 = var_49;
var_39 = var_40;
goto lbl_37;
lbl_38:;
*var_42 = var_39;
lval var_50;
{
lval var_51[2];
var_51[0] = LVI_PROC(fn_20,NULL,-1);
var_51[1] = var_31;
var_50 = wile_gen_list(2, var_51, NULL);
}
var_50 = wile_apply_function(&(var_50), "bld-rtl-dir/wile-rtl2-000026.scm:27");
var_14 = var_50;
lbl_15:;
return var_14;
}
// end of function wile_string_join_by
