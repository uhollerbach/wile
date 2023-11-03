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

// @@@ (list-flatten lst) @@@ bld-rtl-dir/wile-rtl2-000009.scm:15 @@@ wile_list_flatten @@@
lval wile_list_flatten(lptr* var_1, lptr var_2)
{
lval var_4;
do {
lval var_5;
var_5 = LVI_BOOL(var_2[0].vt == LV_NIL);
if (!LV_IS_FALSE(var_5)) {
lval var_6;
var_6 = LVI_NIL();
var_4 = var_6;
break;
}
lval var_7;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_7 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
lval var_8;
{
var_8 = var_7;
while (var_8.vt == LV_PAIR) {
var_8 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
}
var_8 = LVI_BOOL(var_8.vt == LV_NIL);
}
if (!LV_IS_FALSE(var_8)) {
lval var_9;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_9 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
lval var_10;
lval var_11[8];
var_11[0] = var_9;
var_10 = wile_list_flatten(NULL, var_11);
lval var_13;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_13 = (var_2[0].v.pair.cdr ? *(var_2[0].v.pair.cdr) : LVI_NIL());
lval var_14;
lval var_15[8];
var_15[0] = var_13;
var_14 = wile_list_flatten(NULL, var_15);
lval var_17;
{
lval var_19[2];
var_19[0] = var_10;
var_19[1] = var_14;
var_17 = wile_gen_list(2, var_19, NULL);
}
{
lval var_18[8];
var_18[0] = var_17;
var_17 = wile_list_append(NULL, var_18);
}
var_4 = var_17;
break;
}
lval var_20;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_20 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
lval var_21;
if (var_2[0].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_21 = (var_2[0].v.pair.cdr ? *(var_2[0].v.pair.cdr) : LVI_NIL());
lval var_22;
lval var_23[8];
var_23[0] = var_21;
var_22 = wile_list_flatten(NULL, var_23);
lval var_25;
{
lptr p1 = NULL, p2 = NULL;
if (var_20.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_20;
}
if (var_22.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_22;
}
var_25 = LVI_PAIR(p1, p2);
}
var_4 = var_25;
} while (0);
return var_4;
}
// end of function wile_list_flatten
