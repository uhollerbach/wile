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

// (list-partition pred lst)
lval wile_list_partition(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_NIL();
var_4 = var_5;
lval var_6;
lval var_7;
var_7 = LVI_NIL();
var_6 = var_7;
lval var_8;
var_8 = var_2[1];
lval var_10;
lval var_11;
lval var_12;
var_12 = LVI_INT(0);
var_10 = var_12;
do {
lval var_13;
var_13 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_13)) {
break;
}
lval var_14;
lval var_15;
if (var_8.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_15 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
var_14 = var_15;
lval var_17;
{
lval vs[1];
vs[0] = var_14;
var_17 = gen_list(1, vs, NULL);
}
lval var_18;
{
lval vs[2];
vs[0] = var_2[0];
vs[1] = var_17;
var_18 = gen_list(2, vs, NULL);
}
var_18 = wile_apply_function(&(var_18), __FILE__, __LINE__);
if (LV_IS_FALSE(var_18)) {
lval var_19;
{
lptr p1 = NULL, p2 = NULL;
if (var_14.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_14;
}
if (var_6.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6;
}
var_19 = LVI_PAIR(p1, p2);
}
var_6 = var_19;
} else {
lval var_20;
{
lptr p1 = NULL, p2 = NULL;
if (var_14.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_14;
}
if (var_4.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_4;
}
var_20 = LVI_PAIR(p1, p2);
}
var_4 = var_20;
}
lval var_21;
if (var_8.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_21 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_21;
lval var_22;
var_22 = LVI_INT(1);
lval var_23;
var_23 = LVI_INT(var_10.v.iv + var_22.v.iv);
var_11 = var_23;
var_10 = var_11;
} while (1);
lval var_24;
{
lval vs[6];
vs[0] = var_4;
var_24 = wile_list_reverse(NULL, vs);
}
lval var_25;
{
lval vs[6];
vs[0] = var_6;
var_25 = wile_list_reverse(NULL, vs);
}
lval var_26;
{
lval vs[2];
vs[0] = var_24;
vs[1] = var_25;
var_26 = gen_list(2, vs, NULL);
}
return var_26;
}
// end of function wile_list_partition
