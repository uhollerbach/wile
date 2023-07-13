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

// (list-append . lsts)
lval wile_list_append(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_BOOL(var_2[0].vt == LV_NIL);
if (LV_IS_FALSE(var_5)) {
lval var_6;
lval var_7;
var_7 = LVI_NIL();
var_6 = var_7;
lval var_8;
var_8 = var_2[0];
lval var_9;
{
lval vs[6];
vs[0] = var_8;
var_9 = wile_list_reverse(NULL, vs);
}
var_8 = var_9;
lval var_10;
if (var_8.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_10 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
var_6 = var_10;
lval var_11;
if (var_8.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_11 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_11;
lval var_13;
lval var_14;
lval var_15;
var_15 = LVI_INT(0);
var_13 = var_15;
do {
lval var_16;
var_16 = LVI_BOOL(var_8.vt == LV_NIL);
lval var_17;
var_17 = LVI_BOOL(LV_IS_FALSE(var_16));
lval var_18;
var_18 = LVI_BOOL(LV_IS_FALSE(var_17));
if (!LV_IS_FALSE(var_18)) {
break;
}
lval var_19;
lval var_20;
if (var_8.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_20 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_21;
{
lval vs[6];
vs[0] = var_20;
var_21 = wile_list_reverse(NULL, vs);
}
var_19 = var_21;
lval var_23;
lval var_24;
lval var_25;
var_25 = LVI_INT(0);
var_23 = var_25;
do {
lval var_26;
var_26 = LVI_BOOL(var_19.vt == LV_NIL);
lval var_27;
var_27 = LVI_BOOL(LV_IS_FALSE(var_26));
lval var_28;
var_28 = LVI_BOOL(LV_IS_FALSE(var_27));
if (!LV_IS_FALSE(var_28)) {
break;
}
lval var_29;
if (var_19.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_29 = (var_19.v.pair.car ? *(var_19.v.pair.car) : LVI_NIL());
lval var_30;
{
lptr p1 = NULL, p2 = NULL;
if (var_29.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_29;
}
if (var_6.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6;
}
var_30 = LVI_PAIR(p1, p2);
}
var_6 = var_30;
lval var_31;
if (var_19.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_31 = (var_19.v.pair.cdr ? *(var_19.v.pair.cdr) : LVI_NIL());
var_19 = var_31;
lval var_32;
var_32 = LVI_INT(1);
lval var_33;
var_33 = LVI_INT(var_23.v.iv + var_32.v.iv);
var_24 = var_33;
var_23 = var_24;
} while (1);
lval var_34;
if (var_8.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_34 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_34;
lval var_35;
var_35 = LVI_INT(1);
lval var_36;
var_36 = LVI_INT(var_13.v.iv + var_35.v.iv);
var_14 = var_36;
var_13 = var_14;
} while (1);
var_4 = var_6;
} else {
lval var_37;
var_37 = LVI_NIL();
var_4 = var_37;
}
return var_4;
}
// end of function wile_list_append
