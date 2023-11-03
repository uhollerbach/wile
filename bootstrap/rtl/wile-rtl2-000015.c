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

// @@@ (list-filter pred lst) @@@ bld-rtl-dir/wile-rtl2-000015.scm:15 @@@ wile_list_filter @@@
lval wile_list_filter(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_NIL();
var_4 = var_5;
lval var_6;
var_6 = var_2[1];
lval var_8;
lval var_9;
lval var_10;
var_10 = LVI_INT(0);
var_8 = var_10;
do {
lval var_11;
var_11 = LVI_BOOL(var_6.vt == LV_NIL);
if (!LV_IS_FALSE(var_11)) {
break;
}
lval var_12;
lval var_13;
if (var_6.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_13 = (var_6.v.pair.car ? *(var_6.v.pair.car) : LVI_NIL());
var_12 = var_13;
lval var_15;
{
lval var_16[1];
var_16[0] = var_12;
var_15 = wile_gen_list(1, var_16, NULL);
}
lval var_17;
{
lval var_18[2];
var_18[0] = var_2[0];
var_18[1] = var_15;
var_17 = wile_gen_list(2, var_18, NULL);
}
var_17 = wile_apply_function(&(var_17), __FILE__, __LINE__);
if (LV_IS_FALSE(var_17)) {
(void)
 LVI_BOOL(false);
} else {
lval var_20;
{
lptr p1 = NULL, p2 = NULL;
if (var_12.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_12;
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
if (var_6.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_21 = (var_6.v.pair.cdr ? *(var_6.v.pair.cdr) : LVI_NIL());
var_6 = var_21;
lval var_22;
var_22 = LVI_INT(1);
lval var_23;
var_23 = LVI_INT(var_8.v.iv + var_22.v.iv);
var_9 = var_23;
var_8 = var_9;
} while (1);
lval var_24;
{
lval var_25[8];
var_25[0] = var_4;
var_24 = wile_list_reverse(NULL, var_25);
}
return var_24;
}
// end of function wile_list_filter
