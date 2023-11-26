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

// @@@ (list-partition pred lst) @@@ bld-rtl-dir/wile-rtl2-000016.scm:15 @@@ wile_list_partition @@@
lval wile_list_partition(lptr* var_1, lptr var_2, const char* cloc)
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
lptr var_13 = new_lv(VT_UNINIT);
var_13->v.pair.car = &(var_10); //  symbol.3
do {
lval var_14;
var_14 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_14)) {
break;
}
lval var_15;
lval var_16;
if (var_8.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_16 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
var_15 = var_16;
lval var_18;
{
lval var_19[1];
var_19[0] = var_15;
var_18 = wile_gen_list(1, var_19, NULL);
}
lval var_20;
{
lval var_21[2];
var_21[0] = var_2[0];
var_21[1] = var_18;
var_20 = wile_gen_list(2, var_21, NULL);
}
var_20 = wile_apply_function(&(var_20), LISP_WHENCE);
if (LV_IS_FALSE(var_20)) {
lval var_22;
{
lptr p1 = NULL, p2 = NULL;
if (var_15.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_15;
}
if (var_6.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6;
}
var_22 = LVI_PAIR(p1, p2);
}
var_6 = var_22;
} else {
lval var_23;
{
lptr p1 = NULL, p2 = NULL;
if (var_15.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_15;
}
if (var_4.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_4;
}
var_23 = LVI_PAIR(p1, p2);
}
var_4 = var_23;
}
lval var_24;
if (var_8.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_24 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_24;
lval var_25;
var_25 = LVI_INT(1);
lval var_26;
var_26 = LVI_INT(var_10.v.iv + var_25.v.iv);
var_11 = var_26;
var_10 = var_11;
} while (1);
*var_13 = var_10;
lval var_27;
{
lval var_28[8];
var_28[0] = var_4;
// bld-rtl-dir/wile-rtl2-000016.scm:25
var_27 = wile_list_reverse(NULL, var_28, "bld-rtl-dir/wile-rtl2-000016.scm:25");
}
lval var_29;
{
lval var_30[8];
var_30[0] = var_6;
// bld-rtl-dir/wile-rtl2-000016.scm:25
var_29 = wile_list_reverse(NULL, var_30, "bld-rtl-dir/wile-rtl2-000016.scm:25");
}
lval var_31;
{
lval var_32[2];
var_32[0] = var_27;
var_32[1] = var_29;
var_31 = wile_gen_list(2, var_32, NULL);
}
return var_31;
}
// end of function wile_list_partition
