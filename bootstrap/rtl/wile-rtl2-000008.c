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

// @@@ (list-append . lsts) @@@ bld-rtl-dir/wile-rtl2-000008.scm:15 @@@ wile_list_append @@@
lval wile_list_append(lptr* var_1, lptr var_2, const char* cloc)
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
lval var_10[8];
var_10[0] = var_8;
var_9 = wile_list_reverse(NULL, var_10, "bld-rtl-dir/wile-rtl2-000008.scm:20");
}
var_8 = var_9;
lval var_11;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000008.scm:21", "input is not a pair!");
}
var_11 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
var_6 = var_11;
lval var_12;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000008.scm:22", "input is not a pair!");
}
var_12 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_12;
lval var_14;
lval var_15;
lval var_16;
var_16 = LVI_INT(0);
var_14 = var_16;
lptr var_17 = new_lv(VT_UNINIT);
var_17->v.pair.car = &(var_14);
do {
lval var_18;
var_18 = LVI_BOOL(var_8.vt == LV_NIL);
lval var_19;
var_19 = LVI_BOOL(LV_IS_FALSE(var_18));
lval var_20;
var_20 = LVI_BOOL(LV_IS_FALSE(var_19));
if (!LV_IS_FALSE(var_20)) {
break;
}
lval var_21;
lval var_22;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000008.scm:24", "input is not a pair!");
}
var_22 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_23;
{
lval var_24[8];
var_24[0] = var_22;
var_23 = wile_list_reverse(NULL, var_24, "bld-rtl-dir/wile-rtl2-000008.scm:24");
}
var_21 = var_23;
lval var_26;
lval var_27;
lval var_28;
var_28 = LVI_INT(0);
var_26 = var_28;
lptr var_29 = new_lv(VT_UNINIT);
var_29->v.pair.car = &(var_26);
do {
lval var_30;
var_30 = LVI_BOOL(var_21.vt == LV_NIL);
lval var_31;
var_31 = LVI_BOOL(LV_IS_FALSE(var_30));
lval var_32;
var_32 = LVI_BOOL(LV_IS_FALSE(var_31));
if (!LV_IS_FALSE(var_32)) {
break;
}
lval var_33;
if (var_21.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000008.scm:26", "input is not a pair!");
}
var_33 = (var_21.v.pair.car ? *(var_21.v.pair.car) : LVI_NIL());
lval var_34;
{
lptr p1 = NULL, p2 = NULL;
if (var_33.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_33;
}
if (var_6.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6;
}
var_34 = LVI_PAIR(p1, p2);
}
var_6 = var_34;
lval var_35;
if (var_21.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000008.scm:27", "input is not a pair!");
}
var_35 = (var_21.v.pair.cdr ? *(var_21.v.pair.cdr) : LVI_NIL());
var_21 = var_35;
lval var_36;
var_36 = LVI_INT(1);
lval var_37;
var_37 = LVI_INT(var_26.v.iv + var_36.v.iv);
var_27 = var_37;
var_26 = var_27;
} while (1);
*var_29 = var_26;
lval var_38;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000008.scm:28", "input is not a pair!");
}
var_38 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_38;
lval var_39;
var_39 = LVI_INT(1);
lval var_40;
var_40 = LVI_INT(var_14.v.iv + var_39.v.iv);
var_15 = var_40;
var_14 = var_15;
} while (1);
*var_17 = var_14;
var_4 = var_6;
} else {
lval var_41;
var_41 = LVI_NIL();
var_4 = var_41;
}
return var_4;
}
// end of function wile_list_append
