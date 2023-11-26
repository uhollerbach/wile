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

// @@@ (list-reverse lst) @@@ bld-rtl-dir/wile-rtl2-000000.scm:15 @@@ wile_list_reverse @@@
lval wile_list_reverse(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
var_5 = LVI_NIL();
var_4 = var_5;
lval var_6;
var_6 = var_2[0];
lval var_8;
lval var_9;
lval var_10;
var_10 = LVI_INT(0);
var_8 = var_10;
lptr var_11 = new_lv(VT_UNINIT);
var_11->v.pair.car = &(var_8); //  symbol.3
do {
lval var_12;
var_12 = LVI_BOOL(var_6.vt == LV_NIL);
if (!LV_IS_FALSE(var_12)) {
break;
}
lval var_13;
if (var_6.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_13 = (var_6.v.pair.car ? *(var_6.v.pair.car) : LVI_NIL());
lval var_14;
{
lptr p1 = NULL, p2 = NULL;
if (var_13.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_13;
}
if (var_4.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_4;
}
var_14 = LVI_PAIR(p1, p2);
}
var_4 = var_14;
lval var_15;
if (var_6.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_15 = (var_6.v.pair.cdr ? *(var_6.v.pair.cdr) : LVI_NIL());
var_6 = var_15;
lval var_16;
var_16 = LVI_INT(1);
lval var_17;
var_17 = LVI_INT(var_8.v.iv + var_16.v.iv);
var_9 = var_17;
var_8 = var_9;
} while (1);
*var_11 = var_8;
return var_4;
}
// end of function wile_list_reverse
