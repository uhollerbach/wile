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
static lval fn_4(lptr*, lptr, const char*);

// definitions

// @@@ lambda (lst acc) @@@ bld-rtl-dir/wile-rtl2-000042.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_5, lptr var_6, const char* cloc)
{
lbl_7:;
lval var_8;
lval var_9;
var_9 = LVI_BOOL(true);
do {
lval var_10;
var_10 = LVI_BOOL(var_6[0].vt == LV_NIL);
lval var_11;
var_11 = LVI_BOOL(LV_IS_FALSE(var_10));
var_9 = var_11;
if (LV_IS_FALSE(var_9)) { break; }
lval var_12;
if (var_6[0].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000042.scm:16", "input is not a pair!");
}
var_12 = (var_6[0].v.pair.car ? *(var_6[0].v.pair.car) : LVI_NIL());
lval var_13;
{
lval var_14[1];
var_14[0] = var_12;
var_13 = wile_gen_list(1, var_14, NULL);
}
lval var_15;
{
lval var_16[2];
var_16[0] = V_CLOS(var_5,0);
var_16[1] = var_13;
var_15 = wile_gen_list(2, var_16, NULL);
}
var_15 = wile_apply_function(&(var_15), "bld-rtl-dir/wile-rtl2-000042.scm:16");
var_9 = var_15;
if (LV_IS_FALSE(var_9)) { break; }
} while (0);
if (LV_IS_FALSE(var_9)) {
lval var_17;
{
lval var_18[8];
var_18[0] = var_6[1];
var_17 = wile_list_reverse(NULL, var_18, "bld-rtl-dir/wile-rtl2-000042.scm:18");
}
var_8 = var_17;
} else {
lval var_19;
if (var_6[0].vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000042.scm:17", "input is not a pair!");
}
var_19 = (var_6[0].v.pair.cdr ? *(var_6[0].v.pair.cdr) : LVI_NIL());
lval var_20;
if (var_6[0].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000042.scm:17", "input is not a pair!");
}
var_20 = (var_6[0].v.pair.car ? *(var_6[0].v.pair.car) : LVI_NIL());
lval var_21;
{
lptr p1 = NULL, p2 = NULL;
if (var_20.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_20;
}
if (var_6[1].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6[1];
}
var_21 = LVI_PAIR(p1, p2);
}
lval var_24[8];
var_24[0] = var_19;
var_24[1] = var_21;
var_6[0] = var_24[0];
var_6[1] = var_24[1];
goto lbl_7;
}
return var_8;
}
// end of lambda fn_4

// @@@ (list-take-while keep? lst) @@@ bld-rtl-dir/wile-rtl2-000042.scm:13 @@@ wile_list_take_while @@@
lval wile_list_take_while(lptr* var_1, lptr var_2, const char* cloc)
{
MK_CLOS(var_5,1);
lptr var_25 = new_lv(VT_UNINIT);
var_25->v.pair.car = &(var_2[0]);
P_CLOS(var_5,0) = var_25;
lval var_26;
var_26 = LVI_NIL();
lval var_27;
lval var_28[8];
var_28[0] = var_2[1];
var_28[1] = var_26;
var_27 = fn_4(var_5, var_28, "bld-rtl-dir/wile-rtl2-000042.scm:14");
*var_25 = var_2[0];
return var_27;
}
// end of function wile_list_take_while
