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
static lval fn_10(lptr*, lptr, const char*);

// definitions

// @@@ lambda (x) @@@ bld-rtl-dir/wile-rtl2-000043.scm:18 @@@ fn_10 @@@
static lval fn_10(lptr* var_11, lptr var_12, const char* cloc)
{
lval var_14;
if (V_CLOS(var_11,0).vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000043.scm:18", "input is not a pair!");
}
var_14 = (V_CLOS(var_11,0).v.pair.car ? *(V_CLOS(var_11,0).v.pair.car) : LVI_NIL());
lval var_15;
var_15 = LVI_BOOL(wile_do_eqv(&(var_12[0]), &(var_14)));
return var_15;
}
// end of lambda fn_10

// @@@ lambda (lst acc) @@@ bld-rtl-dir/wile-rtl2-000043.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_5, lptr var_6, const char* cloc)
{
lbl_7:;
lval var_8;
lval var_9;
var_9 = LVI_BOOL(var_6[0].vt == LV_NIL);
if (LV_IS_FALSE(var_9)) {
MK_CLOS(var_11,1);
lptr var_16 = new_lv(VT_UNINIT);
var_16->v.pair.car = &(var_6[0]);
P_CLOS(var_11,0) = var_16;
lval var_17;
if (var_6[0].vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000043.scm:18", "input is not a pair!");
}
var_17 = (var_6[0].v.pair.cdr ? *(var_6[0].v.pair.cdr) : LVI_NIL());
lval var_18;
{
lval var_19[8];
var_19[0] = LVI_PROC(fn_10,var_11,1);
var_19[1] = var_17;
var_18 = wile_list_drop_while(NULL, var_19, "bld-rtl-dir/wile-rtl2-000043.scm:18");
}
lval var_20;
if (var_6[0].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000043.scm:19", "input is not a pair!");
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
lval var_22;
lval var_24[8];
var_24[0] = var_18;
var_24[1] = var_21;
*var_16 = var_6[0];
var_6[0] = var_24[0];
var_6[1] = var_24[1];
goto lbl_7;
var_8 = var_22;
} else {
lval var_25;
{
lval var_26[8];
var_26[0] = var_6[1];
var_25 = wile_list_reverse(NULL, var_26, "bld-rtl-dir/wile-rtl2-000043.scm:17");
}
var_8 = var_25;
}
return var_8;
}
// end of lambda fn_4

// @@@ (list-remove-dups lst) @@@ bld-rtl-dir/wile-rtl2-000043.scm:13 @@@ wile_list_remove_dups @@@
lval wile_list_remove_dups(lptr* var_1, lptr var_2, const char* cloc)
{
MK_CLOS(var_5,0);
lval var_27;
var_27 = LVI_NIL();
lval var_28;
lval var_29[8];
var_29[0] = var_2[0];
var_29[1] = var_27;
var_28 = fn_4(var_5, var_29, "bld-rtl-dir/wile-rtl2-000043.scm:14");
return var_28;
}
// end of function wile_list_remove_dups
