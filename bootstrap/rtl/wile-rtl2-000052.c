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

// @@@ (any-true? vals) @@@ bld-rtl-dir/wile-rtl2-000052.scm:13 @@@ wile_any_true @@@
lval wile_any_true(lptr* var_1, lptr var_2, const char* cloc)
{
lbl_3:;
lval var_4;
lval var_6;
var_6 = LVI_BOOL(var_2[0].vt == LV_NIL);
if (!LV_IS_FALSE(var_6)) {
lval var_7;
var_7 = LVI_BOOL(false);
var_4 = var_7;
goto lbl_5;
}
lval var_8;
if (var_2[0].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000052.scm:15", "input is not a pair!");
}
var_8 = (var_2[0].v.pair.car ? *(var_2[0].v.pair.car) : LVI_NIL());
if (!LV_IS_FALSE(var_8)) {
lval var_9;
var_9 = LVI_BOOL(true);
var_4 = var_9;
goto lbl_5;
}
lval var_10;
if (var_2[0].vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000052.scm:16", "input is not a pair!");
}
var_10 = (var_2[0].v.pair.cdr ? *(var_2[0].v.pair.cdr) : LVI_NIL());
lval var_11;
lval var_13[8];
var_13[0] = var_10;
var_2[0] = var_13[0];
goto lbl_3;
var_4 = var_11;
lbl_5:;
return var_4;
}
// end of function wile_any_true
