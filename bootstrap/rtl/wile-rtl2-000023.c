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

// @@@ (foldr func end lst) @@@ bld-rtl-dir/wile-rtl2-000023.scm:13 @@@ wile_foldr @@@
lval wile_foldr(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
var_5 = LVI_BOOL(var_2[2].vt == LV_NIL);
if (LV_IS_FALSE(var_5)) {
lval var_6;
if (var_2[2].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000023.scm:16", "input is not a pair!");
}
var_6 = (var_2[2].v.pair.car ? *(var_2[2].v.pair.car) : LVI_NIL());
lval var_7;
if (var_2[2].vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000023.scm:16", "input is not a pair!");
}
var_7 = (var_2[2].v.pair.cdr ? *(var_2[2].v.pair.cdr) : LVI_NIL());
lval var_8;
lval var_9[8];
var_9[0] = var_2[0];
var_9[1] = var_2[1];
var_9[2] = var_7;
var_8 = wile_foldr(NULL, var_9, "bld-rtl-dir/wile-rtl2-000023.scm:16");
lval var_11;
{
lval var_12[2];
var_12[0] = var_6;
var_12[1] = var_8;
var_11 = wile_gen_list(2, var_12, NULL);
}
lval var_13;
{
lval var_14[2];
var_14[0] = var_2[0];
var_14[1] = var_11;
var_13 = wile_gen_list(2, var_14, NULL);
}
var_13 = wile_apply_function(&(var_13), "bld-rtl-dir/wile-rtl2-000023.scm:16");
var_4 = var_13;
} else {
var_4 = var_2[1];
}
return var_4;
}
// end of function wile_foldr
