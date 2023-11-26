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

// @@@ (foldl1 proc lst) @@@ bld-rtl-dir/wile-rtl2-000027.scm:13 @@@ wile_foldl1 @@@
lval wile_foldl1(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
if (var_2[1].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_4 = (var_2[1].v.pair.car ? *(var_2[1].v.pair.car) : LVI_NIL());
lval var_5;
if (var_2[1].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_5 = (var_2[1].v.pair.cdr ? *(var_2[1].v.pair.cdr) : LVI_NIL());
lval var_6;
{
lval var_7[8];
var_7[0] = var_2[0];
var_7[1] = var_4;
var_7[2] = var_5;
// bld-rtl-dir/wile-rtl2-000027.scm:14
var_6 = wile_foldl(NULL, var_7, "bld-rtl-dir/wile-rtl2-000027.scm:14");
}
return var_6;
}
// end of function wile_foldl1
