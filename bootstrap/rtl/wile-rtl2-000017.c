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

// @@@ (foldr func end lst) @@@ bld-rtl-dir/wile-rtl2-000017.scm:13 @@@ wile_foldr @@@
lval wile_foldr(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_BOOL(var_2[2].vt == LV_NIL);
if (LV_IS_FALSE(var_5)) {
lval var_6;
if (var_2[2].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_6 = (var_2[2].v.pair.car ? *(var_2[2].v.pair.car) : LVI_NIL());
lval var_7;
if (var_2[2].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_7 = (var_2[2].v.pair.cdr ? *(var_2[2].v.pair.cdr) : LVI_NIL());
lval var_8;
lval var_9[8];
var_9[0] = var_2[0];
var_9[1] = var_2[1];
var_9[2] = var_7;
var_8 = wile_foldr(NULL, var_9);
lval var_11;
{
lval vs[2];
vs[0] = var_6;
vs[1] = var_8;
var_11 = gen_list(2, vs, NULL);
}
lval var_12;
{
lval vs[2];
vs[0] = var_2[0];
vs[1] = var_11;
var_12 = gen_list(2, vs, NULL);
}
var_12 = wile_apply_function(&(var_12), __FILE__, __LINE__);
var_4 = var_12;
} else {
var_4 = var_2[1];
}
return var_4;
}
// end of function wile_foldr