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

// @@@ (list-length>=? n lst) @@@ bld-rtl-dir/wile-rtl2-000003.scm:13 @@@ wile_list_length_ge @@@
lval wile_list_length_ge(lptr* var_1, lptr var_2, const char* cloc)
{
lbl_3:;
lval var_4;
do {
lval var_5;
switch (var_2[0].vt) {
case LV_REAL:
var_5 = LVI_BOOL(var_2[0].v.rv == 0.0);
break;
case LV_RAT:
var_5 = LVI_BOOL((var_2[0].v.irv.num == 0 && var_2[0].v.irv.den != 0));
break;
case LV_INT:
var_5 = LVI_BOOL(var_2[0].v.iv == 0);
break;
case LV_CMPLX:
var_5 = LVI_BOOL(CREAL(var_2[0].v.cv) == 0.0 && CIMAG(var_2[0].v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_5)) {
lval var_6;
var_6 = LVI_BOOL(true);
var_4 = var_6;
break;
}
lval var_7;
var_7 = LVI_BOOL(var_2[1].vt == LV_NIL);
if (!LV_IS_FALSE(var_7)) {
lval var_8;
var_8 = LVI_BOOL(false);
var_4 = var_8;
break;
}
lval var_9;
var_9 = LVI_INT(1);
lval var_10;
{
lval var_12[2];
var_12[0] = var_2[0];
var_12[1] = var_9;
var_10 = wile_gen_list(2, var_12, NULL);
}
{
lval var_11[8];
var_11[0] = var_10;
// bld-rtl-dir/wile-rtl2-000003.scm:16
var_10 = wile_subtract(NULL, var_11, "bld-rtl-dir/wile-rtl2-000003.scm:16");
}
lval var_13;
if (var_2[1].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_13 = (var_2[1].v.pair.cdr ? *(var_2[1].v.pair.cdr) : LVI_NIL());
lval var_16[8];
var_16[0] = var_10;
var_16[1] = var_13;
var_2[0] = var_16[0];
var_2[1] = var_16[1];
// bld-rtl-dir/wile-rtl2-000003.scm:16
goto lbl_3;	// selfie
} while (0);
return var_4;
}
// end of function wile_list_length_ge
