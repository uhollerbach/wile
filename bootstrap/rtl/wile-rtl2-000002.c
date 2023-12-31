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

// @@@ (list-length=? n lst) @@@ bld-rtl-dir/wile-rtl2-000002.scm:18 @@@ wile_list_length_eq @@@
lval wile_list_length_eq(lptr* var_1, lptr var_2, const char* cloc)
{
lbl_3:;
lval var_4;
do {
lval var_5;
var_5 = LVI_BOOL(true);
do {
lval var_6;
switch (var_2[0].vt) {
case LV_REAL:
var_6 = LVI_BOOL(var_2[0].v.rv == 0.0);
break;
case LV_RAT:
var_6 = LVI_BOOL((var_2[0].v.irv.num == 0 && var_2[0].v.irv.den != 0));
break;
case LV_INT:
var_6 = LVI_BOOL(var_2[0].v.iv == 0);
break;
case LV_CMPLX:
var_6 = LVI_BOOL(CREAL(var_2[0].v.cv) == 0.0 && CIMAG(var_2[0].v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000002.scm:19", "expects a real-valued number");
}
var_5 = var_6;
if (LV_IS_FALSE(var_5)) { break; }
lval var_7;
var_7 = LVI_BOOL(var_2[1].vt == LV_NIL);
var_5 = var_7;
if (LV_IS_FALSE(var_5)) { break; }
} while (0);
if (!LV_IS_FALSE(var_5)) {
lval var_8;
var_8 = LVI_BOOL(true);
var_4 = var_8;
break;
}
lval var_9;
var_9 = LVI_BOOL(false);
do {
lval var_10;
switch (var_2[0].vt) {
case LV_REAL:
var_10 = LVI_BOOL(var_2[0].v.rv == 0.0);
break;
case LV_RAT:
var_10 = LVI_BOOL((var_2[0].v.irv.num == 0 && var_2[0].v.irv.den != 0));
break;
case LV_INT:
var_10 = LVI_BOOL(var_2[0].v.iv == 0);
break;
case LV_CMPLX:
var_10 = LVI_BOOL(CREAL(var_2[0].v.cv) == 0.0 && CIMAG(var_2[0].v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000002.scm:20", "expects a real-valued number");
}
var_9 = var_10;
if (!LV_IS_FALSE(var_9)) { break; }
lval var_11;
var_11 = LVI_BOOL(var_2[1].vt == LV_NIL);
var_9 = var_11;
if (!LV_IS_FALSE(var_9)) { break; }
} while (0);
if (!LV_IS_FALSE(var_9)) {
lval var_12;
var_12 = LVI_BOOL(false);
var_4 = var_12;
break;
}
lval var_13;
var_13 = LVI_INT(1);
lval var_14;
{
lval var_16[2];
var_16[0] = var_2[0];
var_16[1] = var_13;
var_14 = wile_gen_list(2, var_16, NULL);
}
{
lval var_15[8];
var_15[0] = var_14;
var_14 = wile_subtract(NULL, var_15, "bld-rtl-dir/wile-rtl2-000002.scm:21");
}
lval var_17;
if (var_2[1].vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000002.scm:21", "input is not a pair!");
}
var_17 = (var_2[1].v.pair.cdr ? *(var_2[1].v.pair.cdr) : LVI_NIL());
lval var_20[8];
var_20[0] = var_14;
var_20[1] = var_17;
var_2[0] = var_20[0];
var_2[1] = var_20[1];
goto lbl_3;
} while (0);
return var_4;
}
// end of function wile_list_length_eq
