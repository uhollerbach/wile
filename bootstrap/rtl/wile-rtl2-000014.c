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

// @@@ (list-ref lst n) @@@ bld-rtl-dir/wile-rtl2-000014.scm:13 @@@ wile_list_ref @@@
lval wile_list_ref(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
var_4 = var_2[0];
lval var_5;
var_5 = var_2[1];
lval var_7;
switch (var_5.vt) {
case LV_REAL:
var_7 = LVI_BOOL(var_5.v.rv < 0.0);
break;
case LV_RAT:
var_7 = LVI_BOOL((var_5.v.irv.num < 0 && var_5.v.irv.den >= 0) || (var_5.v.irv.num > 0 && var_5.v.irv.den < 0));
break;
case LV_INT:
var_7 = LVI_BOOL(var_5.v.iv < 0);
break;
default:
wile_exception("negative?", "bld-rtl-dir/wile-rtl2-000014.scm:16", "expects a real-valued number");
}
if (LV_IS_FALSE(var_7)) {
} else {
lval var_9;
{
lval var_10[8];
var_10[0] = var_4;
var_9 = wile_list_length(NULL, var_10, "bld-rtl-dir/wile-rtl2-000014.scm:17");
}
lval var_11;
var_11 = LVI_INT(var_9.v.iv + var_5.v.iv);
var_5 = var_11;
}
lval var_12;
lval var_13;
switch (var_5.vt) {
case LV_REAL:
var_13 = LVI_BOOL(var_5.v.rv < 0.0);
break;
case LV_RAT:
var_13 = LVI_BOOL((var_5.v.irv.num < 0 && var_5.v.irv.den >= 0) || (var_5.v.irv.num > 0 && var_5.v.irv.den < 0));
break;
case LV_INT:
var_13 = LVI_BOOL(var_5.v.iv < 0);
break;
default:
wile_exception("negative?", "bld-rtl-dir/wile-rtl2-000014.scm:18", "expects a real-valued number");
}
if (LV_IS_FALSE(var_13)) {
lval var_15;
lval var_16;
var_15 = var_5;
lval var_14;
lptr var_17 = new_lv(VT_UNINIT);
var_17->v.pair.car = &(var_15);
do {
lval var_18;
var_18 = LVI_BOOL(false);
do {
lval var_19;
switch (var_15.vt) {
case LV_REAL:
var_19 = LVI_BOOL(var_15.v.rv == 0.0);
break;
case LV_RAT:
var_19 = LVI_BOOL((var_15.v.irv.num == 0 && var_15.v.irv.den != 0));
break;
case LV_INT:
var_19 = LVI_BOOL(var_15.v.iv == 0);
break;
case LV_CMPLX:
var_19 = LVI_BOOL(CREAL(var_15.v.cv) == 0.0 && CIMAG(var_15.v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000014.scm:21", "expects a real-valued number");
}
var_18 = var_19;
if (!LV_IS_FALSE(var_18)) { break; }
lval var_20;
var_20 = LVI_BOOL(var_4.vt == LV_NIL);
var_18 = var_20;
if (!LV_IS_FALSE(var_18)) { break; }
} while (0);
if (!LV_IS_FALSE(var_18)) {
lval var_21;
lval var_22;
var_22 = LVI_BOOL(var_4.vt == LV_NIL);
if (LV_IS_FALSE(var_22)) {
lval var_23;
if (var_4.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000014.scm:21", "input is not a pair!");
}
var_23 = (var_4.v.pair.car ? *(var_4.v.pair.car) : LVI_NIL());
var_21 = var_23;
} else {
lval var_24;
var_24 = LVI_NIL();
var_21 = var_24;
}
var_14 = var_21;
break;
}
lval var_25;
if (var_4.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000014.scm:22", "input is not a pair!");
}
var_25 = (var_4.v.pair.cdr ? *(var_4.v.pair.cdr) : LVI_NIL());
var_4 = var_25;
lval var_26;
var_26 = LVI_INT(1);
lval var_27;
var_27 = LVI_INT(var_15.v.iv - var_26.v.iv);
var_16 = var_27;
var_15 = var_16;
} while (1);
*var_17 = var_15;
var_12 = var_14;
} else {
lval var_28;
var_28 = LVI_NIL();
var_12 = var_28;
}
return var_12;
}
// end of function wile_list_ref
