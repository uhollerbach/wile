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

// @@@ (bytevector->list vec) @@@ bld-rtl-dir/wile-rtl2-000021.scm:13 @@@ wile_bytevector2list @@@
lval wile_bytevector2list(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
{
if (var_2[0].vt != LV_BVECTOR) {
wile_exception("bytevector-length", "bld-rtl-dir/wile-rtl2-000021.scm:14", "input is not a bytevector");
}
var_5 = LVI_INT(var_2[0].v.bvec.capa);
}
var_4 = var_5;
lval var_6;
lval var_7;
var_7 = LVI_NIL();
var_6 = var_7;
lval var_11;
lval var_12;
lval var_13;
var_13 = LVI_INT(0);
var_11 = var_13;
lptr var_14 = new_lv(VT_UNINIT);
var_14->v.pair.car = &(var_11);
lbl_9:
lval var_15;
switch (var_4.vt) {
case LV_REAL:
var_15 = LVI_BOOL(var_4.v.rv == 0.0);
break;
case LV_RAT:
var_15 = LVI_BOOL((var_4.v.irv.num == 0 && var_4.v.irv.den != 0));
break;
case LV_INT:
var_15 = LVI_BOOL(var_4.v.iv == 0);
break;
case LV_CMPLX:
var_15 = LVI_BOOL(CREAL(var_4.v.cv) == 0.0 && CIMAG(var_4.v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000021.scm:16", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_15)) {
goto lbl_10;
}
lval var_16;
var_16 = LVI_INT(1);
lval var_17;
var_17 = LVI_INT(var_4.v.iv - var_16.v.iv);
var_4 = var_17;
lval var_18;
{
if (var_2[0].vt != LV_BVECTOR) {
wile_exception("bytevector-ref", "bld-rtl-dir/wile-rtl2-000021.scm:18", "input is not a bytevector");
}
if (var_4.vt != LV_INT || var_4.v.iv < 0 || (size_t) var_4.v.iv >= var_2[0].v.bvec.capa) {
wile_exception("bytevector-ref", "bld-rtl-dir/wile-rtl2-000021.scm:18", "got bad index value");
}
var_18 = LVI_INT(var_2[0].v.bvec.arr[var_4.v.iv]);
}
lval var_19;
{
lptr p1 = NULL, p2 = NULL;
if (var_18.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_18;
}
if (var_6.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6;
}
var_19 = LVI_PAIR(p1, p2);
}
var_6 = var_19;
lval var_20;
var_20 = LVI_INT(1);
lval var_21;
var_21 = LVI_INT(var_11.v.iv + var_20.v.iv);
var_12 = var_21;
var_11 = var_12;
goto lbl_9;
lbl_10:;
*var_14 = var_11;
return var_6;
}
// end of function wile_bytevector2list
