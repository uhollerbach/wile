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
static lval fn_4(lptr*, lptr);
static lval fn_9(lptr*, lptr);

// definitions

// @@@ lambda (c) @@@ bld-rtl-dir/wile-rtl2-000021.scm:17 @@@ fn_9 @@@
static lval fn_9(lptr* var_10, lptr var_11)
{
lval var_13;
{
lval vs[1];
vs[0] = var_11[0];
var_13 = gen_list(1, vs, NULL);
}
lval var_14;
{
lval vs[2];
vs[0] = V_CLOS(var_10,0);
vs[1] = var_13;
var_14 = gen_list(2, vs, NULL);
}
var_14 = wile_apply_function(&(var_14), __FILE__, __LINE__);
lval var_15;
var_15 = LVI_BOOL(LV_IS_FALSE(var_14));
return var_15;
}
// end of lambda fn_9

// @@@ lambda (l a) @@@ bld-rtl-dir/wile-rtl2-000021.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_5, lptr var_6)
{
lbl_7:;
lval var_8;
MK_CLOS(var_10,1);
P_CLOS(var_10,0) = &(V_CLOS(var_5,0));
lval var_16;
{
lval vs[8];
vs[0] = LVI_PROC(fn_9,var_10,1);
vs[1] = var_6[0];
var_16 = wile_list_take_while(NULL, vs);
}
var_8 = var_16;
lval var_17;
lval var_18;
var_18 = LVI_STRING("cadr");
lval var_19;
{
char* cp = strchr(var_18.v.str, 'r');
var_19 = var_8;
while (*(--cp) != 'c') {
if (var_19.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_19 = (var_19.v.pair.car ? *(var_19.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_19 = (var_19.v.pair.cdr ? *(var_19.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_18.v.str);
}
}
}
lval var_20;
{
lval vs[8];
vs[0] = V_CLOS(var_5,0);
vs[1] = var_19;
var_20 = wile_list_drop_while(NULL, vs);
}
var_17 = var_20;
lval var_21;
lval var_22;
if (var_8.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_22 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_23;
{
lval vs[1];
vs[0] = var_22;
var_23 = gen_list(1, vs, NULL);
}
{
lval vs[8];
vs[0] = var_23;
var_23 = wile_char2string(NULL, vs);
}
lval var_24;
{
lptr p1 = NULL, p2 = NULL;
if (var_23.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_23;
}
if (var_6[1].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6[1];
}
var_24 = LVI_PAIR(p1, p2);
}
var_21 = var_24;
lval var_25;
lval var_26;
{
lval vs[8];
vs[0] = var_17;
var_26 = wile_list_length(NULL, vs);
}
lval var_27;
switch (var_26.vt) {
case LV_REAL:
var_27 = LVI_BOOL(var_26.v.rv == 0.0);
break;
case LV_RAT:
var_27 = LVI_BOOL((var_26.v.irv.num == 0 && var_26.v.irv.den != 0));
break;
case LV_INT:
var_27 = LVI_BOOL(var_26.v.iv == 0);
break;
case LV_CMPLX:
var_27 = LVI_BOOL(CREAL(var_26.v.cv) == 0.0 && CIMAG(var_26.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (LV_IS_FALSE(var_27)) {
lval var_30[8];
var_30[0] = var_17;
var_30[1] = var_21;
var_6[0] = var_30[0];
var_6[1] = var_30[1];
goto lbl_7;	// selfie
} else {
lval var_31;
{
lval vs[8];
vs[0] = var_21;
var_31 = wile_list_reverse(NULL, vs);
}
var_25 = var_31;
}
return var_25;
}
// end of lambda fn_4

// @@@ (string-split-by drop? str) @@@ bld-rtl-dir/wile-rtl2-000021.scm:13 @@@ wile_string_split_by @@@
lval wile_string_split_by(lptr* var_1, lptr var_2)
{
MK_CLOS(var_5,1);
P_CLOS(var_5,0) = &(var_2[0]);
lval var_32;
{
lval vs[8];
vs[0] = var_2[1];
var_32 = wile_string2list(NULL, vs);
}
lval var_33;
{
lval vs[8];
vs[0] = var_2[0];
vs[1] = var_32;
var_33 = wile_list_drop_while(NULL, vs);
}
lval var_34;
var_34 = LVI_NIL();
lval var_35;
lval var_36[8];
var_36[0] = var_33;
var_36[1] = var_34;
var_35 = fn_4(var_5, var_36);
return var_35;
}
// end of function wile_string_split_by

// @@@ char-whitespace? @@@ bld-rtl-dir/wile-rtl2-000021.scm:27 @@@ fn_41 @@@
static lval fn_41(lptr* var_42, lptr var_43)
{
lval var_45;
var_45 = LVI_BOOL(isspace(var_43[0].v.chr));
return var_45;
}
// end of prim fn_41

// @@@ (string-split-by-whitespace str) @@@ bld-rtl-dir/wile-rtl2-000021.scm:26 @@@ wile_string_split_by_whitespace @@@
lval wile_string_split_by_whitespace(lptr* var_38, lptr var_39)
{
lval var_48[8];
var_48[0] = LVI_PROC(fn_41,NULL,1);
var_48[1] = var_39[0];
var_39[0] = var_48[0];
var_39[1] = var_48[1];
TAIL_CALL wile_string_split_by(NULL, var_39);
}
// end of function wile_string_split_by_whitespace
