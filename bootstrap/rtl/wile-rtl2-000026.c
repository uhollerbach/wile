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

// @@@ lambda (c) @@@ bld-rtl-dir/wile-rtl2-000026.scm:17 @@@ fn_9 @@@
static lval fn_9(lptr* var_10, lptr var_11)
{
lval var_13;
{
lval var_14[1];
var_14[0] = var_11[0];
var_13 = wile_gen_list(1, var_14, NULL);
}
lval var_15;
{
lval var_16[2];
var_16[0] = V_CLOS(var_10,0);
var_16[1] = var_13;
var_15 = wile_gen_list(2, var_16, NULL);
}
var_15 = wile_apply_function(&(var_15), __FILE__, __LINE__);
lval var_17;
var_17 = LVI_BOOL(LV_IS_FALSE(var_15));
return var_17;
}
// end of lambda fn_9

// @@@ lambda (l a) @@@ bld-rtl-dir/wile-rtl2-000026.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_5, lptr var_6)
{
lbl_7:;
lval var_8;
MK_CLOS(var_10,1);
lptr var_18 = new_lv(VT_UNINIT);
var_18->v.pair.car = &(V_CLOS(var_5,0));
P_CLOS(var_10,0) = var_18;
lval var_19;
{
lval var_20[8];
var_20[0] = LVI_PROC(fn_9,var_10,1);
var_20[1] = var_6[0];
var_19 = wile_list_take_while(NULL, var_20);
}
var_8 = var_19;
lval var_21;
lval var_22;
var_22 = LVI_STRING("cadr");
lval var_23;
{
char* cp = strchr(var_22.v.str, 'r');
var_23 = var_8;
while (*(--cp) != 'c') {
if (var_23.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_23 = (var_23.v.pair.car ? *(var_23.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_23 = (var_23.v.pair.cdr ? *(var_23.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_22.v.str);
}
}
}
lval var_24;
{
lval var_25[8];
var_25[0] = V_CLOS(var_5,0);
var_25[1] = var_23;
var_24 = wile_list_drop_while(NULL, var_25);
}
var_21 = var_24;
lval var_26;
lval var_27;
if (var_8.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_27 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_28;
{
lval var_30[1];
var_30[0] = var_27;
var_28 = wile_gen_list(1, var_30, NULL);
}
{
lval var_29[8];
var_29[0] = var_28;
var_28 = wile_char2string(NULL, var_29);
}
lval var_31;
{
lptr p1 = NULL, p2 = NULL;
if (var_28.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_28;
}
if (var_6[1].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6[1];
}
var_31 = LVI_PAIR(p1, p2);
}
var_26 = var_31;
lval var_32;
lval var_33;
{
lval var_34[8];
var_34[0] = var_21;
var_33 = wile_list_length(NULL, var_34);
}
lval var_35;
switch (var_33.vt) {
case LV_REAL:
var_35 = LVI_BOOL(var_33.v.rv == 0.0);
break;
case LV_RAT:
var_35 = LVI_BOOL((var_33.v.irv.num == 0 && var_33.v.irv.den != 0));
break;
case LV_INT:
var_35 = LVI_BOOL(var_33.v.iv == 0);
break;
case LV_CMPLX:
var_35 = LVI_BOOL(CREAL(var_33.v.cv) == 0.0 && CIMAG(var_33.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (LV_IS_FALSE(var_35)) {
lval var_38[8];
var_38[0] = var_21;
var_38[1] = var_26;
var_6[0] = var_38[0];
var_6[1] = var_38[1];
goto lbl_7;	// selfie
} else {
lval var_39;
{
lval var_40[8];
var_40[0] = var_26;
var_39 = wile_list_reverse(NULL, var_40);
}
var_32 = var_39;
}
*var_18 = V_CLOS(var_5,0);
return var_32;
}
// end of lambda fn_4

// @@@ (string-split-by drop? str) @@@ bld-rtl-dir/wile-rtl2-000026.scm:13 @@@ wile_string_split_by @@@
lval wile_string_split_by(lptr* var_1, lptr var_2)
{
MK_CLOS(var_5,1);
lptr var_41 = new_lv(VT_UNINIT);
var_41->v.pair.car = &(var_2[0]);
P_CLOS(var_5,0) = var_41;
lval var_42;
{
lval var_43[8];
var_43[0] = var_2[1];
var_42 = wile_string2list(NULL, var_43);
}
lval var_44;
{
lval var_45[8];
var_45[0] = var_2[0];
var_45[1] = var_42;
var_44 = wile_list_drop_while(NULL, var_45);
}
lval var_46;
var_46 = LVI_NIL();
lval var_47;
lval var_48[8];
var_48[0] = var_44;
var_48[1] = var_46;
var_47 = fn_4(var_5, var_48);
*var_41 = var_2[0];
return var_47;
}
// end of function wile_string_split_by

// @@@ char-whitespace? @@@ bld-rtl-dir/wile-rtl2-000026.scm:27 @@@ fn_53 @@@
static lval fn_53(lptr* var_54, lptr var_55)
{
lval var_57;
var_57 = LVI_BOOL(isspace(var_55[0].v.chr));
return var_57;
}
// end of prim fn_53

// @@@ (string-split-by-whitespace str) @@@ bld-rtl-dir/wile-rtl2-000026.scm:26 @@@ wile_string_split_by_whitespace @@@
lval wile_string_split_by_whitespace(lptr* var_50, lptr var_51)
{
lval var_60[8];
var_60[0] = LVI_PROC(fn_53,NULL,1);
var_60[1] = var_51[0];
var_51[0] = var_60[0];
var_51[1] = var_60[1];
TAIL_CALL wile_string_split_by(NULL, var_51);
}
// end of function wile_string_split_by_whitespace
