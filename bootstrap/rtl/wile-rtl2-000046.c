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

// @@@ (string-pad-center str pch lmin) @@@ bld-rtl-dir/wile-rtl2-000046.scm:13 @@@ wile_string_pad_center @@@
lval wile_string_pad_center(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_INT(strlen(var_2[0].v.str));
var_4 = var_5;
lval var_6;
lval var_7;
switch (TYPE_COMBO(var_4.vt,var_2[2].vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_7 = LVI_BOOL(var_4.v.iv < var_2[2].v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_7 = LVI_BOOL(var_4.v.iv * var_2[2].v.irv.den < var_2[2].v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_7 = LVI_BOOL(var_4.v.iv < var_2[2].v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_7 = LVI_BOOL(var_4.v.irv.num < var_2[2].v.iv * var_4.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_7 = LVI_BOOL(var_4.v.irv.num * var_2[2].v.irv.den < var_2[2].v.irv.num * var_4.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_7 = LVI_BOOL(var_4.v.irv.num < var_2[2].v.rv * var_4.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_7 = LVI_BOOL(var_4.v.rv < var_2[2].v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_7 = LVI_BOOL(var_4.v.rv * var_2[2].v.irv.den < var_2[2].v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_7 = LVI_BOOL(var_4.v.rv < var_2[2].v.rv);
break;
default:
WILE_EX("<", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_7)) {
var_6 = var_2[0];
} else {
lval var_8;
lval var_9;
var_9 = LVI_INT(var_2[2].v.iv - var_4.v.iv);
var_8 = var_9;
lval var_10;
lval var_11;
var_11 = LVI_INT(2);
lval var_12;
{
lisp_int_t nq, nr;
trunc_qr(var_8.v.iv, var_11.v.iv, &nq, &nr);
var_12 = LVI_INT(nq);
}
var_10 = var_12;
lval var_13;
lval var_14;
var_14 = LVI_INT(var_8.v.iv - var_10.v.iv);
var_13 = var_14;
lval var_15;
var_15 = LVI_STRING("");
lval var_16;
if (var_10.vt != LV_INT || var_10.v.iv < 0) {
WILE_EX("string-create", "first input is not a non-negative integer");
}
if (var_2[1].vt != LV_CHAR || var_2[1].v.chr == '\0') {
WILE_EX("string-create", "second input is not a valid character");
}
var_16.vt = LV_STRING;
var_16.v.str = LISP_ALLOC(char, 1 + var_10.v.iv);
LISP_ASSERT(var_16.v.str != NULL);
memset(var_16.v.str, var_2[1].v.chr, var_10.v.iv);
var_16.v.str[var_10.v.iv] = '\0';
lval var_17;
if (var_13.vt != LV_INT || var_13.v.iv < 0) {
WILE_EX("string-create", "first input is not a non-negative integer");
}
if (var_2[1].vt != LV_CHAR || var_2[1].v.chr == '\0') {
WILE_EX("string-create", "second input is not a valid character");
}
var_17.vt = LV_STRING;
var_17.v.str = LISP_ALLOC(char, 1 + var_13.v.iv);
LISP_ASSERT(var_17.v.str != NULL);
memset(var_17.v.str, var_2[1].v.chr, var_13.v.iv);
var_17.v.str[var_13.v.iv] = '\0';
lval var_18;
{
lval vs[3];
vs[0] = var_16;
vs[1] = var_2[0];
vs[2] = var_17;
var_18 = wile_gen_list(3, vs, NULL);
}
{
lval vs[8];
vs[0] = var_15;
vs[1] = var_18;
var_18 = wile_string_join_by(NULL, vs);
}
var_6 = var_18;
}
return var_6;
}
// end of function wile_string_pad_center
