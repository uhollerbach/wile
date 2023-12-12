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

// @@@ (string-pad-left str pch lmin) @@@ bld-rtl-dir/wile-rtl2-000044.scm:13 @@@ wile_string_pad_left @@@
lval wile_string_pad_left(lptr* var_1, lptr var_2, const char* cloc)
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
var_8 = LVI_STRING("");
lval var_9;
var_9 = LVI_INT(var_2[2].v.iv - var_4.v.iv);
lval var_10;
if (var_9.vt != LV_INT || var_9.v.iv < 0) {
WILE_EX("string-create", "first input is not a non-negative integer");
}
if (var_2[1].vt != LV_CHAR || var_2[1].v.chr == '\0') {
WILE_EX("string-create", "second input is not a valid character");
}
var_10.vt = LV_STRING;
var_10.v.str = LISP_ALLOC(char, 1 + var_9.v.iv);
memset(var_10.v.str, var_2[1].v.chr, var_9.v.iv);
var_10.v.str[var_9.v.iv] = '\0';
lval var_11;
{
lval var_13[2];
var_13[0] = var_10;
var_13[1] = var_2[0];
var_11 = wile_gen_list(2, var_13, NULL);
}
{
lval var_12[8];
var_12[0] = var_8;
var_12[1] = var_11;
// bld-rtl-dir/wile-rtl2-000044.scm:16
var_11 = wile_string_join_by(NULL, var_12, "bld-rtl-dir/wile-rtl2-000044.scm:16");
}
var_6 = var_11;
}
return var_6;
}
// end of function wile_string_pad_left
