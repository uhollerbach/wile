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

// @@@ (type-of v) @@@ bld-rtl-dir/wile-rtl2-000066.scm:13 @@@ wile_typeof @@@
lval wile_typeof(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_6;
var_6 = LVI_BOOL(var_2[0].vt == LV_NIL);
if (!LV_IS_FALSE(var_6)) {
lval var_7;
var_7 = LVI_SYMBOL("nil");
var_4 = var_7;
goto lbl_5;
}
lval var_8;
var_8 = LVI_BOOL(var_2[0].vt == LV_SYMBOL);
if (!LV_IS_FALSE(var_8)) {
lval var_9;
var_9 = LVI_SYMBOL("symbol");
var_4 = var_9;
goto lbl_5;
}
lval var_10;
var_10 = LVI_BOOL(var_2[0].vt == LV_BOOL);
if (!LV_IS_FALSE(var_10)) {
lval var_11;
var_11 = LVI_SYMBOL("boolean");
var_4 = var_11;
goto lbl_5;
}
lval var_12;
var_12 = LVI_BOOL(var_2[0].vt == LV_CHAR);
if (!LV_IS_FALSE(var_12)) {
lval var_13;
var_13 = LVI_SYMBOL("char");
var_4 = var_13;
goto lbl_5;
}
lval var_14;
var_14 = LVI_BOOL(var_2[0].vt == LV_STRING);
if (!LV_IS_FALSE(var_14)) {
lval var_15;
var_15 = LVI_SYMBOL("string");
var_4 = var_15;
goto lbl_5;
}
lval var_16;
var_16 = LVI_BOOL(var_2[0].vt == LV_INT);
if (!LV_IS_FALSE(var_16)) {
lval var_17;
var_17 = LVI_SYMBOL("integer");
var_4 = var_17;
goto lbl_5;
}
lval var_18;
var_18 = LVI_BOOL(var_2[0].vt == LV_RAT || var_2[0].vt == LV_INT);
if (!LV_IS_FALSE(var_18)) {
lval var_19;
var_19 = LVI_SYMBOL("rational");
var_4 = var_19;
goto lbl_5;
}
lval var_20;
var_20 = LVI_BOOL(var_2[0].vt == LV_REAL || var_2[0].vt == LV_RAT || var_2[0].vt == LV_INT);
if (!LV_IS_FALSE(var_20)) {
lval var_21;
var_21 = LVI_SYMBOL("real");
var_4 = var_21;
goto lbl_5;
}
lval var_22;
var_22 = LVI_BOOL(var_2[0].vt == LV_CMPLX || var_2[0].vt == LV_REAL || var_2[0].vt == LV_RAT || var_2[0].vt == LV_INT);
if (!LV_IS_FALSE(var_22)) {
lval var_23;
var_23 = LVI_SYMBOL("complex");
var_4 = var_23;
goto lbl_5;
}
lval var_24;
var_24 = LVI_BOOL(var_2[0].vt == LV_PAIR);
if (!LV_IS_FALSE(var_24)) {
lval var_25;
var_25 = LVI_SYMBOL("pair");
var_4 = var_25;
goto lbl_5;
}
lval var_26;
var_26 = LVI_BOOL(var_2[0].vt == LV_FILE_PORT);
if (!LV_IS_FALSE(var_26)) {
lval var_27;
var_27 = LVI_SYMBOL("file-port");
var_4 = var_27;
goto lbl_5;
}
lval var_28;
var_28 = LVI_BOOL(var_2[0].vt == LV_PIPE_PORT);
if (!LV_IS_FALSE(var_28)) {
lval var_29;
var_29 = LVI_SYMBOL("pipe-port");
var_4 = var_29;
goto lbl_5;
}
lval var_30;
var_30 = LVI_BOOL(var_2[0].vt == LV_SOCK_PORT);
if (!LV_IS_FALSE(var_30)) {
lval var_31;
var_31 = LVI_SYMBOL("socket-port");
var_4 = var_31;
goto lbl_5;
}
lval var_32;
var_32 = LVI_BOOL(var_2[0].vt == LV_STR_PORT);
if (!LV_IS_FALSE(var_32)) {
lval var_33;
var_33 = LVI_SYMBOL("string-port");
var_4 = var_33;
goto lbl_5;
}
lval var_34;
var_34 = LVI_BOOL(var_2[0].vt == LV_SQLITE_PORT);
if (!LV_IS_FALSE(var_34)) {
lval var_35;
var_35 = LVI_SYMBOL("sqlite-port");
var_4 = var_35;
goto lbl_5;
}
lval var_36;
var_36 = LVI_BOOL(var_2[0].vt == LV_SQLITE_STMT);
if (!LV_IS_FALSE(var_36)) {
lval var_37;
var_37 = LVI_SYMBOL("sqlite-statement");
var_4 = var_37;
goto lbl_5;
}
lval var_38;
var_38 = LVI_BOOL(var_2[0].vt == LV_VECTOR);
if (!LV_IS_FALSE(var_38)) {
lval var_39;
var_39 = LVI_SYMBOL("vector");
var_4 = var_39;
goto lbl_5;
}
lval var_40;
var_40 = LVI_BOOL(var_2[0].vt == LV_BVECTOR);
if (!LV_IS_FALSE(var_40)) {
lval var_41;
var_41 = LVI_SYMBOL("bytevector");
var_4 = var_41;
goto lbl_5;
}
lval var_42;
var_42 = LVI_BOOL(var_2[0].vt == LV_PROMISE);
if (!LV_IS_FALSE(var_42)) {
lval var_43;
var_43 = LVI_SYMBOL("promise");
var_4 = var_43;
goto lbl_5;
}
lval var_44;
var_44 = LVI_BOOL(var_2[0].vt == LV_CLAMBDA);
if (!LV_IS_FALSE(var_44)) {
lval var_45;
var_45 = LVI_SYMBOL("compiled-procedure");
var_4 = var_45;
goto lbl_5;
}
lval var_46;
var_46 = LVI_BOOL(var_2[0].vt == LV_ILAMBDA);
if (!LV_IS_FALSE(var_46)) {
lval var_47;
var_47 = LVI_SYMBOL("interpreted-procedure");
var_4 = var_47;
goto lbl_5;
}
lval var_48;
var_48 = LVI_BOOL(var_2[0].vt == LV_CONT);
if (!LV_IS_FALSE(var_48)) {
lval var_49;
var_49 = LVI_SYMBOL("continuation");
var_4 = var_49;
goto lbl_5;
}
lval var_50;
var_50 = LVI_SYMBOL("unknown!");
var_4 = var_50;
lbl_5:;
return var_4;
}
// end of function wile_typeof
