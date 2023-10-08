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

// @@@ (type-of v) @@@ bld-rtl-dir/wile-rtl2-000061.scm:13 @@@ wile_typeof @@@
lval wile_typeof(lptr* var_1, lptr var_2)
{
lval var_4;
do {
lval var_5;
var_5 = LVI_BOOL(var_2[0].vt == LV_NIL);
if (!LV_IS_FALSE(var_5)) {
lval var_6;
var_6 = LVI_SYMBOL("nil");
var_4 = var_6;
break;
}
lval var_7;
var_7 = LVI_BOOL(var_2[0].vt == LV_SYMBOL);
if (!LV_IS_FALSE(var_7)) {
lval var_8;
var_8 = LVI_SYMBOL("symbol");
var_4 = var_8;
break;
}
lval var_9;
var_9 = LVI_BOOL(var_2[0].vt == LV_BOOL);
if (!LV_IS_FALSE(var_9)) {
lval var_10;
var_10 = LVI_SYMBOL("boolean");
var_4 = var_10;
break;
}
lval var_11;
var_11 = LVI_BOOL(var_2[0].vt == LV_CHAR);
if (!LV_IS_FALSE(var_11)) {
lval var_12;
var_12 = LVI_SYMBOL("char");
var_4 = var_12;
break;
}
lval var_13;
var_13 = LVI_BOOL(var_2[0].vt == LV_STRING);
if (!LV_IS_FALSE(var_13)) {
lval var_14;
var_14 = LVI_SYMBOL("string");
var_4 = var_14;
break;
}
lval var_15;
var_15 = LVI_BOOL(var_2[0].vt == LV_INT);
if (!LV_IS_FALSE(var_15)) {
lval var_16;
var_16 = LVI_SYMBOL("integer");
var_4 = var_16;
break;
}
lval var_17;
var_17 = LVI_BOOL(var_2[0].vt == LV_RAT || var_2[0].vt == LV_INT);
if (!LV_IS_FALSE(var_17)) {
lval var_18;
var_18 = LVI_SYMBOL("rational");
var_4 = var_18;
break;
}
lval var_19;
var_19 = LVI_BOOL(var_2[0].vt == LV_REAL || var_2[0].vt == LV_RAT || var_2[0].vt == LV_INT);
if (!LV_IS_FALSE(var_19)) {
lval var_20;
var_20 = LVI_SYMBOL("real");
var_4 = var_20;
break;
}
lval var_21;
var_21 = LVI_BOOL(var_2[0].vt == LV_CMPLX || var_2[0].vt == LV_REAL || var_2[0].vt == LV_RAT || var_2[0].vt == LV_INT);
if (!LV_IS_FALSE(var_21)) {
lval var_22;
var_22 = LVI_SYMBOL("complex");
var_4 = var_22;
break;
}
lval var_23;
var_23 = LVI_BOOL(var_2[0].vt == LV_PAIR);
if (!LV_IS_FALSE(var_23)) {
lval var_24;
var_24 = LVI_SYMBOL("pair");
var_4 = var_24;
break;
}
lval var_25;
var_25 = LVI_BOOL(var_2[0].vt == LV_FILE_PORT);
if (!LV_IS_FALSE(var_25)) {
lval var_26;
var_26 = LVI_SYMBOL("file-port");
var_4 = var_26;
break;
}
lval var_27;
var_27 = LVI_BOOL(var_2[0].vt == LV_PIPE_PORT);
if (!LV_IS_FALSE(var_27)) {
lval var_28;
var_28 = LVI_SYMBOL("pipe-port");
var_4 = var_28;
break;
}
lval var_29;
var_29 = LVI_BOOL(var_2[0].vt == LV_SOCK_PORT);
if (!LV_IS_FALSE(var_29)) {
lval var_30;
var_30 = LVI_SYMBOL("socket-port");
var_4 = var_30;
break;
}
lval var_31;
var_31 = LVI_BOOL(var_2[0].vt == LV_STR_PORT);
if (!LV_IS_FALSE(var_31)) {
lval var_32;
var_32 = LVI_SYMBOL("string-port");
var_4 = var_32;
break;
}
lval var_33;
var_33 = LVI_BOOL(var_2[0].vt == LV_SQLITE_PORT);
if (!LV_IS_FALSE(var_33)) {
lval var_34;
var_34 = LVI_SYMBOL("sqlite-port");
var_4 = var_34;
break;
}
lval var_35;
var_35 = LVI_BOOL(var_2[0].vt == LV_SQLITE_STMT);
if (!LV_IS_FALSE(var_35)) {
lval var_36;
var_36 = LVI_SYMBOL("sqlite-statement");
var_4 = var_36;
break;
}
lval var_37;
var_37 = LVI_BOOL(var_2[0].vt == LV_VECTOR);
if (!LV_IS_FALSE(var_37)) {
lval var_38;
var_38 = LVI_SYMBOL("vector");
var_4 = var_38;
break;
}
lval var_39;
var_39 = LVI_BOOL(var_2[0].vt == LV_BVECTOR);
if (!LV_IS_FALSE(var_39)) {
lval var_40;
var_40 = LVI_SYMBOL("bytevector");
var_4 = var_40;
break;
}
lval var_41;
var_41 = LVI_BOOL(var_2[0].vt == LV_PROMISE);
if (!LV_IS_FALSE(var_41)) {
lval var_42;
var_42 = LVI_SYMBOL("promise");
var_4 = var_42;
break;
}
lval var_43;
var_43 = LVI_BOOL(var_2[0].vt == LV_LAMBDA);
if (!LV_IS_FALSE(var_43)) {
lval var_44;
var_44 = LVI_SYMBOL("procedure");
var_4 = var_44;
break;
}
lval var_45;
var_45 = LVI_BOOL(var_2[0].vt == LV_CONT);
if (!LV_IS_FALSE(var_45)) {
lval var_46;
var_46 = LVI_SYMBOL("continuation");
var_4 = var_46;
break;
}
lval var_47;
var_47 = LVI_SYMBOL("unknown!");
var_4 = var_47;
} while (0);
return var_4;
}
// end of function wile_typeof
