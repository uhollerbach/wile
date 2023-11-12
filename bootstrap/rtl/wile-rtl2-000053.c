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

// @@@ (write-string . strs) @@@ bld-rtl-dir/wile-rtl2-000053.scm:15 @@@ wile_write_string @@@
lval wile_write_string(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_BOOL(false);
var_4 = var_5;
lval var_6;
var_6 = var_2[0];
lval var_8;
var_8 = LVI_BOOL(true);
do {
lval var_9;
var_9 = LVI_BOOL(var_6.vt == LV_NIL);
lval var_10;
var_10 = LVI_BOOL(LV_IS_FALSE(var_9));
var_8 = var_10;
if (LV_IS_FALSE(var_8)) { break; }
lval var_11;
if (var_6.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_11 = (var_6.v.pair.car ? *(var_6.v.pair.car) : LVI_NIL());
lval var_12;
var_12 = LVI_BOOL(var_11.vt == LV_FILE_PORT || var_11.vt == LV_PIPE_PORT || var_11.vt == LV_SOCK_PORT);
var_8 = var_12;
if (LV_IS_FALSE(var_8)) { break; }
} while (0);
if (LV_IS_FALSE(var_8)) {
(void)
 LVI_BOOL(false);
} else {
lval var_14;
if (var_6.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_14 = (var_6.v.pair.car ? *(var_6.v.pair.car) : LVI_NIL());
var_4 = var_14;
lval var_15;
if (var_6.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_15 = (var_6.v.pair.cdr ? *(var_6.v.pair.cdr) : LVI_NIL());
var_6 = var_15;
}
lval var_16;
if (LV_IS_FALSE(var_4)) {
lval var_18;
lval var_19;
lval var_20;
var_20 = LVI_INT(0);
var_18 = var_20;
lval var_17;
lptr var_21 = new_lv(VT_UNINIT);
var_21->v.pair.car = &(var_18); //  symbol.3
do {
lval var_22;
var_22 = LVI_BOOL(var_6.vt == LV_NIL);
if (!LV_IS_FALSE(var_22)) {
var_17 = var_18;
break;
}
lval var_23;
if (var_6.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_23 = (var_6.v.pair.car ? *(var_6.v.pair.car) : LVI_NIL());
if (var_23.vt == LV_CHAR) {
fputc(var_23.v.chr, stdout);
} else if (var_23.vt == LV_STRING) {
fputs(var_23.v.str, stdout);
} else {
WILE_EX("write-string", "input is not a string or char!");
}
(void)
 LVI_BOOL(true);
lval var_25;
if (var_6.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_25 = (var_6.v.pair.cdr ? *(var_6.v.pair.cdr) : LVI_NIL());
var_6 = var_25;
lval var_26;
var_26 = LVI_INT(1);
lval var_27;
var_27 = LVI_INT(var_18.v.iv + var_26.v.iv);
var_19 = var_27;
var_18 = var_19;
} while (1);
*var_21 = var_18;
var_16 = var_17;
} else {
lval var_29;
lval var_30;
lval var_31;
var_31 = LVI_INT(0);
var_29 = var_31;
lval var_28;
lptr var_32 = new_lv(VT_UNINIT);
var_32->v.pair.car = &(var_29); //  symbol.4
do {
lval var_33;
var_33 = LVI_BOOL(var_6.vt == LV_NIL);
if (!LV_IS_FALSE(var_33)) {
var_28 = var_29;
break;
}
lval var_34;
if (var_6.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_34 = (var_6.v.pair.car ? *(var_6.v.pair.car) : LVI_NIL());
{
FILE* fp;
if (var_4.vt == LV_FILE_PORT || var_4.vt == LV_PIPE_PORT || var_4.vt == LV_SOCK_PORT) {
fp = var_4.v.fp;
} else {
WILE_EX("write-string", "first input is not a port!");
}
if (var_34.vt == LV_CHAR) {
fputc(var_34.v.chr, fp);
} else if (var_34.vt == LV_STRING) {
fputs(var_34.v.str, fp);
} else {
WILE_EX("write-string", "second input is not a string or char!");
}
(void)
 LVI_BOOL(true);
}
lval var_36;
if (var_6.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_36 = (var_6.v.pair.cdr ? *(var_6.v.pair.cdr) : LVI_NIL());
var_6 = var_36;
lval var_37;
var_37 = LVI_INT(1);
lval var_38;
var_38 = LVI_INT(var_29.v.iv + var_37.v.iv);
var_30 = var_38;
var_29 = var_30;
} while (1);
*var_32 = var_29;
var_16 = var_28;
}
return var_16;
}
// end of function wile_write_string
