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
static lval fn_12(lptr*, lptr);
static lval fn_16(lptr*, lptr);
static lval fn_51(lptr*, lptr);
static lval fn_96(lptr*, lptr);

// definitions

// @@@ lambda (x) @@@ bld-rtl-dir/wile-rtl2-000076.scm:20 @@@ fn_12 @@@
static lval fn_12(lptr* var_13, lptr var_14)
{
return var_14[0];
}
// end of lambda fn_12

// @@@ lambda (l) @@@ bld-rtl-dir/wile-rtl2-000076.scm:21 @@@ fn_16 @@@
static lval fn_16(lptr* var_17, lptr var_18)
{
lval var_20;
lval var_21;
var_21 = LVI_STRING("\\(\\+0x[0-9a-fA-F]+\\)");
lval var_22;
{
lval vs[8];
vs[0] = var_21;
vs[1] = var_18[0];
var_22 = wile_regex_match(NULL, vs);
}
var_20 = var_22;
if (LV_IS_FALSE(var_20)) {
(void)
 LVI_BOOL(false);
} else {
if (LV_IS_FALSE(V_CLOS(var_17,0))) {
lval var_26;
if (var_20.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_26 = (var_20.v.pair.car ? *(var_20.v.pair.car) : LVI_NIL());
V_CLOS(var_17,0) = var_26;
(void)
 V_CLOS(var_17,0);
} else {
(void)
 LVI_BOOL(false);
}
lval var_28;
var_28 = LVI_STRING("cadr");
lval var_29;
{
char* cp = strchr(var_28.v.str, 'r');
var_29 = var_20;
while (*(--cp) != 'c') {
if (var_29.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_29 = (var_29.v.pair.car ? *(var_29.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_29 = (var_29.v.pair.cdr ? *(var_29.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_28.v.str);
}
}
}
lval var_30;
var_30 = LVI_INT(2);
lval var_31;
if (var_29.vt != LV_STRING) {
WILE_EX("string-copy", "expects a string input");
}
{
size_t len = strlen(var_29.v.str);
if (var_30.v.iv < 0 || (size_t) var_30.v.iv >= len) {
WILE_EX("string-copy", "start index is out of range");
}
var_31 = LVI_STRING(var_29.v.str + var_30.v.iv);
}
var_20 = var_31;
lval var_32;
var_32 = LVI_INT(0);
lval var_33;
var_33 = LVI_INT(strlen(var_20.v.str));
lval var_34;
var_34 = LVI_INT(1);
lval var_35;
{
lval vs[2];
vs[0] = var_33;
vs[1] = var_34;
var_35 = wile_gen_list(2, vs, NULL);
}
{
lval vs[8];
vs[0] = var_35;
var_35 = wile_subtract(NULL, vs);
}
lval var_36;
if (var_20.vt != LV_STRING) {
WILE_EX("string-copy", "expects a string input");
}
{
size_t len = strlen(var_20.v.str);
if (var_32.v.iv < 0 || (size_t) var_32.v.iv >= len) {
WILE_EX("string-copy", "start index is out of range");
}
if (var_35.v.iv < var_32.v.iv || (size_t) var_35.v.iv >= len) {
WILE_EX("string-copy", "end index is out of range");
}
var_36.vt = LV_STRING;
var_36.v.str = LISP_ALLOC(char, 1 + var_35.v.iv - var_32.v.iv);
LISP_ASSERT(var_36.v.str != NULL);
memcpy(var_36.v.str, var_20.v.str + var_32.v.iv, var_35.v.iv - var_32.v.iv);
var_36.v.str[var_35.v.iv - var_32.v.iv] = '\0';
}
var_20 = var_36;
}
return var_20;
}
// end of lambda fn_16

// @@@ string-join-by @@@ bld-rtl-dir/wile-rtl2-000076.scm:31 @@@ fn_39 @@@
static lval fn_39(lptr* var_40, lptr var_41)
{
lval var_43;
var_43 = var_41[1];
{
lval vs[8];
vs[0] = var_41[0];
vs[1] = var_43;
var_43 = wile_string_join_by(NULL, vs);
}
return var_43;
}
// end of prim fn_39

// @@@ lambda () @@@ bld-rtl-dir/wile-rtl2-000076.scm:36 @@@ fn_51 @@@
static lval fn_51(lptr* var_52, lptr var_53)
{
lbl_54:;
lval var_55;
lval var_56;
{
lval vs[8];
vs[0] = V_CLOS(var_52,0);
var_56 = wile_read_line(NULL, vs);
}
var_55 = var_56;
lval var_57;
if (LV_IS_FALSE(var_55)) {
lval var_58;
{
lval vs[8];
vs[0] = V_CLOS(var_52,0);
var_58 = wile_closeport(NULL, vs);
}
var_57 = var_58;
} else {
lval var_59;
var_59 = LVI_CHAR(10);
lval var_60;
{
lval vs[3];
vs[0] = V_CLOS(var_52,1);
vs[1] = var_55;
vs[2] = var_59;
var_60 = wile_gen_list(3, vs, NULL);
}
{
lval vs[8];
vs[0] = var_60;
var_60 = wile_write_string(NULL, vs);
}
goto lbl_54;	// selfie
}
return var_57;
}
// end of lambda fn_51

// @@@ (display-stack-trace trace-data port) @@@ bld-rtl-dir/wile-rtl2-000076.scm:13 @@@ wile_display_stack_trace @@@
lval wile_display_stack_trace(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_BOOL(false);
var_4 = var_5;
lval var_6;
lval var_7;
var_7 = LVI_BOOL(false);
var_6 = var_7;
lval var_8;
lval var_9;
var_9 = LVI_BOOL(false);
var_8 = var_9;
lval var_10;
lval var_11;
var_11 = LVI_BOOL(false);
var_10 = var_11;
MK_CLOS(var_13,0);
MK_CLOS(var_17,1);
P_CLOS(var_17,0) = &(var_4);
lval var_37;
var_37 = LVI_NIL();
{
lval vs[8];
vs[0] = LVI_PROC(fn_16,var_17,1);
vs[1] = var_2[0];
vs[2] = var_37;
var_37 = wile_map(NULL, vs);
}
lval var_38;
{
lval vs[8];
vs[0] = LVI_PROC(fn_12,var_13,1);
vs[1] = var_37;
var_38 = wile_list_filter(NULL, vs);
}
var_6 = var_38;
lval var_44;
var_44 = LVI_STRING(" ");
lval var_45;
var_45 = LVI_STRING("addr2line -f -p -e");
lval var_46;
var_46 = LVI_STRING("-a");
lval var_47;
{
lval vs[6];
vs[0] = LVI_PROC(fn_39,NULL,-2);
vs[1] = var_44;
vs[2] = var_45;
vs[3] = var_4;
vs[4] = var_46;
vs[5] = var_6;
var_47 = wile_gen_list(6, vs, NULL);
}
var_47 = wile_apply_function(&(var_47), __FILE__, __LINE__);
var_8 = var_47;
lval var_48;
var_48 = wile_run_pipe_command(var_8, "r", __FILE__, __LINE__);
var_10 = var_48;
lval var_49;
var_49 = LVI_STRING("wile stack trace begin\n");
lval var_50;
{
lval vs[2];
vs[0] = var_2[1];
vs[1] = var_49;
var_50 = wile_gen_list(2, vs, NULL);
}
{
lval vs[8];
vs[0] = var_50;
var_50 = wile_write_string(NULL, vs);
}
MK_CLOS(var_52,2);
P_CLOS(var_52,1) = &(var_2[1]);
P_CLOS(var_52,0) = &(var_10);
lval var_65[8];
(void)
 fn_51(var_52, var_65);
lval var_67;
var_67 = LVI_STRING("wile stack trace end\n");
lval var_68;
{
lval vs[2];
vs[0] = var_2[1];
vs[1] = var_67;
var_68 = wile_gen_list(2, vs, NULL);
}
{
lval vs[8];
vs[0] = var_68;
var_68 = wile_write_string(NULL, vs);
}
return var_68;
}
// end of function wile_display_stack_trace

// @@@ lambda (acc) @@@ bld-rtl-dir/wile-rtl2-000076.scm:58 @@@ fn_96 @@@
static lval fn_96(lptr* var_97, lptr var_98)
{
lbl_99:;
lval var_100;
lval var_101;
{
lval vs[8];
vs[0] = V_CLOS(var_97,0);
var_101 = wile_read_line(NULL, vs);
}
var_100 = var_101;
lval var_102;
if (LV_IS_FALSE(var_100)) {
lval var_103;
{
lval vs[8];
vs[0] = var_98[0];
var_103 = wile_list_reverse(NULL, vs);
}
var_102 = var_103;
} else {
lval var_104;
{
lptr p1 = NULL, p2 = NULL;
if (var_100.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_100;
}
if (var_98[0].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_98[0];
}
var_104 = LVI_PAIR(p1, p2);
}
lval var_107[8];
var_107[0] = var_104;
var_98[0] = var_107[0];
goto lbl_99;	// selfie
}
return var_102;
}
// end of lambda fn_96

// @@@ (stack-trace port) @@@ bld-rtl-dir/wile-rtl2-000076.scm:45 @@@ wile_stack_trace @@@
lval wile_stack_trace(lptr* var_69, lptr var_70)
{
lval var_72;
lval var_73;
var_73 = LVI_STRING("wile-scratch.XXXXXX");
lval var_74;
{
lval vs[8];
vs[0] = var_73;
var_74 = wile_temp_file(NULL, vs);
}
var_72 = var_74;
lval var_75;
lval var_76;
if (var_72.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_76 = (var_72.v.pair.car ? *(var_72.v.pair.car) : LVI_NIL());
var_75 = var_76;
lval var_77;
lval var_78;
var_78 = LVI_STRING("cadr");
lval var_79;
{
char* cp = strchr(var_78.v.str, 'r');
var_79 = var_72;
while (*(--cp) != 'c') {
if (var_79.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_79 = (var_79.v.pair.car ? *(var_79.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_79 = (var_79.v.pair.cdr ? *(var_79.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_78.v.str);
}
}
}
var_77 = var_79;
lval var_80;
lval var_81;
var_81 = LVI_BOOL(false);
var_80 = var_81;
(void)
 LVI_BOOL(false);
(void)
 LVI_BOOL(false);
(void)
 LVI_BOOL(false);
(void)
 LVI_BOOL(false);
(void)
 LVI_BOOL(unlink(var_77.v.str) == 0);
wile_stack_trace_minimal(fileno((var_75.vt == LV_FILE_PORT) ? var_75.v.fp : stderr));
(void)
 LVI_NIL();
{
lval vs[8];
vs[0] = var_75;
(void)
 wile_flushport(NULL, vs);
}
lval var_93;
var_93 = LVI_INT(0);
lval var_94;
var_94 = LVI_SYMBOL("start");
{
lval vs[8];
vs[0] = var_75;
vs[1] = var_93;
vs[2] = var_94;
(void)
 wile_setfilepos3(NULL, vs);
}
MK_CLOS(var_97,1);
P_CLOS(var_97,0) = &(var_75);
lval var_108;
var_108 = LVI_NIL();
lval var_109;
lval var_110[8];
var_110[0] = var_108;
var_109 = fn_96(var_97, var_110);
var_80 = var_109;
{
lval vs[8];
vs[0] = var_75;
(void)
 wile_closeport(NULL, vs);
}
lval var_115[8];
var_115[0] = var_80;
var_115[1] = var_70[0];
var_70[0] = var_115[0];
var_70[1] = var_115[1];
TAIL_CALL wile_display_stack_trace(NULL, var_70);
}
// end of function wile_stack_trace
