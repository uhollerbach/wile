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
static lval fn_12(lptr*, lptr, const char*);
static lval fn_16(lptr*, lptr, const char*);
static lval fn_61(lptr*, lptr, const char*);
static lval fn_117(lptr*, lptr, const char*);

// definitions

// @@@ lambda (x) @@@ bld-rtl-dir/wile-rtl2-000077.scm:20 @@@ fn_12 @@@
static lval fn_12(lptr* var_13, lptr var_14, const char* cloc)
{
return var_14[0];
}
// end of lambda fn_12

// @@@ lambda (l) @@@ bld-rtl-dir/wile-rtl2-000077.scm:21 @@@ fn_16 @@@
static lval fn_16(lptr* var_17, lptr var_18, const char* cloc)
{
lval var_20;
lval var_21;
var_21 = LVI_STRING("\\(\\+0x[0-9a-fA-F]+\\)");
lval var_22;
{
lval var_23[8];
var_23[0] = var_21;
var_23[1] = var_18[0];
// bld-rtl-dir/wile-rtl2-000077.scm:22
var_22 = wile_regex_match(NULL, var_23, "bld-rtl-dir/wile-rtl2-000077.scm:22");
}
var_20 = var_22;
if (LV_IS_FALSE(var_20)) {
} else {
if (LV_IS_FALSE(V_CLOS(var_17,0))) {
lval var_27;
if (var_20.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000077.scm:25", "input is not a pair!");
}
var_27 = (var_20.v.pair.car ? *(var_20.v.pair.car) : LVI_NIL());
V_CLOS(var_17,0) = var_27;
(void)
 V_CLOS(var_17,0);
} else {
}
lval var_29;
var_29 = LVI_STRING("cadr");
lval var_30;
{
char* cp = strchr(var_29.v.str, 'r');
var_30 = var_20;
while (*(--cp) != 'c') {
if (var_30.vt != LV_PAIR) {
wile_exception("cxr", "bld-rtl-dir/wile-rtl2-000077.scm:26", "input does not have the right structure!");
}
if (*cp == 'a') {
var_30 = (var_30.v.pair.car ? *(var_30.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_30 = (var_30.v.pair.cdr ? *(var_30.v.pair.cdr) : LVI_NIL());
} else {
wile_exception("cxr", "bld-rtl-dir/wile-rtl2-000077.scm:26", "got malformed control string '%s'", var_29.v.str);
}
}
}
lval var_31;
var_31 = LVI_INT(2);
lval var_32;
if (var_30.vt != LV_STRING || var_31.vt != LV_INT) {
wile_exception("string-copy", "bld-rtl-dir/wile-rtl2-000077.scm:26", "expects a string and an integer input");
}
{
size_t len = strlen(var_30.v.str);
if (var_31.v.iv < 0 || (size_t) var_31.v.iv >= len) {
wile_exception("string-copy", "bld-rtl-dir/wile-rtl2-000077.scm:26", "start index is out of range");
}
var_32 = LVI_STRING(var_30.v.str + var_31.v.iv);
}
var_20 = var_32;
lval var_33;
var_33 = LVI_INT(0);
lval var_34;
var_34 = LVI_INT(strlen(var_20.v.str));
lval var_35;
var_35 = LVI_INT(1);
lval var_36;
{
lval var_38[2];
var_38[0] = var_34;
var_38[1] = var_35;
var_36 = wile_gen_list(2, var_38, NULL);
}
{
lval var_37[8];
var_37[0] = var_36;
// bld-rtl-dir/wile-rtl2-000077.scm:27
var_36 = wile_subtract(NULL, var_37, "bld-rtl-dir/wile-rtl2-000077.scm:27");
}
lval var_39;
if (var_20.vt != LV_STRING || var_33.vt != LV_INT || var_36.vt != LV_INT) {
wile_exception("string-copy", "bld-rtl-dir/wile-rtl2-000077.scm:27", "expects a string and two integer inputs");
}
{
size_t len = strlen(var_20.v.str);
if (var_33.v.iv < 0 || (size_t) var_33.v.iv >= len) {
wile_exception("string-copy", "bld-rtl-dir/wile-rtl2-000077.scm:27", "start index is out of range");
}
if (var_36.v.iv < var_33.v.iv || (size_t) var_36.v.iv >= len) {
wile_exception("string-copy", "bld-rtl-dir/wile-rtl2-000077.scm:27", "end index is out of range");
}
var_39.vt = LV_STRING;
var_39.origin = var_20.origin;
var_39.v.str = LISP_ALLOC(char, 1 + var_36.v.iv - var_33.v.iv);
memcpy(var_39.v.str, var_20.v.str + var_33.v.iv, var_36.v.iv - var_33.v.iv);
var_39.v.str[var_36.v.iv - var_33.v.iv] = '\0';
}
var_20 = var_39;
}
return var_20;
}
// end of lambda fn_16

// @@@ string-join-by @@@ bld-rtl-dir/wile-rtl2-000077.scm:31 @@@ fn_45 @@@
static lval fn_45(lptr* var_46, lptr var_47, const char* cloc)
{
lval var_49;
var_49 = var_47[1];
{
lval var_50[8];
var_50[0] = var_47[0];
var_50[1] = var_49;
// bld-rtl-dir/wile-rtl2-000077.scm:31
var_49 = wile_string_join_by(NULL, var_50, "bld-rtl-dir/wile-rtl2-000077.scm:31");
}
return var_49;
}
// end of prim fn_45

// @@@ lambda () @@@ bld-rtl-dir/wile-rtl2-000077.scm:36 @@@ fn_61 @@@
static lval fn_61(lptr* var_62, lptr var_63, const char* cloc)
{
lbl_64:;
lval var_65;
lval var_66;
{
lval var_67[8];
var_67[0] = V_CLOS(var_62,0);
// bld-rtl-dir/wile-rtl2-000077.scm:36
var_66 = wile_read_line(NULL, var_67, "bld-rtl-dir/wile-rtl2-000077.scm:36");
}
var_65 = var_66;
lval var_68;
if (LV_IS_FALSE(var_65)) {
lval var_69;
{
lval var_70[8];
var_70[0] = V_CLOS(var_62,0);
// bld-rtl-dir/wile-rtl2-000077.scm:40
var_69 = wile_closeport(NULL, var_70, "bld-rtl-dir/wile-rtl2-000077.scm:40");
}
var_68 = var_69;
} else {
lval var_71;
var_71 = LVI_CHAR(10);
lval var_72;
{
lval var_74[3];
var_74[0] = V_CLOS(var_62,1);
var_74[1] = var_65;
var_74[2] = var_71;
var_72 = wile_gen_list(3, var_74, NULL);
}
{
lval var_73[8];
var_73[0] = var_72;
// bld-rtl-dir/wile-rtl2-000077.scm:38
var_72 = wile_write_string(NULL, var_73, "bld-rtl-dir/wile-rtl2-000077.scm:38");
}
// bld-rtl-dir/wile-rtl2-000077.scm:39
goto lbl_64;	// selfie
}
return var_68;
}
// end of lambda fn_61

// @@@ (display-stack-trace trace-data port) @@@ bld-rtl-dir/wile-rtl2-000077.scm:13 @@@ wile_display_stack_trace @@@
lval wile_display_stack_trace(lptr* var_1, lptr var_2, const char* cloc)
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
lptr var_40 = new_lv(VT_UNINIT);
var_40->v.pair.car = &(var_4);
P_CLOS(var_17,0) = var_40;
lval var_41;
var_41 = LVI_NIL();
{
lval var_42[8];
var_42[0] = LVI_PROC(fn_16,var_17,1);
var_42[1] = var_2[0];
var_42[2] = var_41;
// bld-rtl-dir/wile-rtl2-000077.scm:21
var_41 = wile_map(NULL, var_42, "bld-rtl-dir/wile-rtl2-000077.scm:21");
}
lval var_43;
{
lval var_44[8];
var_44[0] = LVI_PROC(fn_12,var_13,1);
var_44[1] = var_41;
// bld-rtl-dir/wile-rtl2-000077.scm:19
var_43 = wile_list_filter(NULL, var_44, "bld-rtl-dir/wile-rtl2-000077.scm:19");
}
var_6 = var_43;
lval var_51;
var_51 = LVI_STRING(" ");
lval var_52;
var_52 = LVI_STRING("addr2line -f -p -e");
lval var_53;
var_53 = LVI_STRING("-a");
lval var_54;
{
lval var_55[6];
var_55[0] = LVI_PROC(fn_45,NULL,-2);
var_55[1] = var_51;
var_55[2] = var_52;
var_55[3] = var_4;
var_55[4] = var_53;
var_55[5] = var_6;
var_54 = wile_gen_list(6, var_55, NULL);
}
var_54 = wile_apply_function(&(var_54), "bld-rtl-dir/wile-rtl2-000077.scm:31");
var_8 = var_54;
lval var_56;
var_56 = wile_run_pipe_command(var_8, "r", "bld-rtl-dir/wile-rtl2-000077.scm:33");
var_10 = var_56;
lval var_57;
var_57 = LVI_STRING("wile stack trace begin\n");
lval var_58;
{
lval var_60[2];
var_60[0] = var_2[1];
var_60[1] = var_57;
var_58 = wile_gen_list(2, var_60, NULL);
}
{
lval var_59[8];
var_59[0] = var_58;
// bld-rtl-dir/wile-rtl2-000077.scm:34
var_58 = wile_write_string(NULL, var_59, "bld-rtl-dir/wile-rtl2-000077.scm:34");
}
MK_CLOS(var_62,2);
lptr var_78 = new_lv(VT_UNINIT);
var_78->v.pair.car = &(var_2[1]);
P_CLOS(var_62,1) = var_78;
lptr var_79 = new_lv(VT_UNINIT);
var_79->v.pair.car = &(var_10);
P_CLOS(var_62,0) = var_79;
lval var_81[8];
// bld-rtl-dir/wile-rtl2-000077.scm:35
(void)
 fn_61(var_62, var_81, "bld-rtl-dir/wile-rtl2-000077.scm:35");
*var_79 = var_10;
*var_78 = var_2[1];
lval var_83;
var_83 = LVI_STRING("wile stack trace end\n");
lval var_84;
{
lval var_86[2];
var_86[0] = var_2[1];
var_86[1] = var_83;
var_84 = wile_gen_list(2, var_86, NULL);
}
{
lval var_85[8];
var_85[0] = var_84;
// bld-rtl-dir/wile-rtl2-000077.scm:41
var_84 = wile_write_string(NULL, var_85, "bld-rtl-dir/wile-rtl2-000077.scm:41");
}
*var_40 = var_4;
return var_84;
}
// end of function wile_display_stack_trace

// @@@ lambda (acc) @@@ bld-rtl-dir/wile-rtl2-000077.scm:58 @@@ fn_117 @@@
static lval fn_117(lptr* var_118, lptr var_119, const char* cloc)
{
lbl_120:;
lval var_121;
lval var_122;
{
lval var_123[8];
var_123[0] = V_CLOS(var_118,0);
// bld-rtl-dir/wile-rtl2-000077.scm:59
var_122 = wile_read_line(NULL, var_123, "bld-rtl-dir/wile-rtl2-000077.scm:59");
}
var_121 = var_122;
lval var_124;
if (LV_IS_FALSE(var_121)) {
lval var_125;
{
lval var_126[8];
var_126[0] = var_119[0];
// bld-rtl-dir/wile-rtl2-000077.scm:62
var_125 = wile_list_reverse(NULL, var_126, "bld-rtl-dir/wile-rtl2-000077.scm:62");
}
var_124 = var_125;
} else {
lval var_127;
{
lptr p1 = NULL, p2 = NULL;
if (var_121.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_121;
}
if (var_119[0].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_119[0];
}
var_127 = LVI_PAIR(p1, p2);
}
lval var_130[8];
var_130[0] = var_127;
var_119[0] = var_130[0];
// bld-rtl-dir/wile-rtl2-000077.scm:61
goto lbl_120;	// selfie
}
return var_124;
}
// end of lambda fn_117

// @@@ (stack-trace port) @@@ bld-rtl-dir/wile-rtl2-000077.scm:45 @@@ wile_stack_trace @@@
lval wile_stack_trace(lptr* var_87, lptr var_88, const char* cloc)
{
lval var_90;
lval var_91;
var_91 = LVI_STRING("wile-scratch.XXXXXX");
lval var_92;
{
lval var_93[8];
var_93[0] = var_91;
// bld-rtl-dir/wile-rtl2-000077.scm:46
var_92 = wile_temp_file(NULL, var_93, "bld-rtl-dir/wile-rtl2-000077.scm:46");
}
var_90 = var_92;
lval var_94;
lval var_95;
if (var_90.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000077.scm:47", "input is not a pair!");
}
var_95 = (var_90.v.pair.car ? *(var_90.v.pair.car) : LVI_NIL());
var_94 = var_95;
lval var_96;
lval var_97;
var_97 = LVI_STRING("cadr");
lval var_98;
{
char* cp = strchr(var_97.v.str, 'r');
var_98 = var_90;
while (*(--cp) != 'c') {
if (var_98.vt != LV_PAIR) {
wile_exception("cxr", "bld-rtl-dir/wile-rtl2-000077.scm:48", "input does not have the right structure!");
}
if (*cp == 'a') {
var_98 = (var_98.v.pair.car ? *(var_98.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_98 = (var_98.v.pair.cdr ? *(var_98.v.pair.cdr) : LVI_NIL());
} else {
wile_exception("cxr", "bld-rtl-dir/wile-rtl2-000077.scm:48", "got malformed control string '%s'", var_97.v.str);
}
}
}
var_96 = var_98;
lval var_99;
lval var_100;
var_100 = LVI_BOOL(false);
var_99 = var_100;
(void)
 LVI_BOOL(unlink(var_96.v.str) == 0);
wile_stack_trace_minimal(fileno((var_94.vt == LV_FILE_PORT) ? var_94.v.fp : stderr));
(void)
 LVI_NIL();
{
lval var_112[8];
var_112[0] = var_94;
// bld-rtl-dir/wile-rtl2-000077.scm:56
(void)
 wile_flushport(NULL, var_112, "bld-rtl-dir/wile-rtl2-000077.scm:56");
}
lval var_113;
var_113 = LVI_INT(0);
lval var_114;
var_114 = LVI_SYMBOL("start");
{
lval var_116[8];
var_116[0] = var_94;
var_116[1] = var_113;
var_116[2] = var_114;
// bld-rtl-dir/wile-rtl2-000077.scm:57
(void)
 wile_setfilepos3(NULL, var_116, "bld-rtl-dir/wile-rtl2-000077.scm:57");
}
MK_CLOS(var_118,1);
lptr var_131 = new_lv(VT_UNINIT);
var_131->v.pair.car = &(var_94);
P_CLOS(var_118,0) = var_131;
lval var_132;
var_132 = LVI_NIL();
lval var_133;
lval var_134[8];
var_134[0] = var_132;
// bld-rtl-dir/wile-rtl2-000077.scm:58
var_133 = fn_117(var_118, var_134, "bld-rtl-dir/wile-rtl2-000077.scm:58");
*var_131 = var_94;
var_99 = var_133;
{
lval var_137[8];
var_137[0] = var_94;
// bld-rtl-dir/wile-rtl2-000077.scm:63
(void)
 wile_closeport(NULL, var_137, "bld-rtl-dir/wile-rtl2-000077.scm:63");
}
lval var_140[8];
var_140[0] = var_99;
var_140[1] = var_88[0];
var_88[0] = var_140[0];
var_88[1] = var_140[1];
// bld-rtl-dir/wile-rtl2-000077.scm:64
TAIL_CALL wile_display_stack_trace(NULL, var_88, "bld-rtl-dir/wile-rtl2-000077.scm:64");
}
// end of function wile_stack_trace
