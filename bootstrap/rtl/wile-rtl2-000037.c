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
static lval fn_4(lptr*, lptr, const char*);
static lval fn_23(lptr*, lptr, const char*);
static lval fn_58(lptr*, lptr, const char*);

// definitions

// @@@ (merge1 lst acc) @@@ bld-rtl-dir/wile-rtl2-000037.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_5, lptr var_6, const char* cloc)
{
lval var_8;
var_8 = var_6[0];
lval var_9;
var_9 = var_6[1];
lval var_13;
lval var_14;
lval var_15;
var_15 = LVI_INT(0);
var_13 = var_15;
lptr var_16 = new_lv(VT_UNINIT);
var_16->v.pair.car = &(var_13);
lbl_11:
lval var_17;
var_17 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_17)) {
goto lbl_12;
}
lval var_18;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000037.scm:18", "input is not a pair!");
}
var_18 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_19;
{
lptr p1 = NULL, p2 = NULL;
if (var_18.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_18;
}
if (var_9.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_9;
}
var_19 = LVI_PAIR(p1, p2);
}
var_9 = var_19;
lval var_20;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000037.scm:19", "input is not a pair!");
}
var_20 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_20;
lval var_21;
var_21 = LVI_INT(1);
lval var_22;
var_22 = LVI_INT(var_13.v.iv + var_21.v.iv);
var_14 = var_22;
var_13 = var_14;
goto lbl_11;
lbl_12:;
*var_16 = var_13;
return var_9;
}
// end of function fn_4

// @@@ (merge2 is-lt? lst1 lst2 acc) @@@ bld-rtl-dir/wile-rtl2-000037.scm:21 @@@ fn_23 @@@
static lval fn_23(lptr* var_24, lptr var_25, const char* cloc)
{
lbl_26:;
lval var_27;
var_27 = var_25[1];
lval var_28;
var_28 = var_25[2];
lval var_29;
var_29 = var_25[3];
lval var_30;
lval var_32;
var_32 = LVI_BOOL(var_27.vt == LV_NIL);
if (!LV_IS_FALSE(var_32)) {
lval var_33;
lval var_35[8];
var_35[0] = var_28;
var_35[1] = var_29;
var_25[0] = var_35[0];
var_25[1] = var_35[1];
TAIL_CALL fn_4(NULL, var_25, "bld-rtl-dir/wile-rtl2-000037.scm:25");
var_30 = var_33;
goto lbl_31;
}
lval var_36;
var_36 = LVI_BOOL(var_28.vt == LV_NIL);
if (!LV_IS_FALSE(var_36)) {
lval var_37;
lval var_39[8];
var_39[0] = var_27;
var_39[1] = var_29;
var_25[0] = var_39[0];
var_25[1] = var_39[1];
TAIL_CALL fn_4(NULL, var_25, "bld-rtl-dir/wile-rtl2-000037.scm:26");
var_30 = var_37;
goto lbl_31;
}
lval var_40;
if (var_28.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000037.scm:27", "input is not a pair!");
}
var_40 = (var_28.v.pair.car ? *(var_28.v.pair.car) : LVI_NIL());
lval var_41;
if (var_27.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000037.scm:27", "input is not a pair!");
}
var_41 = (var_27.v.pair.car ? *(var_27.v.pair.car) : LVI_NIL());
lval var_42;
{
lval var_43[2];
var_43[0] = var_40;
var_43[1] = var_41;
var_42 = wile_gen_list(2, var_43, NULL);
}
lval var_44;
{
lval var_45[2];
var_45[0] = var_25[0];
var_45[1] = var_42;
var_44 = wile_gen_list(2, var_45, NULL);
}
var_44 = wile_apply_function(&(var_44), "bld-rtl-dir/wile-rtl2-000037.scm:27");
if (!LV_IS_FALSE(var_44)) {
lval var_46;
if (var_28.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000037.scm:28", "input is not a pair!");
}
var_46 = (var_28.v.pair.cdr ? *(var_28.v.pair.cdr) : LVI_NIL());
lval var_47;
if (var_28.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000037.scm:28", "input is not a pair!");
}
var_47 = (var_28.v.pair.car ? *(var_28.v.pair.car) : LVI_NIL());
lval var_48;
{
lptr p1 = NULL, p2 = NULL;
if (var_47.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_47;
}
if (var_29.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_29;
}
var_48 = LVI_PAIR(p1, p2);
}
lval var_49;
lval var_51[8];
var_51[0] = var_25[0];
var_51[1] = var_27;
var_51[2] = var_46;
var_51[3] = var_48;
var_25[0] = var_51[0];
var_25[1] = var_51[1];
var_25[2] = var_51[2];
var_25[3] = var_51[3];
goto lbl_26;
var_30 = var_49;
goto lbl_31;
}
lval var_52;
if (var_27.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000037.scm:30", "input is not a pair!");
}
var_52 = (var_27.v.pair.cdr ? *(var_27.v.pair.cdr) : LVI_NIL());
lval var_53;
if (var_27.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000037.scm:30", "input is not a pair!");
}
var_53 = (var_27.v.pair.car ? *(var_27.v.pair.car) : LVI_NIL());
lval var_54;
{
lptr p1 = NULL, p2 = NULL;
if (var_53.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_53;
}
if (var_29.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_29;
}
var_54 = LVI_PAIR(p1, p2);
}
lval var_55;
lval var_57[8];
var_57[0] = var_25[0];
var_57[1] = var_52;
var_57[2] = var_28;
var_57[3] = var_54;
var_25[0] = var_57[0];
var_25[1] = var_57[1];
var_25[2] = var_57[2];
var_25[3] = var_57[3];
goto lbl_26;
var_30 = var_55;
lbl_31:;
return var_30;
}
// end of function fn_23

// @@@ (one-pass is-lt? lst acc) @@@ bld-rtl-dir/wile-rtl2-000037.scm:31 @@@ fn_58 @@@
static lval fn_58(lptr* var_59, lptr var_60, const char* cloc)
{
lbl_61:;
lval var_62;
lval var_64;
var_64 = LVI_BOOL(var_60[1].vt == LV_NIL);
if (!LV_IS_FALSE(var_64)) {
lval var_65;
{
lval var_66[8];
var_66[0] = var_60[2];
var_65 = wile_list_reverse(NULL, var_66, "bld-rtl-dir/wile-rtl2-000037.scm:33");
}
var_62 = var_65;
goto lbl_63;
}
lval var_67;
if (var_60[1].vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000037.scm:34", "input is not a pair!");
}
var_67 = (var_60[1].v.pair.cdr ? *(var_60[1].v.pair.cdr) : LVI_NIL());
lval var_68;
var_68 = LVI_BOOL(var_67.vt == LV_NIL);
if (!LV_IS_FALSE(var_68)) {
lval var_69;
if (var_60[1].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000037.scm:34", "input is not a pair!");
}
var_69 = (var_60[1].v.pair.car ? *(var_60[1].v.pair.car) : LVI_NIL());
lval var_70;
{
lptr p1 = NULL, p2 = NULL;
if (var_69.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_69;
}
if (var_60[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_60[2];
}
var_70 = LVI_PAIR(p1, p2);
}
lval var_71;
{
lval var_72[8];
var_72[0] = var_70;
var_71 = wile_list_reverse(NULL, var_72, "bld-rtl-dir/wile-rtl2-000037.scm:34");
}
var_62 = var_71;
goto lbl_63;
}
lval var_73;
var_73 = LVI_STRING("cddr");
lval var_74;
{
char* cp = strchr(var_73.v.str, 'r');
var_74 = var_60[1];
while (*(--cp) != 'c') {
if (var_74.vt != LV_PAIR) {
wile_exception("cxr", "bld-rtl-dir/wile-rtl2-000037.scm:36", "input does not have the right structure!");
}
if (*cp == 'a') {
var_74 = (var_74.v.pair.car ? *(var_74.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_74 = (var_74.v.pair.cdr ? *(var_74.v.pair.cdr) : LVI_NIL());
} else {
wile_exception("cxr", "bld-rtl-dir/wile-rtl2-000037.scm:36", "got malformed control string '%s'", var_73.v.str);
}
}
}
lval var_75;
if (var_60[1].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000037.scm:37", "input is not a pair!");
}
var_75 = (var_60[1].v.pair.car ? *(var_60[1].v.pair.car) : LVI_NIL());
lval var_76;
var_76 = LVI_STRING("cadr");
lval var_77;
{
char* cp = strchr(var_76.v.str, 'r');
var_77 = var_60[1];
while (*(--cp) != 'c') {
if (var_77.vt != LV_PAIR) {
wile_exception("cxr", "bld-rtl-dir/wile-rtl2-000037.scm:37", "input does not have the right structure!");
}
if (*cp == 'a') {
var_77 = (var_77.v.pair.car ? *(var_77.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_77 = (var_77.v.pair.cdr ? *(var_77.v.pair.cdr) : LVI_NIL());
} else {
wile_exception("cxr", "bld-rtl-dir/wile-rtl2-000037.scm:37", "got malformed control string '%s'", var_76.v.str);
}
}
}
lval var_78;
var_78 = LVI_NIL();
lval var_79;
lval var_80[8];
var_80[0] = var_60[0];
var_80[1] = var_75;
var_80[2] = var_77;
var_80[3] = var_78;
var_79 = fn_23(NULL, var_80, "bld-rtl-dir/wile-rtl2-000037.scm:37");
lval var_82;
{
lval var_83[8];
var_83[0] = var_79;
var_82 = wile_list_reverse(NULL, var_83, "bld-rtl-dir/wile-rtl2-000037.scm:37");
}
lval var_84;
{
lptr p1 = NULL, p2 = NULL;
if (var_82.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_82;
}
if (var_60[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_60[2];
}
var_84 = LVI_PAIR(p1, p2);
}
lval var_85;
lval var_87[8];
var_87[0] = var_60[0];
var_87[1] = var_74;
var_87[2] = var_84;
var_60[0] = var_87[0];
var_60[1] = var_87[1];
var_60[2] = var_87[2];
goto lbl_61;
var_62 = var_85;
lbl_63:;
return var_62;
}
// end of function fn_58

// @@@ list @@@ bld-rtl-dir/wile-rtl2-000037.scm:40 @@@ fn_92 @@@
static lval fn_92(lptr* var_93, lptr var_94, const char* cloc)
{
lval var_96;
var_96 = var_94[0];
return var_96;
}
// end of prim fn_92

// @@@ (list-sort is-lt? lst) @@@ bld-rtl-dir/wile-rtl2-000037.scm:13 @@@ wile_list_sort @@@
lval wile_list_sort(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_88;
lval var_89;
{
lval var_90[8];
var_90[0] = var_2[1];
var_89 = wile_list_length(NULL, var_90, "bld-rtl-dir/wile-rtl2-000037.scm:39");
}
var_88 = var_89;
lval var_91;
lval var_97;
var_97 = LVI_NIL();
{
lval var_98[8];
var_98[0] = LVI_PROC(fn_92,NULL,-1);
var_98[1] = var_2[1];
var_98[2] = var_97;
var_97 = wile_map(NULL, var_98, "bld-rtl-dir/wile-rtl2-000037.scm:40");
}
var_91 = var_97;
lval var_99;
lval var_100;
var_100 = LVI_INT(1);
var_99 = var_100;
lval var_101;
lval var_102;
switch (var_88.vt) {
case LV_REAL:
var_102 = LVI_BOOL(var_88.v.rv > 0.0);
break;
case LV_RAT:
var_102 = LVI_BOOL((var_88.v.irv.num > 0 && var_88.v.irv.den >= 0) || (var_88.v.irv.num < 0 && var_88.v.irv.den < 0));
break;
case LV_INT:
var_102 = LVI_BOOL(var_88.v.iv > 0);
break;
default:
wile_exception("positive?", "bld-rtl-dir/wile-rtl2-000037.scm:42", "expects a real-valued number");
}
if (LV_IS_FALSE(var_102)) {
lval var_103;
var_103 = LVI_NIL();
var_101 = var_103;
} else {
lval var_107;
lval var_108;
lval var_109;
var_109 = LVI_INT(0);
var_107 = var_109;
lptr var_110 = new_lv(VT_UNINIT);
var_110->v.pair.car = &(var_107);
lbl_105:
lval var_111;
switch (TYPE_COMBO(var_99.vt,var_88.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_111 = LVI_BOOL(var_99.v.iv < var_88.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_111 = LVI_BOOL(var_99.v.iv * var_88.v.irv.den < var_88.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_111 = LVI_BOOL(var_99.v.iv < var_88.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_111 = LVI_BOOL(var_99.v.irv.num < var_88.v.iv * var_99.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_111 = LVI_BOOL(var_99.v.irv.num * var_88.v.irv.den < var_88.v.irv.num * var_99.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_111 = LVI_BOOL(var_99.v.irv.num < var_88.v.rv * var_99.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_111 = LVI_BOOL(var_99.v.rv < var_88.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_111 = LVI_BOOL(var_99.v.rv * var_88.v.irv.den < var_88.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_111 = LVI_BOOL(var_99.v.rv < var_88.v.rv);
break;
default:
wile_exception("<", "bld-rtl-dir/wile-rtl2-000037.scm:43", "inputs are not real-valued numbers");
break;
}
lval var_112;
var_112 = LVI_BOOL(LV_IS_FALSE(var_111));
if (!LV_IS_FALSE(var_112)) {
goto lbl_106;
}
lval var_113;
var_113 = LVI_INT(2);
lval var_114;
var_114 = LVI_INT(var_113.v.iv * var_99.v.iv);
var_99 = var_114;
lval var_115;
var_115 = LVI_NIL();
lval var_116;
lval var_117[8];
var_117[0] = var_2[0];
var_117[1] = var_91;
var_117[2] = var_115;
var_116 = fn_58(NULL, var_117, "bld-rtl-dir/wile-rtl2-000037.scm:45");
var_91 = var_116;
lval var_119;
var_119 = LVI_INT(1);
lval var_120;
var_120 = LVI_INT(var_107.v.iv + var_119.v.iv);
var_108 = var_120;
var_107 = var_108;
goto lbl_105;
lbl_106:;
*var_110 = var_107;
lval var_121;
if (var_91.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000037.scm:46", "input is not a pair!");
}
var_121 = (var_91.v.pair.car ? *(var_91.v.pair.car) : LVI_NIL());
var_101 = var_121;
}
return var_101;
}
// end of function wile_list_sort
