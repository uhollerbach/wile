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
static lval fn_4(lptr*, lptr);	// (merge1 lst acc)
static lval fn_5(lptr*, lptr);	// (merge2 is-lt? lst1 lst2 acc)
static lval fn_6(lptr*, lptr);	// (one-pass is-lt? lst acc)

// definitions

// @@@ (merge1 lst acc) @@@ bld-rtl-dir/wile-rtl2-000036.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_7, lptr var_8)
{
lval var_10;
var_10 = var_8[0];
lval var_11;
var_11 = var_8[1];
lval var_13;
lval var_14;
lval var_15;
var_15 = LVI_INT(0);
var_13 = var_15;
do {
lval var_16;
var_16 = LVI_BOOL(var_10.vt == LV_NIL);
if (!LV_IS_FALSE(var_16)) {
break;
}
lval var_17;
if (var_10.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_17 = (var_10.v.pair.car ? *(var_10.v.pair.car) : LVI_NIL());
lval var_18;
{
lptr p1 = NULL, p2 = NULL;
if (var_17.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_17;
}
if (var_11.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_11;
}
var_18 = LVI_PAIR(p1, p2);
}
var_11 = var_18;
lval var_19;
if (var_10.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_19 = (var_10.v.pair.cdr ? *(var_10.v.pair.cdr) : LVI_NIL());
var_10 = var_19;
lval var_20;
var_20 = LVI_INT(1);
lval var_21;
var_21 = LVI_INT(var_13.v.iv + var_20.v.iv);
var_14 = var_21;
var_13 = var_14;
} while (1);
return var_11;
}
// end of function fn_4

// @@@ (merge2 is-lt? lst1 lst2 acc) @@@ bld-rtl-dir/wile-rtl2-000036.scm:21 @@@ fn_5 @@@
static lval fn_5(lptr* var_22, lptr var_23)
{
lbl_24:;
lval var_25;
var_25 = var_23[1];
lval var_26;
var_26 = var_23[2];
lval var_27;
var_27 = var_23[3];
lval var_28;
do {
lval var_29;
var_29 = LVI_BOOL(var_25.vt == LV_NIL);
if (!LV_IS_FALSE(var_29)) {
lval var_32[8];
var_32[0] = var_26;
var_32[1] = var_27;
var_23[0] = var_32[0];
var_23[1] = var_32[1];
TAIL_CALL fn_4(NULL, var_23);
}
lval var_33;
var_33 = LVI_BOOL(var_26.vt == LV_NIL);
if (!LV_IS_FALSE(var_33)) {
lval var_36[8];
var_36[0] = var_25;
var_36[1] = var_27;
var_23[0] = var_36[0];
var_23[1] = var_36[1];
TAIL_CALL fn_4(NULL, var_23);
}
lval var_37;
if (var_26.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_37 = (var_26.v.pair.car ? *(var_26.v.pair.car) : LVI_NIL());
lval var_38;
if (var_25.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_38 = (var_25.v.pair.car ? *(var_25.v.pair.car) : LVI_NIL());
lval var_39;
{
lval var_40[2];
var_40[0] = var_37;
var_40[1] = var_38;
var_39 = wile_gen_list(2, var_40, NULL);
}
lval var_41;
{
lval var_42[2];
var_42[0] = var_23[0];
var_42[1] = var_39;
var_41 = wile_gen_list(2, var_42, NULL);
}
var_41 = wile_apply_function(&(var_41), __FILE__, __LINE__);
if (!LV_IS_FALSE(var_41)) {
lval var_43;
if (var_26.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_43 = (var_26.v.pair.cdr ? *(var_26.v.pair.cdr) : LVI_NIL());
lval var_44;
if (var_26.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_44 = (var_26.v.pair.car ? *(var_26.v.pair.car) : LVI_NIL());
lval var_45;
{
lptr p1 = NULL, p2 = NULL;
if (var_44.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_44;
}
if (var_27.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_27;
}
var_45 = LVI_PAIR(p1, p2);
}
lval var_48[8];
var_48[0] = var_23[0];
var_48[1] = var_25;
var_48[2] = var_43;
var_48[3] = var_45;
var_23[0] = var_48[0];
var_23[1] = var_48[1];
var_23[2] = var_48[2];
var_23[3] = var_48[3];
goto lbl_24;	// selfie
}
lval var_49;
if (var_25.vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_49 = (var_25.v.pair.cdr ? *(var_25.v.pair.cdr) : LVI_NIL());
lval var_50;
if (var_25.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_50 = (var_25.v.pair.car ? *(var_25.v.pair.car) : LVI_NIL());
lval var_51;
{
lptr p1 = NULL, p2 = NULL;
if (var_50.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_50;
}
if (var_27.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_27;
}
var_51 = LVI_PAIR(p1, p2);
}
lval var_54[8];
var_54[0] = var_23[0];
var_54[1] = var_49;
var_54[2] = var_26;
var_54[3] = var_51;
var_23[0] = var_54[0];
var_23[1] = var_54[1];
var_23[2] = var_54[2];
var_23[3] = var_54[3];
goto lbl_24;	// selfie
} while (0);
return var_28;
}
// end of function fn_5

// @@@ (one-pass is-lt? lst acc) @@@ bld-rtl-dir/wile-rtl2-000036.scm:31 @@@ fn_6 @@@
static lval fn_6(lptr* var_55, lptr var_56)
{
lbl_57:;
lval var_58;
do {
lval var_59;
var_59 = LVI_BOOL(var_56[1].vt == LV_NIL);
if (!LV_IS_FALSE(var_59)) {
lval var_60;
{
lval var_61[8];
var_61[0] = var_56[2];
var_60 = wile_list_reverse(NULL, var_61);
}
var_58 = var_60;
break;
}
lval var_62;
if (var_56[1].vt != LV_PAIR) {
WILE_EX("cdr", "input is not a pair!");
}
var_62 = (var_56[1].v.pair.cdr ? *(var_56[1].v.pair.cdr) : LVI_NIL());
lval var_63;
var_63 = LVI_BOOL(var_62.vt == LV_NIL);
if (!LV_IS_FALSE(var_63)) {
lval var_64;
if (var_56[1].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_64 = (var_56[1].v.pair.car ? *(var_56[1].v.pair.car) : LVI_NIL());
lval var_65;
{
lptr p1 = NULL, p2 = NULL;
if (var_64.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_64;
}
if (var_56[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_56[2];
}
var_65 = LVI_PAIR(p1, p2);
}
lval var_66;
{
lval var_67[8];
var_67[0] = var_65;
var_66 = wile_list_reverse(NULL, var_67);
}
var_58 = var_66;
break;
}
lval var_68;
var_68 = LVI_STRING("cddr");
lval var_69;
{
char* cp = strchr(var_68.v.str, 'r');
var_69 = var_56[1];
while (*(--cp) != 'c') {
if (var_69.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_69 = (var_69.v.pair.car ? *(var_69.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_69 = (var_69.v.pair.cdr ? *(var_69.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_68.v.str);
}
}
}
lval var_70;
if (var_56[1].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_70 = (var_56[1].v.pair.car ? *(var_56[1].v.pair.car) : LVI_NIL());
lval var_71;
var_71 = LVI_STRING("cadr");
lval var_72;
{
char* cp = strchr(var_71.v.str, 'r');
var_72 = var_56[1];
while (*(--cp) != 'c') {
if (var_72.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_72 = (var_72.v.pair.car ? *(var_72.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_72 = (var_72.v.pair.cdr ? *(var_72.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_71.v.str);
}
}
}
lval var_73;
var_73 = LVI_NIL();
lval var_74;
lval var_75[8];
var_75[0] = var_56[0];
var_75[1] = var_70;
var_75[2] = var_72;
var_75[3] = var_73;
var_74 = fn_5(NULL, var_75);
lval var_77;
{
lval var_78[8];
var_78[0] = var_74;
var_77 = wile_list_reverse(NULL, var_78);
}
lval var_79;
{
lptr p1 = NULL, p2 = NULL;
if (var_77.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_77;
}
if (var_56[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_56[2];
}
var_79 = LVI_PAIR(p1, p2);
}
lval var_82[8];
var_82[0] = var_56[0];
var_82[1] = var_69;
var_82[2] = var_79;
var_56[0] = var_82[0];
var_56[1] = var_82[1];
var_56[2] = var_82[2];
goto lbl_57;	// selfie
} while (0);
return var_58;
}
// end of function fn_6

// @@@ list @@@ bld-rtl-dir/wile-rtl2-000036.scm:40 @@@ fn_87 @@@
static lval fn_87(lptr* var_88, lptr var_89)
{
lval var_91;
var_91 = var_89[0];
return var_91;
}
// end of prim fn_87

// @@@ (list-sort is-lt? lst) @@@ bld-rtl-dir/wile-rtl2-000036.scm:13 @@@ wile_list_sort @@@
lval wile_list_sort(lptr* var_1, lptr var_2)
{
lval var_83;
lval var_84;
{
lval var_85[8];
var_85[0] = var_2[1];
var_84 = wile_list_length(NULL, var_85);
}
var_83 = var_84;
lval var_86;
lval var_92;
var_92 = LVI_NIL();
{
lval var_93[8];
var_93[0] = LVI_PROC(fn_87,NULL,-1);
var_93[1] = var_2[1];
var_93[2] = var_92;
var_92 = wile_map(NULL, var_93);
}
var_86 = var_92;
lval var_94;
lval var_95;
var_95 = LVI_INT(1);
var_94 = var_95;
lval var_96;
lval var_97;
switch (var_83.vt) {
case LV_REAL:
var_97 = LVI_BOOL(var_83.v.rv > 0.0);
break;
case LV_RAT:
var_97 = LVI_BOOL((var_83.v.irv.num > 0 && var_83.v.irv.den >= 0) || (var_83.v.irv.num < 0 && var_83.v.irv.den < 0));
break;
case LV_INT:
var_97 = LVI_BOOL(var_83.v.iv > 0);
break;
default:
WILE_EX("positive?", "expects a real-valued number");
}
if (LV_IS_FALSE(var_97)) {
lval var_98;
var_98 = LVI_NIL();
var_96 = var_98;
} else {
lval var_100;
lval var_101;
lval var_102;
var_102 = LVI_INT(0);
var_100 = var_102;
do {
lval var_103;
switch (TYPE_COMBO(var_94.vt,var_83.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_103 = LVI_BOOL(var_94.v.iv < var_83.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_103 = LVI_BOOL(var_94.v.iv * var_83.v.irv.den < var_83.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_103 = LVI_BOOL(var_94.v.iv < var_83.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_103 = LVI_BOOL(var_94.v.irv.num < var_83.v.iv * var_94.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_103 = LVI_BOOL(var_94.v.irv.num * var_83.v.irv.den < var_83.v.irv.num * var_94.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_103 = LVI_BOOL(var_94.v.irv.num < var_83.v.rv * var_94.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_103 = LVI_BOOL(var_94.v.rv < var_83.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_103 = LVI_BOOL(var_94.v.rv * var_83.v.irv.den < var_83.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_103 = LVI_BOOL(var_94.v.rv < var_83.v.rv);
break;
default:
WILE_EX("<", "inputs are not real-valued numbers");
break;
}
lval var_104;
var_104 = LVI_BOOL(LV_IS_FALSE(var_103));
if (!LV_IS_FALSE(var_104)) {
break;
}
lval var_105;
var_105 = LVI_INT(2);
lval var_106;
var_106 = LVI_INT(var_105.v.iv * var_94.v.iv);
var_94 = var_106;
lval var_107;
var_107 = LVI_NIL();
lval var_108;
lval var_109[8];
var_109[0] = var_2[0];
var_109[1] = var_86;
var_109[2] = var_107;
var_108 = fn_6(NULL, var_109);
var_86 = var_108;
lval var_111;
var_111 = LVI_INT(1);
lval var_112;
var_112 = LVI_INT(var_100.v.iv + var_111.v.iv);
var_101 = var_112;
var_100 = var_101;
} while (1);
lval var_113;
if (var_86.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_113 = (var_86.v.pair.car ? *(var_86.v.pair.car) : LVI_NIL());
var_96 = var_113;
}
return var_96;
}
// end of function wile_list_sort
