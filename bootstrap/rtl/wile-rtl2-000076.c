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
static lval fn_1(lptr*, lptr);	// (bracket-error)
static lval fn_2(lptr*, lptr);	// (budget-error val)
static lval fn_3(lptr*, lptr);	// (exact v)
static lval fn_4(lptr*, lptr);	// (approx v v1 v2)
static lval fn_37(lptr*, lptr);
static lval fn_93(lptr*, lptr);
static lval fn_184(lptr*, lptr);

// definitions

// @@@ (bracket-error) @@@ bld-rtl-dir/wile-rtl2-000076.scm:13 @@@ fn_1 @@@
static lval fn_1(lptr* var_5, lptr var_6)
{
lval var_8;
var_8 = LVI_STRING("un-bracketed root or minimum");
lval var_9;
{
lval var_10[1];
var_10[0] = var_8;
var_9 = wile_gen_list(1, var_10, NULL);
}
if (var_9.vt == LV_PAIR && (var_9.v.pair.cdr == NULL || var_9.v.pair.cdr->vt == LV_NIL)) {
var_9 = (var_9.v.pair.car ? *(var_9.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_9;
cachalot->l_whence = 0;
cachalot->c_whence = LISP_WHENCE;
longjmp(cachalot->cenv, 1);
return var_9;
}
// end of function fn_1

// @@@ (budget-error val) @@@ bld-rtl-dir/wile-rtl2-000076.scm:16 @@@ fn_2 @@@
static lval fn_2(lptr* var_11, lptr var_12)
{
lval var_14;
var_14 = LVI_STRING("function evaluation budget exhausted");
lval var_15;
{
lval var_16[2];
var_16[0] = var_14;
var_16[1] = var_12[0];
var_15 = wile_gen_list(2, var_16, NULL);
}
lval var_17;
{
lval var_18[1];
var_18[0] = var_15;
var_17 = wile_gen_list(1, var_18, NULL);
}
if (var_17.vt == LV_PAIR && (var_17.v.pair.cdr == NULL || var_17.v.pair.cdr->vt == LV_NIL)) {
var_17 = (var_17.v.pair.car ? *(var_17.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_17;
cachalot->l_whence = 0;
cachalot->c_whence = LISP_WHENCE;
longjmp(cachalot->cenv, 1);
return var_17;
}
// end of function fn_2

// @@@ (exact v) @@@ bld-rtl-dir/wile-rtl2-000076.scm:19 @@@ fn_3 @@@
static lval fn_3(lptr* var_19, lptr var_20)
{
lval var_22;
var_22 = LVI_INT(0);
lval var_23;
{
lval var_24[2];
var_24[0] = var_20[0];
var_24[1] = var_22;
var_23 = wile_gen_list(2, var_24, NULL);
}
return var_23;
}
// end of function fn_3

// @@@ (approx v v1 v2) @@@ bld-rtl-dir/wile-rtl2-000076.scm:22 @@@ fn_4 @@@
static lval fn_4(lptr* var_25, lptr var_26)
{
lval var_28;
{
lval var_30[2];
var_30[0] = var_26[1];
var_30[1] = var_26[2];
var_28 = wile_gen_list(2, var_30, NULL);
}
{
lval var_29[8];
var_29[0] = var_28;
var_28 = wile_subtract(NULL, var_29);
}
lval var_31;
switch (var_28.vt) {
case LV_INT:
var_31 = LVI_INT(WILE_ABS(var_28.v.iv));
break;
case LV_RAT:
var_31 = LVI_RAT(WILE_ABS(var_28.v.irv.num), WILE_ABS(var_28.v.irv.den));
break;
case LV_REAL:
var_31 = LVI_REAL(WILE_ABS(var_28.v.rv));
break;
case LV_CMPLX:
var_31 = LVI_REAL(CABS(var_28.v.cv));
break;
default:
WILE_EX("abs", "got a non-numeric argument");
}
lval var_32;
{
lval var_33[2];
var_33[0] = var_26[0];
var_33[1] = var_31;
var_32 = wile_gen_list(2, var_33, NULL);
}
return var_32;
}
// end of function fn_4

// @@@ lambda (s) @@@ bld-rtl-dir/wile-rtl2-000076.scm:28 @@@ fn_37 @@@
static lval fn_37(lptr* var_38, lptr var_39)
{
lbl_40:;
lval var_41;
lval var_42;
{
lval var_44[2];
var_44[0] = V_CLOS(var_38,0);
var_44[1] = var_39[0];
var_42 = wile_gen_list(2, var_44, NULL);
}
{
lval var_43[8];
var_43[0] = var_42;
var_42 = wile_subtract(NULL, var_43);
}
var_41 = var_42;
lval var_45;
lval var_46;
{
lval var_48[2];
var_48[0] = V_CLOS(var_38,0);
var_48[1] = var_39[0];
var_46 = wile_gen_list(2, var_48, NULL);
}
{
lval var_47[8];
var_47[0] = var_46;
var_46 = wile_add(NULL, var_47);
}
var_45 = var_46;
lval var_49;
lval var_50;
{
lval var_51[1];
var_51[0] = var_41;
var_50 = wile_gen_list(1, var_51, NULL);
}
lval var_52;
{
lval var_53[2];
var_53[0] = V_CLOS(var_38,1);
var_53[1] = var_50;
var_52 = wile_gen_list(2, var_53, NULL);
}
var_52 = wile_apply_function(&(var_52), __FILE__, __LINE__);
var_49 = var_52;
lval var_54;
lval var_55;
{
lval var_56[1];
var_56[0] = var_45;
var_55 = wile_gen_list(1, var_56, NULL);
}
lval var_57;
{
lval var_58[2];
var_58[0] = V_CLOS(var_38,1);
var_58[1] = var_55;
var_57 = wile_gen_list(2, var_58, NULL);
}
var_57 = wile_apply_function(&(var_57), __FILE__, __LINE__);
var_54 = var_57;
lval var_59;
lval var_60;
switch (var_49.vt) {
case LV_INT:
var_60 = LVI_INT(WILE_SIGN(var_49.v.iv));
break;
case LV_RAT:
var_60 = LVI_INT(WILE_SIGN(var_49.v.irv.num));
if (var_49.v.irv.den < 0) {
var_60.v.iv = -var_60.v.iv;
}
break;
case LV_REAL:
var_60 = LVI_INT(WILE_SIGN(var_49.v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_61;
switch (var_54.vt) {
case LV_INT:
var_61 = LVI_INT(WILE_SIGN(var_54.v.iv));
break;
case LV_RAT:
var_61 = LVI_INT(WILE_SIGN(var_54.v.irv.num));
if (var_54.v.irv.den < 0) {
var_61.v.iv = -var_61.v.iv;
}
break;
case LV_REAL:
var_61 = LVI_INT(WILE_SIGN(var_54.v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_62;
switch (TYPE_COMBO(var_60.vt,var_61.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_62 = LVI_BOOL(var_60.v.iv != var_61.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_62 = LVI_BOOL(var_60.v.iv * var_61.v.irv.den != var_61.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_62 = LVI_BOOL(var_60.v.iv != var_61.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_62 = LVI_BOOL(var_60.v.irv.num != var_61.v.iv * var_60.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_62 = LVI_BOOL(var_60.v.irv.num * var_61.v.irv.den != var_61.v.irv.num * var_60.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_62 = LVI_BOOL(var_60.v.irv.num != var_61.v.rv * var_60.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_62 = LVI_BOOL(var_60.v.rv != var_61.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_62 = LVI_BOOL(var_60.v.rv * var_61.v.irv.den != var_61.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_62 = LVI_BOOL(var_60.v.rv != var_61.v.rv);
break;
default:
WILE_EX("!=", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_62)) {
lval var_63;
var_63 = LVI_REAL(1.41400000000000000000000000000000005e+00Q);
lval var_64;
{
lval var_66[2];
var_66[0] = var_63;
var_66[1] = var_39[0];
var_64 = wile_gen_list(2, var_66, NULL);
}
{
lval var_65[8];
var_65[0] = var_64;
var_64 = wile_multiply(NULL, var_65);
}
lval var_69[8];
var_69[0] = var_64;
var_39[0] = var_69[0];
goto lbl_40;	// selfie
} else {
lval var_70;
{
lval var_71[2];
var_71[0] = var_41;
var_71[1] = var_45;
var_70 = wile_gen_list(2, var_71, NULL);
}
var_59 = var_70;
}
return var_59;
}
// end of lambda fn_37

// @@@ (root-bracket f x scale) @@@ bld-rtl-dir/wile-rtl2-000076.scm:27 @@@ wile_root_bracket @@@
lval wile_root_bracket(lptr* var_34, lptr var_35)
{
MK_CLOS(var_38,2);
lptr var_72 = new_lv(VT_UNINIT);
var_72->v.pair.car = &(var_35[0]);
P_CLOS(var_38,1) = var_72;
lptr var_73 = new_lv(VT_UNINIT);
var_73->v.pair.car = &(var_35[1]);
P_CLOS(var_38,0) = var_73;
lval var_74;
lval var_75[8];
var_75[0] = var_35[2];
var_74 = fn_37(var_38, var_75);
*var_72 = var_35[0];
*var_73 = var_35[1];
return var_74;
}
// end of function wile_root_bracket
lval var_80;
lval var_81;
lval var_82;
lval var_83;

// @@@ lambda (ne a fa b fb) @@@ bld-rtl-dir/wile-rtl2-000076.scm:45 @@@ fn_93 @@@
static lval fn_93(lptr* var_94, lptr var_95)
{
lval var_97;
lval var_98;
var_98 = LVI_REAL(5.00000000000000000000000000000000000e-01Q);
lval var_99;
{
lval var_101[2];
var_101[0] = var_95[1];
var_101[1] = var_95[3];
var_99 = wile_gen_list(2, var_101, NULL);
}
{
lval var_100[8];
var_100[0] = var_99;
var_99 = wile_add(NULL, var_100);
}
lval var_102;
{
lval var_104[2];
var_104[0] = var_98;
var_104[1] = var_99;
var_102 = wile_gen_list(2, var_104, NULL);
}
{
lval var_103[8];
var_103[0] = var_102;
var_102 = wile_multiply(NULL, var_103);
}
var_97 = var_102;
lval var_105;
lval var_106;
{
lval var_107[1];
var_107[0] = var_97;
var_106 = wile_gen_list(1, var_107, NULL);
}
lval var_108;
{
lval var_109[2];
var_109[0] = V_CLOS(var_94,0);
var_109[1] = var_106;
var_108 = wile_gen_list(2, var_109, NULL);
}
var_108 = wile_apply_function(&(var_108), __FILE__, __LINE__);
var_105 = var_108;
lval var_110;
lval var_111;
lval var_112[8];
var_112[0] = var_97;
var_112[1] = var_95[1];
var_112[2] = var_95[3];
var_111 = fn_4(NULL, var_112);
var_110 = var_111;
lval var_114;
lval var_115;
var_115 = LVI_INT(1);
lval var_116;
{
lval var_118[2];
var_118[0] = var_95[0];
var_118[1] = var_115;
var_116 = wile_gen_list(2, var_118, NULL);
}
{
lval var_117[8];
var_117[0] = var_116;
var_116 = wile_subtract(NULL, var_117);
}
var_114 = var_116;
lval var_119;
do {
lval var_120;
switch (var_95[0].vt) {
case LV_REAL:
var_120 = LVI_BOOL(var_95[0].v.rv == 0.0);
break;
case LV_RAT:
var_120 = LVI_BOOL((var_95[0].v.irv.num == 0 && var_95[0].v.irv.den != 0));
break;
case LV_INT:
var_120 = LVI_BOOL(var_95[0].v.iv == 0);
break;
case LV_CMPLX:
var_120 = LVI_BOOL(CREAL(var_95[0].v.cv) == 0.0 && CIMAG(var_95[0].v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_120)) {
lval var_123[8];
var_123[0] = var_110;
var_95[0] = var_123[0];
TAIL_CALL fn_2(NULL, var_95);
}
lval var_124;
switch (var_105.vt) {
case LV_REAL:
var_124 = LVI_BOOL(var_105.v.rv == 0.0);
break;
case LV_RAT:
var_124 = LVI_BOOL((var_105.v.irv.num == 0 && var_105.v.irv.den != 0));
break;
case LV_INT:
var_124 = LVI_BOOL(var_105.v.iv == 0);
break;
case LV_CMPLX:
var_124 = LVI_BOOL(CREAL(var_105.v.cv) == 0.0 && CIMAG(var_105.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_124)) {
lval var_127[8];
var_127[0] = var_97;
var_95[0] = var_127[0];
TAIL_CALL fn_3(NULL, var_95);
}
lval var_128;
{
lval var_129[2];
var_129[0] = var_95[1];
var_129[1] = var_95[3];
var_128 = wile_gen_list(2, var_129, NULL);
}
lval var_130;
{
lval var_131[2];
var_131[0] = V_CLOS(var_94,1);
var_131[1] = var_128;
var_130 = wile_gen_list(2, var_131, NULL);
}
var_130 = wile_apply_function(&(var_130), __FILE__, __LINE__);
if (!LV_IS_FALSE(var_130)) {
var_119 = var_110;
break;
}
lval var_132;
switch (var_105.vt) {
case LV_REAL:
var_132 = LVI_BOOL(var_105.v.rv < 0.0);
break;
case LV_RAT:
var_132 = LVI_BOOL((var_105.v.irv.num < 0 && var_105.v.irv.den >= 0) || (var_105.v.irv.num > 0 && var_105.v.irv.den < 0));
break;
case LV_INT:
var_132 = LVI_BOOL(var_105.v.iv < 0);
break;
default:
WILE_EX("negative?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_132)) {
lval var_133;
{
lval var_134[5];
var_134[0] = var_114;
var_134[1] = var_97;
var_134[2] = var_105;
var_134[3] = var_95[3];
var_134[4] = var_95[4];
var_133 = wile_gen_list(5, var_134, NULL);
}
lval var_135;
{
lval var_136[2];
var_136[0] = V_CLOS(var_94,2);
var_136[1] = var_133;
var_135 = wile_gen_list(2, var_136, NULL);
}
var_135 = wile_apply_function(&(var_135), __FILE__, __LINE__);
var_119 = var_135;
break;
}
lval var_137;
{
lval var_138[5];
var_138[0] = var_114;
var_138[1] = var_95[1];
var_138[2] = var_95[2];
var_138[3] = var_97;
var_138[4] = var_105;
var_137 = wile_gen_list(5, var_138, NULL);
}
lval var_139;
{
lval var_140[2];
var_140[0] = V_CLOS(var_94,2);
var_140[1] = var_137;
var_139 = wile_gen_list(2, var_140, NULL);
}
var_139 = wile_apply_function(&(var_139), __FILE__, __LINE__);
var_119 = var_139;
} while (0);
return var_119;
}
// end of lambda fn_93

// @@@ (root-bisect et fn a0 b0) @@@ bld-rtl-dir/wile-rtl2-000076.scm:41 @@@ wile_root_bisect @@@
lval wile_root_bisect(lptr* var_77, lptr var_78)
{
lval var_84;
{
lval var_85[1];
var_85[0] = var_78[2];
var_84 = wile_gen_list(1, var_85, NULL);
}
lval var_86;
{
lval var_87[2];
var_87[0] = var_78[1];
var_87[1] = var_84;
var_86 = wile_gen_list(2, var_87, NULL);
}
var_86 = wile_apply_function(&(var_86), __FILE__, __LINE__);
var_80 = var_86;
lval var_88;
{
lval var_89[1];
var_89[0] = var_78[3];
var_88 = wile_gen_list(1, var_89, NULL);
}
lval var_90;
{
lval var_91[2];
var_91[0] = var_78[1];
var_91[1] = var_88;
var_90 = wile_gen_list(2, var_91, NULL);
}
var_90 = wile_apply_function(&(var_90), __FILE__, __LINE__);
var_81 = var_90;
lval var_92;
var_92 = LVI_INT(128);
var_82 = var_92;
MK_CLOS(var_94,3);
lptr var_141 = new_lv(VT_UNINIT);
var_141->v.pair.car = &(var_83);
P_CLOS(var_94,2) = var_141;
lptr var_142 = new_lv(VT_UNINIT);
var_142->v.pair.car = &(var_78[0]);
P_CLOS(var_94,1) = var_142;
lptr var_143 = new_lv(VT_UNINIT);
var_143->v.pair.car = &(var_78[1]);
P_CLOS(var_94,0) = var_143;
var_83 = LVI_PROC(fn_93,var_94,5);
lval var_144;
do {
lval var_145;
switch (var_80.vt) {
case LV_REAL:
var_145 = LVI_BOOL(var_80.v.rv == 0.0);
break;
case LV_RAT:
var_145 = LVI_BOOL((var_80.v.irv.num == 0 && var_80.v.irv.den != 0));
break;
case LV_INT:
var_145 = LVI_BOOL(var_80.v.iv == 0);
break;
case LV_CMPLX:
var_145 = LVI_BOOL(CREAL(var_80.v.cv) == 0.0 && CIMAG(var_80.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_145)) {
lval var_148[8];
var_148[0] = var_78[2];
var_78[0] = var_148[0];
TAIL_CALL fn_3(NULL, var_78);
}
lval var_149;
switch (var_81.vt) {
case LV_REAL:
var_149 = LVI_BOOL(var_81.v.rv == 0.0);
break;
case LV_RAT:
var_149 = LVI_BOOL((var_81.v.irv.num == 0 && var_81.v.irv.den != 0));
break;
case LV_INT:
var_149 = LVI_BOOL(var_81.v.iv == 0);
break;
case LV_CMPLX:
var_149 = LVI_BOOL(CREAL(var_81.v.cv) == 0.0 && CIMAG(var_81.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_149)) {
lval var_152[8];
var_152[0] = var_78[3];
var_78[0] = var_152[0];
TAIL_CALL fn_3(NULL, var_78);
}
lval var_153;
switch (var_80.vt) {
case LV_INT:
var_153 = LVI_INT(WILE_SIGN(var_80.v.iv));
break;
case LV_RAT:
var_153 = LVI_INT(WILE_SIGN(var_80.v.irv.num));
if (var_80.v.irv.den < 0) {
var_153.v.iv = -var_153.v.iv;
}
break;
case LV_REAL:
var_153 = LVI_INT(WILE_SIGN(var_80.v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_154;
switch (var_81.vt) {
case LV_INT:
var_154 = LVI_INT(WILE_SIGN(var_81.v.iv));
break;
case LV_RAT:
var_154 = LVI_INT(WILE_SIGN(var_81.v.irv.num));
if (var_81.v.irv.den < 0) {
var_154.v.iv = -var_154.v.iv;
}
break;
case LV_REAL:
var_154 = LVI_INT(WILE_SIGN(var_81.v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_155;
switch (TYPE_COMBO(var_153.vt,var_154.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_155 = LVI_BOOL(var_153.v.iv == var_154.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_155 = LVI_BOOL(var_153.v.iv * var_154.v.irv.den == var_154.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_155 = LVI_BOOL(var_153.v.iv == var_154.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_155 = LVI_BOOL(var_153.v.irv.num == var_154.v.iv * var_153.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_155 = LVI_BOOL(var_153.v.irv.num * var_154.v.irv.den == var_154.v.irv.num * var_153.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_155 = LVI_BOOL(var_153.v.irv.num == var_154.v.rv * var_153.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_155 = LVI_BOOL(var_153.v.rv == var_154.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_155 = LVI_BOOL(var_153.v.rv * var_154.v.irv.den == var_154.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_155 = LVI_BOOL(var_153.v.rv == var_154.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_155)) {
TAIL_CALL fn_1(NULL, var_78);
}
lval var_159;
switch (var_80.vt) {
case LV_REAL:
var_159 = LVI_BOOL(var_80.v.rv < 0.0);
break;
case LV_RAT:
var_159 = LVI_BOOL((var_80.v.irv.num < 0 && var_80.v.irv.den >= 0) || (var_80.v.irv.num > 0 && var_80.v.irv.den < 0));
break;
case LV_INT:
var_159 = LVI_BOOL(var_80.v.iv < 0);
break;
default:
WILE_EX("negative?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_159)) {
lval var_160;
{
lval var_161[5];
var_161[0] = var_82;
var_161[1] = var_78[2];
var_161[2] = var_80;
var_161[3] = var_78[3];
var_161[4] = var_81;
var_160 = wile_gen_list(5, var_161, NULL);
}
lval var_162;
{
lval var_163[2];
var_163[0] = var_83;
var_163[1] = var_160;
var_162 = wile_gen_list(2, var_163, NULL);
}
var_162 = wile_apply_function(&(var_162), __FILE__, __LINE__);
var_144 = var_162;
break;
}
lval var_164;
{
lval var_165[5];
var_165[0] = var_82;
var_165[1] = var_78[3];
var_165[2] = var_81;
var_165[3] = var_78[2];
var_165[4] = var_80;
var_164 = wile_gen_list(5, var_165, NULL);
}
lval var_166;
{
lval var_167[2];
var_167[0] = var_83;
var_167[1] = var_164;
var_166 = wile_gen_list(2, var_167, NULL);
}
var_166 = wile_apply_function(&(var_166), __FILE__, __LINE__);
var_144 = var_166;
} while (0);
*var_141 = var_83;
*var_143 = var_78[1];
*var_142 = var_78[0];
return var_144;
}
// end of function wile_root_bisect
lval var_171;
lval var_172;
lval var_173;
lval var_174;

// @@@ lambda (ne a fa b fb) @@@ bld-rtl-dir/wile-rtl2-000076.scm:94 @@@ fn_184 @@@
static lval fn_184(lptr* var_185, lptr var_186)
{
lval var_188;
lval var_189;
var_189 = LVI_REAL(5.00000000000000000000000000000000000e-01Q);
lval var_190;
{
lval var_192[2];
var_192[0] = var_186[1];
var_192[1] = var_186[3];
var_190 = wile_gen_list(2, var_192, NULL);
}
{
lval var_191[8];
var_191[0] = var_190;
var_190 = wile_add(NULL, var_191);
}
lval var_193;
{
lval var_195[2];
var_195[0] = var_189;
var_195[1] = var_190;
var_193 = wile_gen_list(2, var_195, NULL);
}
{
lval var_194[8];
var_194[0] = var_193;
var_193 = wile_multiply(NULL, var_194);
}
var_188 = var_193;
lval var_196;
lval var_197;
{
lval var_198[1];
var_198[0] = var_188;
var_197 = wile_gen_list(1, var_198, NULL);
}
lval var_199;
{
lval var_200[2];
var_200[0] = V_CLOS(var_185,0);
var_200[1] = var_197;
var_199 = wile_gen_list(2, var_200, NULL);
}
var_199 = wile_apply_function(&(var_199), __FILE__, __LINE__);
var_196 = var_199;
lval var_201;
lval var_202;
{
lval var_204[2];
var_204[0] = var_188;
var_204[1] = var_186[1];
var_202 = wile_gen_list(2, var_204, NULL);
}
{
lval var_203[8];
var_203[0] = var_202;
var_202 = wile_subtract(NULL, var_203);
}
lval var_205;
{
lval var_207[2];
var_207[0] = var_186[2];
var_207[1] = var_186[4];
var_205 = wile_gen_list(2, var_207, NULL);
}
{
lval var_206[8];
var_206[0] = var_205;
var_205 = wile_subtract(NULL, var_206);
}
lval var_208;
switch (var_205.vt) {
case LV_INT:
var_208 = LVI_INT(WILE_SIGN(var_205.v.iv));
break;
case LV_RAT:
var_208 = LVI_INT(WILE_SIGN(var_205.v.irv.num));
if (var_205.v.irv.den < 0) {
var_208.v.iv = -var_208.v.iv;
}
break;
case LV_REAL:
var_208 = LVI_INT(WILE_SIGN(var_205.v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_209;
{
lval var_211[3];
var_211[0] = var_202;
var_211[1] = var_196;
var_211[2] = var_208;
var_209 = wile_gen_list(3, var_211, NULL);
}
{
lval var_210[8];
var_210[0] = var_209;
var_209 = wile_multiply(NULL, var_210);
}
lval var_212;
{
lval var_214[2];
var_214[0] = var_196;
var_214[1] = var_196;
var_212 = wile_gen_list(2, var_214, NULL);
}
{
lval var_213[8];
var_213[0] = var_212;
var_212 = wile_multiply(NULL, var_213);
}
lval var_215;
{
lval var_217[2];
var_217[0] = var_186[2];
var_217[1] = var_186[4];
var_215 = wile_gen_list(2, var_217, NULL);
}
{
lval var_216[8];
var_216[0] = var_215;
var_215 = wile_multiply(NULL, var_216);
}
lval var_218;
{
lval var_220[2];
var_220[0] = var_212;
var_220[1] = var_215;
var_218 = wile_gen_list(2, var_220, NULL);
}
{
lval var_219[8];
var_219[0] = var_218;
var_218 = wile_subtract(NULL, var_219);
}
lval var_222;
if (var_218.vt == LV_INT) {
var_222 = LVI_REAL((lisp_real_t) var_218.v.iv);
} else if (var_218.vt == LV_RAT) {
var_222 = LVI_REAL(LV_RAT2REAL(var_218));
} else {
var_222 = var_218;
}
lval var_221;
if (var_222.vt == LV_REAL) {
if (var_222.v.rv < 0.0) {
var_221 = LVI_CMPLX2(0.0, SQRT(-var_222.v.rv));
} else {
var_221 = LVI_REAL(SQRT(var_222.v.rv));
}
} else if (var_222.vt == LV_CMPLX) {
var_221 = LVI_CMPLX1(CSQRT(var_222.v.cv));
} else {
WILE_EX("sqrt", "expects one numeric argument");
}
lval var_223;
{
lval var_225[2];
var_225[0] = var_209;
var_225[1] = var_221;
var_223 = wile_gen_list(2, var_225, NULL);
}
{
lval var_224[8];
var_224[0] = var_223;
var_223 = wile_divide(NULL, var_224);
}
lval var_226;
{
lval var_228[2];
var_228[0] = var_188;
var_228[1] = var_223;
var_226 = wile_gen_list(2, var_228, NULL);
}
{
lval var_227[8];
var_227[0] = var_226;
var_226 = wile_add(NULL, var_227);
}
var_201 = var_226;
lval var_229;
lval var_230;
{
lval var_231[1];
var_231[0] = var_201;
var_230 = wile_gen_list(1, var_231, NULL);
}
lval var_232;
{
lval var_233[2];
var_233[0] = V_CLOS(var_185,0);
var_233[1] = var_230;
var_232 = wile_gen_list(2, var_233, NULL);
}
var_232 = wile_apply_function(&(var_232), __FILE__, __LINE__);
var_229 = var_232;
lval var_234;
lval var_235;
lval var_236[8];
var_236[0] = var_201;
var_236[1] = var_188;
var_236[2] = var_201;
var_235 = fn_4(NULL, var_236);
var_234 = var_235;
lval var_238;
lval var_239;
var_239 = LVI_INT(1);
lval var_240;
{
lval var_242[2];
var_242[0] = var_186[0];
var_242[1] = var_239;
var_240 = wile_gen_list(2, var_242, NULL);
}
{
lval var_241[8];
var_241[0] = var_240;
var_240 = wile_subtract(NULL, var_241);
}
var_238 = var_240;
lval var_243;
do {
lval var_244;
switch (var_186[0].vt) {
case LV_REAL:
var_244 = LVI_BOOL(var_186[0].v.rv == 0.0);
break;
case LV_RAT:
var_244 = LVI_BOOL((var_186[0].v.irv.num == 0 && var_186[0].v.irv.den != 0));
break;
case LV_INT:
var_244 = LVI_BOOL(var_186[0].v.iv == 0);
break;
case LV_CMPLX:
var_244 = LVI_BOOL(CREAL(var_186[0].v.cv) == 0.0 && CIMAG(var_186[0].v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_244)) {
lval var_247[8];
var_247[0] = var_234;
var_186[0] = var_247[0];
TAIL_CALL fn_2(NULL, var_186);
}
lval var_248;
switch (var_196.vt) {
case LV_REAL:
var_248 = LVI_BOOL(var_196.v.rv == 0.0);
break;
case LV_RAT:
var_248 = LVI_BOOL((var_196.v.irv.num == 0 && var_196.v.irv.den != 0));
break;
case LV_INT:
var_248 = LVI_BOOL(var_196.v.iv == 0);
break;
case LV_CMPLX:
var_248 = LVI_BOOL(CREAL(var_196.v.cv) == 0.0 && CIMAG(var_196.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_248)) {
lval var_251[8];
var_251[0] = var_188;
var_186[0] = var_251[0];
TAIL_CALL fn_3(NULL, var_186);
}
lval var_252;
switch (var_229.vt) {
case LV_REAL:
var_252 = LVI_BOOL(var_229.v.rv == 0.0);
break;
case LV_RAT:
var_252 = LVI_BOOL((var_229.v.irv.num == 0 && var_229.v.irv.den != 0));
break;
case LV_INT:
var_252 = LVI_BOOL(var_229.v.iv == 0);
break;
case LV_CMPLX:
var_252 = LVI_BOOL(CREAL(var_229.v.cv) == 0.0 && CIMAG(var_229.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_252)) {
lval var_255[8];
var_255[0] = var_201;
var_186[0] = var_255[0];
TAIL_CALL fn_3(NULL, var_186);
}
lval var_256;
{
lval var_257[2];
var_257[0] = var_186[1];
var_257[1] = var_186[3];
var_256 = wile_gen_list(2, var_257, NULL);
}
lval var_258;
{
lval var_259[2];
var_259[0] = V_CLOS(var_185,1);
var_259[1] = var_256;
var_258 = wile_gen_list(2, var_259, NULL);
}
var_258 = wile_apply_function(&(var_258), __FILE__, __LINE__);
if (!LV_IS_FALSE(var_258)) {
var_243 = var_234;
break;
}
lval var_260;
switch (var_229.vt) {
case LV_INT:
var_260 = LVI_INT(WILE_SIGN(var_229.v.iv));
break;
case LV_RAT:
var_260 = LVI_INT(WILE_SIGN(var_229.v.irv.num));
if (var_229.v.irv.den < 0) {
var_260.v.iv = -var_260.v.iv;
}
break;
case LV_REAL:
var_260 = LVI_INT(WILE_SIGN(var_229.v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_261;
switch (var_196.vt) {
case LV_INT:
var_261 = LVI_INT(WILE_SIGN(var_196.v.iv));
break;
case LV_RAT:
var_261 = LVI_INT(WILE_SIGN(var_196.v.irv.num));
if (var_196.v.irv.den < 0) {
var_261.v.iv = -var_261.v.iv;
}
break;
case LV_REAL:
var_261 = LVI_INT(WILE_SIGN(var_196.v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_262;
switch (TYPE_COMBO(var_260.vt,var_261.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_262 = LVI_BOOL(var_260.v.iv != var_261.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_262 = LVI_BOOL(var_260.v.iv * var_261.v.irv.den != var_261.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_262 = LVI_BOOL(var_260.v.iv != var_261.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_262 = LVI_BOOL(var_260.v.irv.num != var_261.v.iv * var_260.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_262 = LVI_BOOL(var_260.v.irv.num * var_261.v.irv.den != var_261.v.irv.num * var_260.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_262 = LVI_BOOL(var_260.v.irv.num != var_261.v.rv * var_260.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_262 = LVI_BOOL(var_260.v.rv != var_261.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_262 = LVI_BOOL(var_260.v.rv * var_261.v.irv.den != var_261.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_262 = LVI_BOOL(var_260.v.rv != var_261.v.rv);
break;
default:
WILE_EX("!=", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_262)) {
lval var_263;
{
lval var_264[5];
var_264[0] = var_238;
var_264[1] = var_188;
var_264[2] = var_196;
var_264[3] = var_201;
var_264[4] = var_229;
var_263 = wile_gen_list(5, var_264, NULL);
}
lval var_265;
{
lval var_266[2];
var_266[0] = V_CLOS(var_185,2);
var_266[1] = var_263;
var_265 = wile_gen_list(2, var_266, NULL);
}
var_265 = wile_apply_function(&(var_265), __FILE__, __LINE__);
var_243 = var_265;
break;
}
lval var_267;
switch (var_229.vt) {
case LV_INT:
var_267 = LVI_INT(WILE_SIGN(var_229.v.iv));
break;
case LV_RAT:
var_267 = LVI_INT(WILE_SIGN(var_229.v.irv.num));
if (var_229.v.irv.den < 0) {
var_267.v.iv = -var_267.v.iv;
}
break;
case LV_REAL:
var_267 = LVI_INT(WILE_SIGN(var_229.v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_268;
switch (var_186[2].vt) {
case LV_INT:
var_268 = LVI_INT(WILE_SIGN(var_186[2].v.iv));
break;
case LV_RAT:
var_268 = LVI_INT(WILE_SIGN(var_186[2].v.irv.num));
if (var_186[2].v.irv.den < 0) {
var_268.v.iv = -var_268.v.iv;
}
break;
case LV_REAL:
var_268 = LVI_INT(WILE_SIGN(var_186[2].v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_269;
switch (TYPE_COMBO(var_267.vt,var_268.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_269 = LVI_BOOL(var_267.v.iv != var_268.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_269 = LVI_BOOL(var_267.v.iv * var_268.v.irv.den != var_268.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_269 = LVI_BOOL(var_267.v.iv != var_268.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_269 = LVI_BOOL(var_267.v.irv.num != var_268.v.iv * var_267.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_269 = LVI_BOOL(var_267.v.irv.num * var_268.v.irv.den != var_268.v.irv.num * var_267.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_269 = LVI_BOOL(var_267.v.irv.num != var_268.v.rv * var_267.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_269 = LVI_BOOL(var_267.v.rv != var_268.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_269 = LVI_BOOL(var_267.v.rv * var_268.v.irv.den != var_268.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_269 = LVI_BOOL(var_267.v.rv != var_268.v.rv);
break;
default:
WILE_EX("!=", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_269)) {
lval var_270;
{
lval var_271[5];
var_271[0] = var_238;
var_271[1] = var_186[1];
var_271[2] = var_186[2];
var_271[3] = var_201;
var_271[4] = var_229;
var_270 = wile_gen_list(5, var_271, NULL);
}
lval var_272;
{
lval var_273[2];
var_273[0] = V_CLOS(var_185,2);
var_273[1] = var_270;
var_272 = wile_gen_list(2, var_273, NULL);
}
var_272 = wile_apply_function(&(var_272), __FILE__, __LINE__);
var_243 = var_272;
break;
}
lval var_274;
{
lval var_275[5];
var_275[0] = var_238;
var_275[1] = var_201;
var_275[2] = var_229;
var_275[3] = var_186[3];
var_275[4] = var_186[4];
var_274 = wile_gen_list(5, var_275, NULL);
}
lval var_276;
{
lval var_277[2];
var_277[0] = V_CLOS(var_185,2);
var_277[1] = var_274;
var_276 = wile_gen_list(2, var_277, NULL);
}
var_276 = wile_apply_function(&(var_276), __FILE__, __LINE__);
var_243 = var_276;
} while (0);
return var_243;
}
// end of lambda fn_184

// @@@ (root-ridders et fn a0 b0) @@@ bld-rtl-dir/wile-rtl2-000076.scm:90 @@@ wile_root_ridders @@@
lval wile_root_ridders(lptr* var_168, lptr var_169)
{
lval var_175;
{
lval var_176[1];
var_176[0] = var_169[2];
var_175 = wile_gen_list(1, var_176, NULL);
}
lval var_177;
{
lval var_178[2];
var_178[0] = var_169[1];
var_178[1] = var_175;
var_177 = wile_gen_list(2, var_178, NULL);
}
var_177 = wile_apply_function(&(var_177), __FILE__, __LINE__);
var_171 = var_177;
lval var_179;
{
lval var_180[1];
var_180[0] = var_169[3];
var_179 = wile_gen_list(1, var_180, NULL);
}
lval var_181;
{
lval var_182[2];
var_182[0] = var_169[1];
var_182[1] = var_179;
var_181 = wile_gen_list(2, var_182, NULL);
}
var_181 = wile_apply_function(&(var_181), __FILE__, __LINE__);
var_172 = var_181;
lval var_183;
var_183 = LVI_INT(128);
var_173 = var_183;
MK_CLOS(var_185,3);
lptr var_278 = new_lv(VT_UNINIT);
var_278->v.pair.car = &(var_174);
P_CLOS(var_185,2) = var_278;
lptr var_279 = new_lv(VT_UNINIT);
var_279->v.pair.car = &(var_169[0]);
P_CLOS(var_185,1) = var_279;
lptr var_280 = new_lv(VT_UNINIT);
var_280->v.pair.car = &(var_169[1]);
P_CLOS(var_185,0) = var_280;
var_174 = LVI_PROC(fn_184,var_185,5);
lval var_281;
do {
lval var_282;
switch (var_171.vt) {
case LV_REAL:
var_282 = LVI_BOOL(var_171.v.rv == 0.0);
break;
case LV_RAT:
var_282 = LVI_BOOL((var_171.v.irv.num == 0 && var_171.v.irv.den != 0));
break;
case LV_INT:
var_282 = LVI_BOOL(var_171.v.iv == 0);
break;
case LV_CMPLX:
var_282 = LVI_BOOL(CREAL(var_171.v.cv) == 0.0 && CIMAG(var_171.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_282)) {
lval var_285[8];
var_285[0] = var_169[2];
var_169[0] = var_285[0];
TAIL_CALL fn_3(NULL, var_169);
}
lval var_286;
switch (var_172.vt) {
case LV_REAL:
var_286 = LVI_BOOL(var_172.v.rv == 0.0);
break;
case LV_RAT:
var_286 = LVI_BOOL((var_172.v.irv.num == 0 && var_172.v.irv.den != 0));
break;
case LV_INT:
var_286 = LVI_BOOL(var_172.v.iv == 0);
break;
case LV_CMPLX:
var_286 = LVI_BOOL(CREAL(var_172.v.cv) == 0.0 && CIMAG(var_172.v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_286)) {
lval var_289[8];
var_289[0] = var_169[3];
var_169[0] = var_289[0];
TAIL_CALL fn_3(NULL, var_169);
}
lval var_290;
switch (var_171.vt) {
case LV_INT:
var_290 = LVI_INT(WILE_SIGN(var_171.v.iv));
break;
case LV_RAT:
var_290 = LVI_INT(WILE_SIGN(var_171.v.irv.num));
if (var_171.v.irv.den < 0) {
var_290.v.iv = -var_290.v.iv;
}
break;
case LV_REAL:
var_290 = LVI_INT(WILE_SIGN(var_171.v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_291;
switch (var_172.vt) {
case LV_INT:
var_291 = LVI_INT(WILE_SIGN(var_172.v.iv));
break;
case LV_RAT:
var_291 = LVI_INT(WILE_SIGN(var_172.v.irv.num));
if (var_172.v.irv.den < 0) {
var_291.v.iv = -var_291.v.iv;
}
break;
case LV_REAL:
var_291 = LVI_INT(WILE_SIGN(var_172.v.rv));
break;
default:
WILE_EX("sign", "got a non-real-valued argument");
}
lval var_292;
switch (TYPE_COMBO(var_290.vt,var_291.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_292 = LVI_BOOL(var_290.v.iv == var_291.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_292 = LVI_BOOL(var_290.v.iv * var_291.v.irv.den == var_291.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_292 = LVI_BOOL(var_290.v.iv == var_291.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_292 = LVI_BOOL(var_290.v.irv.num == var_291.v.iv * var_290.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_292 = LVI_BOOL(var_290.v.irv.num * var_291.v.irv.den == var_291.v.irv.num * var_290.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_292 = LVI_BOOL(var_290.v.irv.num == var_291.v.rv * var_290.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_292 = LVI_BOOL(var_290.v.rv == var_291.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_292 = LVI_BOOL(var_290.v.rv * var_291.v.irv.den == var_291.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_292 = LVI_BOOL(var_290.v.rv == var_291.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_292)) {
TAIL_CALL fn_1(NULL, var_169);
}
lval var_296;
{
lval var_297[5];
var_297[0] = var_173;
var_297[1] = var_169[2];
var_297[2] = var_171;
var_297[3] = var_169[3];
var_297[4] = var_172;
var_296 = wile_gen_list(5, var_297, NULL);
}
lval var_298;
{
lval var_299[2];
var_299[0] = var_174;
var_299[1] = var_296;
var_298 = wile_gen_list(2, var_299, NULL);
}
var_298 = wile_apply_function(&(var_298), __FILE__, __LINE__);
var_281 = var_298;
} while (0);
*var_278 = var_174;
*var_280 = var_169[1];
*var_279 = var_169[0];
return var_281;
}
// end of function wile_root_ridders
