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
static lval fn_1(lptr*, lptr, const char*);
static lval fn_2(lptr*, lptr, const char*);
static lval fn_3(lptr*, lptr, const char*);
static lval fn_4(lptr*, lptr, const char*);
static lval fn_37(lptr*, lptr, const char*);
static lval fn_93(lptr*, lptr, const char*);
static lval fn_186(lptr*, lptr, const char*);

// definitions

// @@@ (bracket-error) @@@ bld-rtl-dir/wile-rtl2-000080.scm:13 @@@ fn_1 @@@
static lval fn_1(lptr* var_5, lptr var_6, const char* cloc)
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
cachalot->whence = "bld-rtl-dir/wile-rtl2-000080.scm:14";
longjmp(cachalot->cenv, 1);
return var_9;
}
// end of function fn_1

// @@@ (budget-error val) @@@ bld-rtl-dir/wile-rtl2-000080.scm:16 @@@ fn_2 @@@
static lval fn_2(lptr* var_11, lptr var_12, const char* cloc)
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
cachalot->whence = "bld-rtl-dir/wile-rtl2-000080.scm:17";
longjmp(cachalot->cenv, 1);
return var_17;
}
// end of function fn_2

// @@@ (exact v) @@@ bld-rtl-dir/wile-rtl2-000080.scm:19 @@@ fn_3 @@@
static lval fn_3(lptr* var_19, lptr var_20, const char* cloc)
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

// @@@ (approx v v1 v2) @@@ bld-rtl-dir/wile-rtl2-000080.scm:22 @@@ fn_4 @@@
static lval fn_4(lptr* var_25, lptr var_26, const char* cloc)
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
var_28 = wile_subtract(NULL, var_29, "bld-rtl-dir/wile-rtl2-000080.scm:23");
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
wile_exception("abs", "bld-rtl-dir/wile-rtl2-000080.scm:23", "got a non-numeric argument");
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

// @@@ lambda (s) @@@ bld-rtl-dir/wile-rtl2-000080.scm:28 @@@ fn_37 @@@
static lval fn_37(lptr* var_38, lptr var_39, const char* cloc)
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
var_42 = wile_subtract(NULL, var_43, "bld-rtl-dir/wile-rtl2-000080.scm:29");
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
var_46 = wile_add(NULL, var_47, "bld-rtl-dir/wile-rtl2-000080.scm:30");
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
var_52 = wile_apply_function(&(var_52), "bld-rtl-dir/wile-rtl2-000080.scm:31");
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
var_57 = wile_apply_function(&(var_57), "bld-rtl-dir/wile-rtl2-000080.scm:32");
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
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:33", "got a non-real-valued argument");
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
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:33", "got a non-real-valued argument");
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
wile_exception("!=", "bld-rtl-dir/wile-rtl2-000080.scm:33", "inputs are not real-valued numbers");
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
var_64 = wile_multiply(NULL, var_65, "bld-rtl-dir/wile-rtl2-000080.scm:35");
}
lval var_67;
lval var_69[8];
var_69[0] = var_64;
var_39[0] = var_69[0];
goto lbl_40;
var_59 = var_67;
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

// @@@ (root-bracket f x scale) @@@ bld-rtl-dir/wile-rtl2-000080.scm:27 @@@ wile_root_bracket @@@
lval wile_root_bracket(lptr* var_34, lptr var_35, const char* cloc)
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
var_74 = fn_37(var_38, var_75, "bld-rtl-dir/wile-rtl2-000080.scm:28");
*var_72 = var_35[0];
*var_73 = var_35[1];
return var_74;
}
// end of function wile_root_bracket
lval var_80;
lval var_81;
lval var_82;
lval var_83;

// @@@ lambda (ne a fa b fb) @@@ bld-rtl-dir/wile-rtl2-000080.scm:45 @@@ fn_93 @@@
static lval fn_93(lptr* var_94, lptr var_95, const char* cloc)
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
var_99 = wile_add(NULL, var_100, "bld-rtl-dir/wile-rtl2-000080.scm:46");
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
var_102 = wile_multiply(NULL, var_103, "bld-rtl-dir/wile-rtl2-000080.scm:46");
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
var_108 = wile_apply_function(&(var_108), "bld-rtl-dir/wile-rtl2-000080.scm:47");
var_105 = var_108;
lval var_110;
lval var_111;
lval var_112[8];
var_112[0] = var_97;
var_112[1] = var_95[1];
var_112[2] = var_95[3];
var_111 = fn_4(NULL, var_112, "bld-rtl-dir/wile-rtl2-000080.scm:48");
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
var_116 = wile_subtract(NULL, var_117, "bld-rtl-dir/wile-rtl2-000080.scm:49");
}
var_114 = var_116;
lval var_119;
lval var_121;
switch (var_95[0].vt) {
case LV_REAL:
var_121 = LVI_BOOL(var_95[0].v.rv == 0.0);
break;
case LV_RAT:
var_121 = LVI_BOOL((var_95[0].v.irv.num == 0 && var_95[0].v.irv.den != 0));
break;
case LV_INT:
var_121 = LVI_BOOL(var_95[0].v.iv == 0);
break;
case LV_CMPLX:
var_121 = LVI_BOOL(CREAL(var_95[0].v.cv) == 0.0 && CIMAG(var_95[0].v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000080.scm:50", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_121)) {
lval var_122;
lval var_124[8];
var_124[0] = var_110;
var_95[0] = var_124[0];
TAIL_CALL fn_2(NULL, var_95, "bld-rtl-dir/wile-rtl2-000080.scm:50");
var_119 = var_122;
goto lbl_120;
}
lval var_125;
switch (var_105.vt) {
case LV_REAL:
var_125 = LVI_BOOL(var_105.v.rv == 0.0);
break;
case LV_RAT:
var_125 = LVI_BOOL((var_105.v.irv.num == 0 && var_105.v.irv.den != 0));
break;
case LV_INT:
var_125 = LVI_BOOL(var_105.v.iv == 0);
break;
case LV_CMPLX:
var_125 = LVI_BOOL(CREAL(var_105.v.cv) == 0.0 && CIMAG(var_105.v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000080.scm:51", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_125)) {
lval var_126;
lval var_128[8];
var_128[0] = var_97;
var_95[0] = var_128[0];
TAIL_CALL fn_3(NULL, var_95, "bld-rtl-dir/wile-rtl2-000080.scm:51");
var_119 = var_126;
goto lbl_120;
}
lval var_129;
{
lval var_130[2];
var_130[0] = var_95[1];
var_130[1] = var_95[3];
var_129 = wile_gen_list(2, var_130, NULL);
}
lval var_131;
{
lval var_132[2];
var_132[0] = V_CLOS(var_94,1);
var_132[1] = var_129;
var_131 = wile_gen_list(2, var_132, NULL);
}
var_131 = wile_apply_function(&(var_131), "bld-rtl-dir/wile-rtl2-000080.scm:52");
if (!LV_IS_FALSE(var_131)) {
var_119 = var_110;
goto lbl_120;
}
lval var_133;
switch (var_105.vt) {
case LV_REAL:
var_133 = LVI_BOOL(var_105.v.rv < 0.0);
break;
case LV_RAT:
var_133 = LVI_BOOL((var_105.v.irv.num < 0 && var_105.v.irv.den >= 0) || (var_105.v.irv.num > 0 && var_105.v.irv.den < 0));
break;
case LV_INT:
var_133 = LVI_BOOL(var_105.v.iv < 0);
break;
default:
wile_exception("negative?", "bld-rtl-dir/wile-rtl2-000080.scm:53", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_133)) {
lval var_134;
{
lval var_135[5];
var_135[0] = var_114;
var_135[1] = var_97;
var_135[2] = var_105;
var_135[3] = var_95[3];
var_135[4] = var_95[4];
var_134 = wile_gen_list(5, var_135, NULL);
}
lval var_136;
{
lval var_137[2];
var_137[0] = V_CLOS(var_94,2);
var_137[1] = var_134;
var_136 = wile_gen_list(2, var_137, NULL);
}
var_136 = wile_apply_function(&(var_136), "bld-rtl-dir/wile-rtl2-000080.scm:53");
var_119 = var_136;
goto lbl_120;
}
lval var_138;
{
lval var_139[5];
var_139[0] = var_114;
var_139[1] = var_95[1];
var_139[2] = var_95[2];
var_139[3] = var_97;
var_139[4] = var_105;
var_138 = wile_gen_list(5, var_139, NULL);
}
lval var_140;
{
lval var_141[2];
var_141[0] = V_CLOS(var_94,2);
var_141[1] = var_138;
var_140 = wile_gen_list(2, var_141, NULL);
}
var_140 = wile_apply_function(&(var_140), "bld-rtl-dir/wile-rtl2-000080.scm:54");
var_119 = var_140;
lbl_120:;
return var_119;
}
// end of lambda fn_93

// @@@ (root-bisect et fn a0 b0) @@@ bld-rtl-dir/wile-rtl2-000080.scm:41 @@@ wile_root_bisect @@@
lval wile_root_bisect(lptr* var_77, lptr var_78, const char* cloc)
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
var_86 = wile_apply_function(&(var_86), "bld-rtl-dir/wile-rtl2-000080.scm:42");
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
var_90 = wile_apply_function(&(var_90), "bld-rtl-dir/wile-rtl2-000080.scm:43");
var_81 = var_90;
lval var_92;
var_92 = LVI_INT(128);
var_82 = var_92;
MK_CLOS(var_94,3);
lptr var_142 = new_lv(VT_UNINIT);
var_142->v.pair.car = &(var_83);
P_CLOS(var_94,2) = var_142;
lptr var_143 = new_lv(VT_UNINIT);
var_143->v.pair.car = &(var_78[0]);
P_CLOS(var_94,1) = var_143;
lptr var_144 = new_lv(VT_UNINIT);
var_144->v.pair.car = &(var_78[1]);
P_CLOS(var_94,0) = var_144;
var_83 = LVI_PROC(fn_93,var_94,5);
lval var_145;
lval var_147;
switch (var_80.vt) {
case LV_REAL:
var_147 = LVI_BOOL(var_80.v.rv == 0.0);
break;
case LV_RAT:
var_147 = LVI_BOOL((var_80.v.irv.num == 0 && var_80.v.irv.den != 0));
break;
case LV_INT:
var_147 = LVI_BOOL(var_80.v.iv == 0);
break;
case LV_CMPLX:
var_147 = LVI_BOOL(CREAL(var_80.v.cv) == 0.0 && CIMAG(var_80.v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000080.scm:55", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_147)) {
lval var_148;
lval var_150[8];
var_150[0] = var_78[2];
var_78[0] = var_150[0];
TAIL_CALL fn_3(NULL, var_78, "bld-rtl-dir/wile-rtl2-000080.scm:55");
var_145 = var_148;
goto lbl_146;
}
lval var_151;
switch (var_81.vt) {
case LV_REAL:
var_151 = LVI_BOOL(var_81.v.rv == 0.0);
break;
case LV_RAT:
var_151 = LVI_BOOL((var_81.v.irv.num == 0 && var_81.v.irv.den != 0));
break;
case LV_INT:
var_151 = LVI_BOOL(var_81.v.iv == 0);
break;
case LV_CMPLX:
var_151 = LVI_BOOL(CREAL(var_81.v.cv) == 0.0 && CIMAG(var_81.v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000080.scm:56", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_151)) {
lval var_152;
lval var_154[8];
var_154[0] = var_78[3];
var_78[0] = var_154[0];
TAIL_CALL fn_3(NULL, var_78, "bld-rtl-dir/wile-rtl2-000080.scm:56");
var_145 = var_152;
goto lbl_146;
}
lval var_155;
switch (var_80.vt) {
case LV_INT:
var_155 = LVI_INT(WILE_SIGN(var_80.v.iv));
break;
case LV_RAT:
var_155 = LVI_INT(WILE_SIGN(var_80.v.irv.num));
if (var_80.v.irv.den < 0) {
var_155.v.iv = -var_155.v.iv;
}
break;
case LV_REAL:
var_155 = LVI_INT(WILE_SIGN(var_80.v.rv));
break;
default:
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:57", "got a non-real-valued argument");
}
lval var_156;
switch (var_81.vt) {
case LV_INT:
var_156 = LVI_INT(WILE_SIGN(var_81.v.iv));
break;
case LV_RAT:
var_156 = LVI_INT(WILE_SIGN(var_81.v.irv.num));
if (var_81.v.irv.den < 0) {
var_156.v.iv = -var_156.v.iv;
}
break;
case LV_REAL:
var_156 = LVI_INT(WILE_SIGN(var_81.v.rv));
break;
default:
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:57", "got a non-real-valued argument");
}
lval var_157;
switch (TYPE_COMBO(var_155.vt,var_156.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_157 = LVI_BOOL(var_155.v.iv == var_156.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_157 = LVI_BOOL(var_155.v.iv * var_156.v.irv.den == var_156.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_157 = LVI_BOOL(var_155.v.iv == var_156.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_157 = LVI_BOOL(var_155.v.irv.num == var_156.v.iv * var_155.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_157 = LVI_BOOL(var_155.v.irv.num * var_156.v.irv.den == var_156.v.irv.num * var_155.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_157 = LVI_BOOL(var_155.v.irv.num == var_156.v.rv * var_155.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_157 = LVI_BOOL(var_155.v.rv == var_156.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_157 = LVI_BOOL(var_155.v.rv * var_156.v.irv.den == var_156.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_157 = LVI_BOOL(var_155.v.rv == var_156.v.rv);
break;
default:
wile_exception("==", "bld-rtl-dir/wile-rtl2-000080.scm:57", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_157)) {
lval var_158;
TAIL_CALL fn_1(NULL, var_78, "bld-rtl-dir/wile-rtl2-000080.scm:57");
var_145 = var_158;
goto lbl_146;
}
lval var_161;
switch (var_80.vt) {
case LV_REAL:
var_161 = LVI_BOOL(var_80.v.rv < 0.0);
break;
case LV_RAT:
var_161 = LVI_BOOL((var_80.v.irv.num < 0 && var_80.v.irv.den >= 0) || (var_80.v.irv.num > 0 && var_80.v.irv.den < 0));
break;
case LV_INT:
var_161 = LVI_BOOL(var_80.v.iv < 0);
break;
default:
wile_exception("negative?", "bld-rtl-dir/wile-rtl2-000080.scm:58", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_161)) {
lval var_162;
{
lval var_163[5];
var_163[0] = var_82;
var_163[1] = var_78[2];
var_163[2] = var_80;
var_163[3] = var_78[3];
var_163[4] = var_81;
var_162 = wile_gen_list(5, var_163, NULL);
}
lval var_164;
{
lval var_165[2];
var_165[0] = var_83;
var_165[1] = var_162;
var_164 = wile_gen_list(2, var_165, NULL);
}
var_164 = wile_apply_function(&(var_164), "bld-rtl-dir/wile-rtl2-000080.scm:58");
var_145 = var_164;
goto lbl_146;
}
lval var_166;
{
lval var_167[5];
var_167[0] = var_82;
var_167[1] = var_78[3];
var_167[2] = var_81;
var_167[3] = var_78[2];
var_167[4] = var_80;
var_166 = wile_gen_list(5, var_167, NULL);
}
lval var_168;
{
lval var_169[2];
var_169[0] = var_83;
var_169[1] = var_166;
var_168 = wile_gen_list(2, var_169, NULL);
}
var_168 = wile_apply_function(&(var_168), "bld-rtl-dir/wile-rtl2-000080.scm:59");
var_145 = var_168;
lbl_146:;
*var_142 = var_83;
*var_144 = var_78[1];
*var_143 = var_78[0];
return var_145;
}
// end of function wile_root_bisect
lval var_173;
lval var_174;
lval var_175;
lval var_176;

// @@@ lambda (ne a fa b fb) @@@ bld-rtl-dir/wile-rtl2-000080.scm:94 @@@ fn_186 @@@
static lval fn_186(lptr* var_187, lptr var_188, const char* cloc)
{
lval var_190;
lval var_191;
var_191 = LVI_REAL(5.00000000000000000000000000000000000e-01Q);
lval var_192;
{
lval var_194[2];
var_194[0] = var_188[1];
var_194[1] = var_188[3];
var_192 = wile_gen_list(2, var_194, NULL);
}
{
lval var_193[8];
var_193[0] = var_192;
var_192 = wile_add(NULL, var_193, "bld-rtl-dir/wile-rtl2-000080.scm:95");
}
lval var_195;
{
lval var_197[2];
var_197[0] = var_191;
var_197[1] = var_192;
var_195 = wile_gen_list(2, var_197, NULL);
}
{
lval var_196[8];
var_196[0] = var_195;
var_195 = wile_multiply(NULL, var_196, "bld-rtl-dir/wile-rtl2-000080.scm:95");
}
var_190 = var_195;
lval var_198;
lval var_199;
{
lval var_200[1];
var_200[0] = var_190;
var_199 = wile_gen_list(1, var_200, NULL);
}
lval var_201;
{
lval var_202[2];
var_202[0] = V_CLOS(var_187,0);
var_202[1] = var_199;
var_201 = wile_gen_list(2, var_202, NULL);
}
var_201 = wile_apply_function(&(var_201), "bld-rtl-dir/wile-rtl2-000080.scm:96");
var_198 = var_201;
lval var_203;
lval var_204;
{
lval var_206[2];
var_206[0] = var_190;
var_206[1] = var_188[1];
var_204 = wile_gen_list(2, var_206, NULL);
}
{
lval var_205[8];
var_205[0] = var_204;
var_204 = wile_subtract(NULL, var_205, "bld-rtl-dir/wile-rtl2-000080.scm:97");
}
lval var_207;
{
lval var_209[2];
var_209[0] = var_188[2];
var_209[1] = var_188[4];
var_207 = wile_gen_list(2, var_209, NULL);
}
{
lval var_208[8];
var_208[0] = var_207;
var_207 = wile_subtract(NULL, var_208, "bld-rtl-dir/wile-rtl2-000080.scm:97");
}
lval var_210;
switch (var_207.vt) {
case LV_INT:
var_210 = LVI_INT(WILE_SIGN(var_207.v.iv));
break;
case LV_RAT:
var_210 = LVI_INT(WILE_SIGN(var_207.v.irv.num));
if (var_207.v.irv.den < 0) {
var_210.v.iv = -var_210.v.iv;
}
break;
case LV_REAL:
var_210 = LVI_INT(WILE_SIGN(var_207.v.rv));
break;
default:
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:97", "got a non-real-valued argument");
}
lval var_211;
{
lval var_213[3];
var_213[0] = var_204;
var_213[1] = var_198;
var_213[2] = var_210;
var_211 = wile_gen_list(3, var_213, NULL);
}
{
lval var_212[8];
var_212[0] = var_211;
var_211 = wile_multiply(NULL, var_212, "bld-rtl-dir/wile-rtl2-000080.scm:97");
}
lval var_214;
{
lval var_216[2];
var_216[0] = var_198;
var_216[1] = var_198;
var_214 = wile_gen_list(2, var_216, NULL);
}
{
lval var_215[8];
var_215[0] = var_214;
var_214 = wile_multiply(NULL, var_215, "bld-rtl-dir/wile-rtl2-000080.scm:98");
}
lval var_217;
{
lval var_219[2];
var_219[0] = var_188[2];
var_219[1] = var_188[4];
var_217 = wile_gen_list(2, var_219, NULL);
}
{
lval var_218[8];
var_218[0] = var_217;
var_217 = wile_multiply(NULL, var_218, "bld-rtl-dir/wile-rtl2-000080.scm:98");
}
lval var_220;
{
lval var_222[2];
var_222[0] = var_214;
var_222[1] = var_217;
var_220 = wile_gen_list(2, var_222, NULL);
}
{
lval var_221[8];
var_221[0] = var_220;
var_220 = wile_subtract(NULL, var_221, "bld-rtl-dir/wile-rtl2-000080.scm:98");
}
lval var_224;
if (var_220.vt == LV_INT) {
var_224 = LVI_REAL((lisp_real_t) var_220.v.iv);
} else if (var_220.vt == LV_RAT) {
var_224 = LVI_REAL(LV_RAT2REAL(var_220));
} else {
var_224 = var_220;
}
lval var_223;
if (var_224.vt == LV_REAL) {
if (var_224.v.rv < 0.0) {
var_223 = LVI_CMPLX2(0.0, SQRT(-var_224.v.rv));
} else {
var_223 = LVI_REAL(SQRT(var_224.v.rv));
}
} else if (var_224.vt == LV_CMPLX) {
var_223 = LVI_CMPLX1(CSQRT(var_224.v.cv));
} else {
wile_exception("sqrt", "bld-rtl-dir/wile-rtl2-000080.scm:98", "expects one numeric argument");
}
lval var_225;
{
lval var_227[2];
var_227[0] = var_211;
var_227[1] = var_223;
var_225 = wile_gen_list(2, var_227, NULL);
}
{
lval var_226[8];
var_226[0] = var_225;
var_225 = wile_divide(NULL, var_226, "bld-rtl-dir/wile-rtl2-000080.scm:97");
}
lval var_228;
{
lval var_230[2];
var_230[0] = var_190;
var_230[1] = var_225;
var_228 = wile_gen_list(2, var_230, NULL);
}
{
lval var_229[8];
var_229[0] = var_228;
var_228 = wile_add(NULL, var_229, "bld-rtl-dir/wile-rtl2-000080.scm:97");
}
var_203 = var_228;
lval var_231;
lval var_232;
{
lval var_233[1];
var_233[0] = var_203;
var_232 = wile_gen_list(1, var_233, NULL);
}
lval var_234;
{
lval var_235[2];
var_235[0] = V_CLOS(var_187,0);
var_235[1] = var_232;
var_234 = wile_gen_list(2, var_235, NULL);
}
var_234 = wile_apply_function(&(var_234), "bld-rtl-dir/wile-rtl2-000080.scm:99");
var_231 = var_234;
lval var_236;
lval var_237;
lval var_238[8];
var_238[0] = var_203;
var_238[1] = var_190;
var_238[2] = var_203;
var_237 = fn_4(NULL, var_238, "bld-rtl-dir/wile-rtl2-000080.scm:100");
var_236 = var_237;
lval var_240;
lval var_241;
var_241 = LVI_INT(1);
lval var_242;
{
lval var_244[2];
var_244[0] = var_188[0];
var_244[1] = var_241;
var_242 = wile_gen_list(2, var_244, NULL);
}
{
lval var_243[8];
var_243[0] = var_242;
var_242 = wile_subtract(NULL, var_243, "bld-rtl-dir/wile-rtl2-000080.scm:101");
}
var_240 = var_242;
lval var_245;
lval var_247;
switch (var_188[0].vt) {
case LV_REAL:
var_247 = LVI_BOOL(var_188[0].v.rv == 0.0);
break;
case LV_RAT:
var_247 = LVI_BOOL((var_188[0].v.irv.num == 0 && var_188[0].v.irv.den != 0));
break;
case LV_INT:
var_247 = LVI_BOOL(var_188[0].v.iv == 0);
break;
case LV_CMPLX:
var_247 = LVI_BOOL(CREAL(var_188[0].v.cv) == 0.0 && CIMAG(var_188[0].v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000080.scm:102", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_247)) {
lval var_248;
lval var_250[8];
var_250[0] = var_236;
var_188[0] = var_250[0];
TAIL_CALL fn_2(NULL, var_188, "bld-rtl-dir/wile-rtl2-000080.scm:102");
var_245 = var_248;
goto lbl_246;
}
lval var_251;
switch (var_198.vt) {
case LV_REAL:
var_251 = LVI_BOOL(var_198.v.rv == 0.0);
break;
case LV_RAT:
var_251 = LVI_BOOL((var_198.v.irv.num == 0 && var_198.v.irv.den != 0));
break;
case LV_INT:
var_251 = LVI_BOOL(var_198.v.iv == 0);
break;
case LV_CMPLX:
var_251 = LVI_BOOL(CREAL(var_198.v.cv) == 0.0 && CIMAG(var_198.v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000080.scm:103", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_251)) {
lval var_252;
lval var_254[8];
var_254[0] = var_190;
var_188[0] = var_254[0];
TAIL_CALL fn_3(NULL, var_188, "bld-rtl-dir/wile-rtl2-000080.scm:103");
var_245 = var_252;
goto lbl_246;
}
lval var_255;
switch (var_231.vt) {
case LV_REAL:
var_255 = LVI_BOOL(var_231.v.rv == 0.0);
break;
case LV_RAT:
var_255 = LVI_BOOL((var_231.v.irv.num == 0 && var_231.v.irv.den != 0));
break;
case LV_INT:
var_255 = LVI_BOOL(var_231.v.iv == 0);
break;
case LV_CMPLX:
var_255 = LVI_BOOL(CREAL(var_231.v.cv) == 0.0 && CIMAG(var_231.v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000080.scm:104", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_255)) {
lval var_256;
lval var_258[8];
var_258[0] = var_203;
var_188[0] = var_258[0];
TAIL_CALL fn_3(NULL, var_188, "bld-rtl-dir/wile-rtl2-000080.scm:104");
var_245 = var_256;
goto lbl_246;
}
lval var_259;
{
lval var_260[2];
var_260[0] = var_188[1];
var_260[1] = var_188[3];
var_259 = wile_gen_list(2, var_260, NULL);
}
lval var_261;
{
lval var_262[2];
var_262[0] = V_CLOS(var_187,1);
var_262[1] = var_259;
var_261 = wile_gen_list(2, var_262, NULL);
}
var_261 = wile_apply_function(&(var_261), "bld-rtl-dir/wile-rtl2-000080.scm:105");
if (!LV_IS_FALSE(var_261)) {
var_245 = var_236;
goto lbl_246;
}
lval var_263;
switch (var_231.vt) {
case LV_INT:
var_263 = LVI_INT(WILE_SIGN(var_231.v.iv));
break;
case LV_RAT:
var_263 = LVI_INT(WILE_SIGN(var_231.v.irv.num));
if (var_231.v.irv.den < 0) {
var_263.v.iv = -var_263.v.iv;
}
break;
case LV_REAL:
var_263 = LVI_INT(WILE_SIGN(var_231.v.rv));
break;
default:
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:106", "got a non-real-valued argument");
}
lval var_264;
switch (var_198.vt) {
case LV_INT:
var_264 = LVI_INT(WILE_SIGN(var_198.v.iv));
break;
case LV_RAT:
var_264 = LVI_INT(WILE_SIGN(var_198.v.irv.num));
if (var_198.v.irv.den < 0) {
var_264.v.iv = -var_264.v.iv;
}
break;
case LV_REAL:
var_264 = LVI_INT(WILE_SIGN(var_198.v.rv));
break;
default:
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:106", "got a non-real-valued argument");
}
lval var_265;
switch (TYPE_COMBO(var_263.vt,var_264.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_265 = LVI_BOOL(var_263.v.iv != var_264.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_265 = LVI_BOOL(var_263.v.iv * var_264.v.irv.den != var_264.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_265 = LVI_BOOL(var_263.v.iv != var_264.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_265 = LVI_BOOL(var_263.v.irv.num != var_264.v.iv * var_263.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_265 = LVI_BOOL(var_263.v.irv.num * var_264.v.irv.den != var_264.v.irv.num * var_263.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_265 = LVI_BOOL(var_263.v.irv.num != var_264.v.rv * var_263.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_265 = LVI_BOOL(var_263.v.rv != var_264.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_265 = LVI_BOOL(var_263.v.rv * var_264.v.irv.den != var_264.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_265 = LVI_BOOL(var_263.v.rv != var_264.v.rv);
break;
default:
wile_exception("!=", "bld-rtl-dir/wile-rtl2-000080.scm:106", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_265)) {
lval var_266;
{
lval var_267[5];
var_267[0] = var_240;
var_267[1] = var_190;
var_267[2] = var_198;
var_267[3] = var_203;
var_267[4] = var_231;
var_266 = wile_gen_list(5, var_267, NULL);
}
lval var_268;
{
lval var_269[2];
var_269[0] = V_CLOS(var_187,2);
var_269[1] = var_266;
var_268 = wile_gen_list(2, var_269, NULL);
}
var_268 = wile_apply_function(&(var_268), "bld-rtl-dir/wile-rtl2-000080.scm:106");
var_245 = var_268;
goto lbl_246;
}
lval var_270;
switch (var_231.vt) {
case LV_INT:
var_270 = LVI_INT(WILE_SIGN(var_231.v.iv));
break;
case LV_RAT:
var_270 = LVI_INT(WILE_SIGN(var_231.v.irv.num));
if (var_231.v.irv.den < 0) {
var_270.v.iv = -var_270.v.iv;
}
break;
case LV_REAL:
var_270 = LVI_INT(WILE_SIGN(var_231.v.rv));
break;
default:
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:107", "got a non-real-valued argument");
}
lval var_271;
switch (var_188[2].vt) {
case LV_INT:
var_271 = LVI_INT(WILE_SIGN(var_188[2].v.iv));
break;
case LV_RAT:
var_271 = LVI_INT(WILE_SIGN(var_188[2].v.irv.num));
if (var_188[2].v.irv.den < 0) {
var_271.v.iv = -var_271.v.iv;
}
break;
case LV_REAL:
var_271 = LVI_INT(WILE_SIGN(var_188[2].v.rv));
break;
default:
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:107", "got a non-real-valued argument");
}
lval var_272;
switch (TYPE_COMBO(var_270.vt,var_271.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_272 = LVI_BOOL(var_270.v.iv != var_271.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_272 = LVI_BOOL(var_270.v.iv * var_271.v.irv.den != var_271.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_272 = LVI_BOOL(var_270.v.iv != var_271.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_272 = LVI_BOOL(var_270.v.irv.num != var_271.v.iv * var_270.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_272 = LVI_BOOL(var_270.v.irv.num * var_271.v.irv.den != var_271.v.irv.num * var_270.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_272 = LVI_BOOL(var_270.v.irv.num != var_271.v.rv * var_270.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_272 = LVI_BOOL(var_270.v.rv != var_271.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_272 = LVI_BOOL(var_270.v.rv * var_271.v.irv.den != var_271.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_272 = LVI_BOOL(var_270.v.rv != var_271.v.rv);
break;
default:
wile_exception("!=", "bld-rtl-dir/wile-rtl2-000080.scm:107", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_272)) {
lval var_273;
{
lval var_274[5];
var_274[0] = var_240;
var_274[1] = var_188[1];
var_274[2] = var_188[2];
var_274[3] = var_203;
var_274[4] = var_231;
var_273 = wile_gen_list(5, var_274, NULL);
}
lval var_275;
{
lval var_276[2];
var_276[0] = V_CLOS(var_187,2);
var_276[1] = var_273;
var_275 = wile_gen_list(2, var_276, NULL);
}
var_275 = wile_apply_function(&(var_275), "bld-rtl-dir/wile-rtl2-000080.scm:107");
var_245 = var_275;
goto lbl_246;
}
lval var_277;
{
lval var_278[5];
var_278[0] = var_240;
var_278[1] = var_203;
var_278[2] = var_231;
var_278[3] = var_188[3];
var_278[4] = var_188[4];
var_277 = wile_gen_list(5, var_278, NULL);
}
lval var_279;
{
lval var_280[2];
var_280[0] = V_CLOS(var_187,2);
var_280[1] = var_277;
var_279 = wile_gen_list(2, var_280, NULL);
}
var_279 = wile_apply_function(&(var_279), "bld-rtl-dir/wile-rtl2-000080.scm:108");
var_245 = var_279;
lbl_246:;
return var_245;
}
// end of lambda fn_186

// @@@ (root-ridders et fn a0 b0) @@@ bld-rtl-dir/wile-rtl2-000080.scm:90 @@@ wile_root_ridders @@@
lval wile_root_ridders(lptr* var_170, lptr var_171, const char* cloc)
{
lval var_177;
{
lval var_178[1];
var_178[0] = var_171[2];
var_177 = wile_gen_list(1, var_178, NULL);
}
lval var_179;
{
lval var_180[2];
var_180[0] = var_171[1];
var_180[1] = var_177;
var_179 = wile_gen_list(2, var_180, NULL);
}
var_179 = wile_apply_function(&(var_179), "bld-rtl-dir/wile-rtl2-000080.scm:91");
var_173 = var_179;
lval var_181;
{
lval var_182[1];
var_182[0] = var_171[3];
var_181 = wile_gen_list(1, var_182, NULL);
}
lval var_183;
{
lval var_184[2];
var_184[0] = var_171[1];
var_184[1] = var_181;
var_183 = wile_gen_list(2, var_184, NULL);
}
var_183 = wile_apply_function(&(var_183), "bld-rtl-dir/wile-rtl2-000080.scm:92");
var_174 = var_183;
lval var_185;
var_185 = LVI_INT(128);
var_175 = var_185;
MK_CLOS(var_187,3);
lptr var_281 = new_lv(VT_UNINIT);
var_281->v.pair.car = &(var_176);
P_CLOS(var_187,2) = var_281;
lptr var_282 = new_lv(VT_UNINIT);
var_282->v.pair.car = &(var_171[0]);
P_CLOS(var_187,1) = var_282;
lptr var_283 = new_lv(VT_UNINIT);
var_283->v.pair.car = &(var_171[1]);
P_CLOS(var_187,0) = var_283;
var_176 = LVI_PROC(fn_186,var_187,5);
lval var_284;
lval var_286;
switch (var_173.vt) {
case LV_REAL:
var_286 = LVI_BOOL(var_173.v.rv == 0.0);
break;
case LV_RAT:
var_286 = LVI_BOOL((var_173.v.irv.num == 0 && var_173.v.irv.den != 0));
break;
case LV_INT:
var_286 = LVI_BOOL(var_173.v.iv == 0);
break;
case LV_CMPLX:
var_286 = LVI_BOOL(CREAL(var_173.v.cv) == 0.0 && CIMAG(var_173.v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000080.scm:109", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_286)) {
lval var_287;
lval var_289[8];
var_289[0] = var_171[2];
var_171[0] = var_289[0];
TAIL_CALL fn_3(NULL, var_171, "bld-rtl-dir/wile-rtl2-000080.scm:109");
var_284 = var_287;
goto lbl_285;
}
lval var_290;
switch (var_174.vt) {
case LV_REAL:
var_290 = LVI_BOOL(var_174.v.rv == 0.0);
break;
case LV_RAT:
var_290 = LVI_BOOL((var_174.v.irv.num == 0 && var_174.v.irv.den != 0));
break;
case LV_INT:
var_290 = LVI_BOOL(var_174.v.iv == 0);
break;
case LV_CMPLX:
var_290 = LVI_BOOL(CREAL(var_174.v.cv) == 0.0 && CIMAG(var_174.v.cv) == 0.0);
break;
default:
wile_exception("zero?", "bld-rtl-dir/wile-rtl2-000080.scm:110", "expects a real-valued number");
}
if (!LV_IS_FALSE(var_290)) {
lval var_291;
lval var_293[8];
var_293[0] = var_171[3];
var_171[0] = var_293[0];
TAIL_CALL fn_3(NULL, var_171, "bld-rtl-dir/wile-rtl2-000080.scm:110");
var_284 = var_291;
goto lbl_285;
}
lval var_294;
switch (var_173.vt) {
case LV_INT:
var_294 = LVI_INT(WILE_SIGN(var_173.v.iv));
break;
case LV_RAT:
var_294 = LVI_INT(WILE_SIGN(var_173.v.irv.num));
if (var_173.v.irv.den < 0) {
var_294.v.iv = -var_294.v.iv;
}
break;
case LV_REAL:
var_294 = LVI_INT(WILE_SIGN(var_173.v.rv));
break;
default:
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:111", "got a non-real-valued argument");
}
lval var_295;
switch (var_174.vt) {
case LV_INT:
var_295 = LVI_INT(WILE_SIGN(var_174.v.iv));
break;
case LV_RAT:
var_295 = LVI_INT(WILE_SIGN(var_174.v.irv.num));
if (var_174.v.irv.den < 0) {
var_295.v.iv = -var_295.v.iv;
}
break;
case LV_REAL:
var_295 = LVI_INT(WILE_SIGN(var_174.v.rv));
break;
default:
wile_exception("sign", "bld-rtl-dir/wile-rtl2-000080.scm:111", "got a non-real-valued argument");
}
lval var_296;
switch (TYPE_COMBO(var_294.vt,var_295.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_296 = LVI_BOOL(var_294.v.iv == var_295.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_296 = LVI_BOOL(var_294.v.iv * var_295.v.irv.den == var_295.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_296 = LVI_BOOL(var_294.v.iv == var_295.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_296 = LVI_BOOL(var_294.v.irv.num == var_295.v.iv * var_294.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_296 = LVI_BOOL(var_294.v.irv.num * var_295.v.irv.den == var_295.v.irv.num * var_294.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_296 = LVI_BOOL(var_294.v.irv.num == var_295.v.rv * var_294.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_296 = LVI_BOOL(var_294.v.rv == var_295.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_296 = LVI_BOOL(var_294.v.rv * var_295.v.irv.den == var_295.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_296 = LVI_BOOL(var_294.v.rv == var_295.v.rv);
break;
default:
wile_exception("==", "bld-rtl-dir/wile-rtl2-000080.scm:111", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_296)) {
lval var_297;
TAIL_CALL fn_1(NULL, var_171, "bld-rtl-dir/wile-rtl2-000080.scm:111");
var_284 = var_297;
goto lbl_285;
}
lval var_300;
{
lval var_301[5];
var_301[0] = var_175;
var_301[1] = var_171[2];
var_301[2] = var_173;
var_301[3] = var_171[3];
var_301[4] = var_174;
var_300 = wile_gen_list(5, var_301, NULL);
}
lval var_302;
{
lval var_303[2];
var_303[0] = var_176;
var_303[1] = var_300;
var_302 = wile_gen_list(2, var_303, NULL);
}
var_302 = wile_apply_function(&(var_302), "bld-rtl-dir/wile-rtl2-000080.scm:112");
var_284 = var_302;
lbl_285:;
*var_281 = var_176;
*var_283 = var_171[1];
*var_282 = var_171[0];
return var_284;
}
// end of function wile_root_ridders
