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
static lval fn_34(lptr*, lptr, const char*);
static lval fn_67(lptr*, lptr, const char*);
static lval fn_110(lptr*, lptr, const char*);

// definitions
lval var_23;
lval var_24;
lval var_25;
lval var_26;
lval var_27;
lval var_28;
lval var_29;

// @@@ lambda (d s) @@@ bld-rtl-dir/wile-rtl2-000082.scm:23 @@@ fn_34 @@@
static lval fn_34(lptr* var_35, lptr var_36, const char* cloc)
{
lval var_38;
lval var_39;
var_39 = LVI_BOOL((var_36[0].v.iv)%2 == 0);
if (LV_IS_FALSE(var_39)) {
lval var_40;
{
lval var_41[2];
var_41[0] = var_36[0];
var_41[1] = var_36[1];
var_40 = wile_gen_list(2, var_41, NULL);
}
var_38 = var_40;
} else {
lval var_42;
var_42 = LVI_INT(2);
lval var_43;
{
lval var_45[2];
var_45[0] = var_36[0];
var_45[1] = var_42;
var_43 = wile_gen_list(2, var_45, NULL);
}
{
lval var_44[8];
var_44[0] = var_43;
// bld-rtl-dir/wile-rtl2-000082.scm:25
var_43 = wile_divide(NULL, var_44, "bld-rtl-dir/wile-rtl2-000082.scm:25");
}
lval var_46;
var_46 = LVI_INT(1);
lval var_47;
{
lval var_49[2];
var_49[0] = var_36[1];
var_49[1] = var_46;
var_47 = wile_gen_list(2, var_49, NULL);
}
{
lval var_48[8];
var_48[0] = var_47;
// bld-rtl-dir/wile-rtl2-000082.scm:25
var_47 = wile_add(NULL, var_48, "bld-rtl-dir/wile-rtl2-000082.scm:25");
}
lval var_50;
{
lval var_51[2];
var_51[0] = var_43;
var_51[1] = var_47;
var_50 = wile_gen_list(2, var_51, NULL);
}
lval var_52;
{
lval var_53[2];
var_53[0] = V_CLOS(var_35,0);
var_53[1] = var_50;
var_52 = wile_gen_list(2, var_53, NULL);
}
var_52 = wile_apply_function(&(var_52), LISP_WHENCE);
var_38 = var_52;
}
return var_38;
}
// end of lambda fn_34

// @@@ lambda (kk) @@@ bld-rtl-dir/wile-rtl2-000082.scm:30 @@@ fn_67 @@@
static lval fn_67(lptr* var_68, lptr var_69, const char* cloc)
{
lval var_71;
lval var_72;
switch (var_69[0].vt) {
case LV_REAL:
var_72 = LVI_BOOL(var_69[0].v.rv == 0.0);
break;
case LV_RAT:
var_72 = LVI_BOOL((var_69[0].v.irv.num == 0 && var_69[0].v.irv.den != 0));
break;
case LV_INT:
var_72 = LVI_BOOL(var_69[0].v.iv == 0);
break;
case LV_CMPLX:
var_72 = LVI_BOOL(CREAL(var_69[0].v.cv) == 0.0 && CIMAG(var_69[0].v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (LV_IS_FALSE(var_72)) {
lval var_73;
lval var_74;
var_74 = LVI_INT(2);
lval var_75;
var_75 = LVI_INT(2);
lval var_76;
{
lval var_78[2];
var_78[0] = V_CLOS(var_68,0);
var_78[1] = var_75;
var_76 = wile_gen_list(2, var_78, NULL);
}
{
lval var_77[8];
var_77[0] = var_76;
// bld-rtl-dir/wile-rtl2-000082.scm:33
var_76 = wile_subtract(NULL, var_77, "bld-rtl-dir/wile-rtl2-000082.scm:33");
}
lval var_80;
if (var_74.vt == LV_INT) {
var_80 = LVI_REAL((lisp_real_t) var_74.v.iv);
} else if (var_74.vt == LV_RAT) {
var_80 = LVI_REAL(LV_RAT2REAL(var_74));
} else if (var_74.vt == LV_REAL) {
var_80 = var_74;
} else {
WILE_EX("random-uniform", "expects a real-valued input");
}
lval var_81;
if (var_76.vt == LV_INT) {
var_81 = LVI_REAL((lisp_real_t) var_76.v.iv);
} else if (var_76.vt == LV_RAT) {
var_81 = LVI_REAL(LV_RAT2REAL(var_76));
} else if (var_76.vt == LV_REAL) {
var_81 = var_76;
} else {
WILE_EX("random-uniform", "expects a real-valued input");
}
lval var_79;
var_79 = LVI_REAL(var_80.v.rv + (var_81.v.rv - var_80.v.rv)*drand48());
lval var_82;
if (var_79.vt == LV_INT) {
var_82 = var_79;
} else if (var_79.vt == LV_RAT) {
var_82 = LVI_INT(var_79.v.irv.num/var_79.v.irv.den);
} else if (var_79.vt == LV_REAL) {
var_82 = LVI_INT((var_79.v.rv >= 0.0) ? FLOOR(var_79.v.rv) : CEIL(var_79.v.rv));
} else {
WILE_EX("integer", "expects one real-valued argument");
}
var_73 = var_82;
lval var_83;
lval var_84;
{
lval var_85[8];
var_85[0] = var_73;
var_85[1] = V_CLOS(var_68,1);
var_85[2] = V_CLOS(var_68,0);
// bld-rtl-dir/wile-rtl2-000082.scm:34
var_84 = wile_expmod(NULL, var_85, "bld-rtl-dir/wile-rtl2-000082.scm:34");
}
var_83 = var_84;
lval var_86;
lval var_87;
var_87 = LVI_BOOL(false);
do {
lval var_88;
var_88 = LVI_INT(1);
lval var_89;
switch (TYPE_COMBO(var_83.vt,var_88.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_89 = LVI_BOOL(var_83.v.iv == var_88.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_89 = LVI_BOOL(var_83.v.iv * var_88.v.irv.den == var_88.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_89 = LVI_BOOL(var_83.v.iv == var_88.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_89 = LVI_BOOL(var_83.v.irv.num == var_88.v.iv * var_83.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_89 = LVI_BOOL(var_83.v.irv.num * var_88.v.irv.den == var_88.v.irv.num * var_83.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_89 = LVI_BOOL(var_83.v.irv.num == var_88.v.rv * var_83.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_89 = LVI_BOOL(var_83.v.rv == var_88.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_89 = LVI_BOOL(var_83.v.rv * var_88.v.irv.den == var_88.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_89 = LVI_BOOL(var_83.v.rv == var_88.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
var_87 = var_89;
if (!LV_IS_FALSE(var_87)) { break; }
lval var_90;
switch (TYPE_COMBO(var_83.vt,V_CLOS(var_68,2).vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_90 = LVI_BOOL(var_83.v.iv == V_CLOS(var_68,2).v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_90 = LVI_BOOL(var_83.v.iv * V_CLOS(var_68,2).v.irv.den == V_CLOS(var_68,2).v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_90 = LVI_BOOL(var_83.v.iv == V_CLOS(var_68,2).v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_90 = LVI_BOOL(var_83.v.irv.num == V_CLOS(var_68,2).v.iv * var_83.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_90 = LVI_BOOL(var_83.v.irv.num * V_CLOS(var_68,2).v.irv.den == V_CLOS(var_68,2).v.irv.num * var_83.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_90 = LVI_BOOL(var_83.v.irv.num == V_CLOS(var_68,2).v.rv * var_83.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_90 = LVI_BOOL(var_83.v.rv == V_CLOS(var_68,2).v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_90 = LVI_BOOL(var_83.v.rv * V_CLOS(var_68,2).v.irv.den == V_CLOS(var_68,2).v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_90 = LVI_BOOL(var_83.v.rv == V_CLOS(var_68,2).v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
var_87 = var_90;
if (!LV_IS_FALSE(var_87)) { break; }
} while (0);
if (LV_IS_FALSE(var_87)) {
lval var_91;
{
lval var_92[3];
var_92[0] = V_CLOS(var_68,4);
var_92[1] = var_83;
var_92[2] = var_69[0];
var_91 = wile_gen_list(3, var_92, NULL);
}
lval var_93;
{
lval var_94[2];
var_94[0] = V_CLOS(var_68,3);
var_94[1] = var_91;
var_93 = wile_gen_list(2, var_94, NULL);
}
var_93 = wile_apply_function(&(var_93), LISP_WHENCE);
var_86 = var_93;
} else {
lval var_95;
var_95 = LVI_INT(1);
lval var_96;
{
lval var_98[2];
var_98[0] = var_69[0];
var_98[1] = var_95;
var_96 = wile_gen_list(2, var_98, NULL);
}
{
lval var_97[8];
var_97[0] = var_96;
// bld-rtl-dir/wile-rtl2-000082.scm:37
var_96 = wile_subtract(NULL, var_97, "bld-rtl-dir/wile-rtl2-000082.scm:37");
}
lval var_99;
{
lval var_100[1];
var_100[0] = var_96;
var_99 = wile_gen_list(1, var_100, NULL);
}
lval var_101;
{
lval var_102[2];
var_102[0] = V_CLOS(var_68,5);
var_102[1] = var_99;
var_101 = wile_gen_list(2, var_102, NULL);
}
var_101 = wile_apply_function(&(var_101), LISP_WHENCE);
var_86 = var_101;
}
var_71 = var_86;
} else {
lval var_103;
var_103 = LVI_BOOL(true);
var_71 = var_103;
}
return var_71;
}
// end of lambda fn_67

// @@@ lambda (ss x kk) @@@ bld-rtl-dir/wile-rtl2-000082.scm:39 @@@ fn_110 @@@
static lval fn_110(lptr* var_111, lptr var_112, const char* cloc)
{
lval var_114;
lval var_115;
switch (var_112[0].vt) {
case LV_REAL:
var_115 = LVI_BOOL(var_112[0].v.rv == 0.0);
break;
case LV_RAT:
var_115 = LVI_BOOL((var_112[0].v.irv.num == 0 && var_112[0].v.irv.den != 0));
break;
case LV_INT:
var_115 = LVI_BOOL(var_112[0].v.iv == 0);
break;
case LV_CMPLX:
var_115 = LVI_BOOL(CREAL(var_112[0].v.cv) == 0.0 && CIMAG(var_112[0].v.cv) == 0.0);
break;
default:
WILE_EX("zero?", "expects a real-valued number");
}
if (LV_IS_FALSE(var_115)) {
lval var_116;
lval var_117;
var_117 = LVI_INT(2);
lval var_118;
{
lval var_119[8];
var_119[0] = var_112[1];
var_119[1] = var_117;
var_119[2] = V_CLOS(var_111,0);
// bld-rtl-dir/wile-rtl2-000082.scm:42
var_118 = wile_expmod(NULL, var_119, "bld-rtl-dir/wile-rtl2-000082.scm:42");
}
var_116 = var_118;
lval var_120;
do {
lval var_121;
var_121 = LVI_INT(1);
lval var_122;
switch (TYPE_COMBO(var_116.vt,var_121.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_122 = LVI_BOOL(var_116.v.iv == var_121.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_122 = LVI_BOOL(var_116.v.iv * var_121.v.irv.den == var_121.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_122 = LVI_BOOL(var_116.v.iv == var_121.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_122 = LVI_BOOL(var_116.v.irv.num == var_121.v.iv * var_116.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_122 = LVI_BOOL(var_116.v.irv.num * var_121.v.irv.den == var_121.v.irv.num * var_116.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_122 = LVI_BOOL(var_116.v.irv.num == var_121.v.rv * var_116.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_122 = LVI_BOOL(var_116.v.rv == var_121.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_122 = LVI_BOOL(var_116.v.rv * var_121.v.irv.den == var_121.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_122 = LVI_BOOL(var_116.v.rv == var_121.v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_122)) {
lval var_123;
var_123 = LVI_BOOL(false);
var_120 = var_123;
break;
}
lval var_124;
switch (TYPE_COMBO(var_116.vt,V_CLOS(var_111,1).vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_124 = LVI_BOOL(var_116.v.iv == V_CLOS(var_111,1).v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_124 = LVI_BOOL(var_116.v.iv * V_CLOS(var_111,1).v.irv.den == V_CLOS(var_111,1).v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_124 = LVI_BOOL(var_116.v.iv == V_CLOS(var_111,1).v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_124 = LVI_BOOL(var_116.v.irv.num == V_CLOS(var_111,1).v.iv * var_116.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_124 = LVI_BOOL(var_116.v.irv.num * V_CLOS(var_111,1).v.irv.den == V_CLOS(var_111,1).v.irv.num * var_116.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_124 = LVI_BOOL(var_116.v.irv.num == V_CLOS(var_111,1).v.rv * var_116.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_124 = LVI_BOOL(var_116.v.rv == V_CLOS(var_111,1).v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_124 = LVI_BOOL(var_116.v.rv * V_CLOS(var_111,1).v.irv.den == V_CLOS(var_111,1).v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_124 = LVI_BOOL(var_116.v.rv == V_CLOS(var_111,1).v.rv);
break;
default:
WILE_EX("==", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_124)) {
lval var_125;
var_125 = LVI_INT(1);
lval var_126;
{
lval var_128[2];
var_128[0] = var_112[2];
var_128[1] = var_125;
var_126 = wile_gen_list(2, var_128, NULL);
}
{
lval var_127[8];
var_127[0] = var_126;
// bld-rtl-dir/wile-rtl2-000082.scm:44
var_126 = wile_subtract(NULL, var_127, "bld-rtl-dir/wile-rtl2-000082.scm:44");
}
lval var_129;
{
lval var_130[1];
var_130[0] = var_126;
var_129 = wile_gen_list(1, var_130, NULL);
}
lval var_131;
{
lval var_132[2];
var_132[0] = V_CLOS(var_111,2);
var_132[1] = var_129;
var_131 = wile_gen_list(2, var_132, NULL);
}
var_131 = wile_apply_function(&(var_131), LISP_WHENCE);
var_120 = var_131;
break;
}
lval var_133;
var_133 = LVI_INT(1);
lval var_134;
{
lval var_136[2];
var_136[0] = var_112[0];
var_136[1] = var_133;
var_134 = wile_gen_list(2, var_136, NULL);
}
{
lval var_135[8];
var_135[0] = var_134;
// bld-rtl-dir/wile-rtl2-000082.scm:45
var_134 = wile_subtract(NULL, var_135, "bld-rtl-dir/wile-rtl2-000082.scm:45");
}
lval var_137;
{
lval var_138[3];
var_138[0] = var_134;
var_138[1] = var_116;
var_138[2] = var_112[2];
var_137 = wile_gen_list(3, var_138, NULL);
}
lval var_139;
{
lval var_140[2];
var_140[0] = V_CLOS(var_111,3);
var_140[1] = var_137;
var_139 = wile_gen_list(2, var_140, NULL);
}
var_139 = wile_apply_function(&(var_139), LISP_WHENCE);
var_120 = var_139;
} while (0);
var_114 = var_120;
} else {
lval var_141;
var_141 = LVI_BOOL(false);
var_114 = var_141;
}
return var_114;
}
// end of lambda fn_110

// @@@ (is-prime? n . k) @@@ bld-rtl-dir/wile-rtl2-000082.scm:15 @@@ wile_mr_primality @@@
lval wile_mr_primality(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
var_5 = LVI_BOOL(var_2[1].vt == LV_NIL);
if (LV_IS_FALSE(var_5)) {
lval var_6;
if (var_2[1].vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_6 = (var_2[1].v.pair.car ? *(var_2[1].v.pair.car) : LVI_NIL());
var_4 = var_6;
} else {
lval var_7;
var_7 = LVI_INT(100);
var_4 = var_7;
}
var_2[1] = var_4;
lval var_9;
switch (var_2[0].vt) {
case LV_REAL:
var_9 = LVI_BOOL(var_2[0].v.rv < 0.0);
break;
case LV_RAT:
var_9 = LVI_BOOL((var_2[0].v.irv.num < 0 && var_2[0].v.irv.den >= 0) || (var_2[0].v.irv.num > 0 && var_2[0].v.irv.den < 0));
break;
case LV_INT:
var_9 = LVI_BOOL(var_2[0].v.iv < 0);
break;
default:
WILE_EX("negative?", "expects a real-valued number");
}
if (LV_IS_FALSE(var_9)) {
(void)
 LVI_BOOL(false);
} else {
lval var_11;
{
lval var_13[1];
var_13[0] = var_2[0];
var_11 = wile_gen_list(1, var_13, NULL);
}
{
lval var_12[8];
var_12[0] = var_11;
// bld-rtl-dir/wile-rtl2-000082.scm:17
var_11 = wile_subtract(NULL, var_12, "bld-rtl-dir/wile-rtl2-000082.scm:17");
}
var_2[0] = var_11;
(void)
 var_2[0];
}
lval var_14;
do {
lval var_15;
var_15 = LVI_INT(1);
lval var_16;
switch (TYPE_COMBO(var_2[0].vt,var_15.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_16 = LVI_BOOL(var_2[0].v.iv <= var_15.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_16 = LVI_BOOL(var_2[0].v.iv * var_15.v.irv.den <= var_15.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_16 = LVI_BOOL(var_2[0].v.iv <= var_15.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_16 = LVI_BOOL(var_2[0].v.irv.num <= var_15.v.iv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_16 = LVI_BOOL(var_2[0].v.irv.num * var_15.v.irv.den <= var_15.v.irv.num * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_16 = LVI_BOOL(var_2[0].v.irv.num <= var_15.v.rv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_16 = LVI_BOOL(var_2[0].v.rv <= var_15.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_16 = LVI_BOOL(var_2[0].v.rv * var_15.v.irv.den <= var_15.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_16 = LVI_BOOL(var_2[0].v.rv <= var_15.v.rv);
break;
default:
WILE_EX("<=", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_16)) {
lval var_17;
var_17 = LVI_BOOL(false);
var_14 = var_17;
break;
}
lval var_18;
var_18 = LVI_INT(3);
lval var_19;
switch (TYPE_COMBO(var_2[0].vt,var_18.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_19 = LVI_BOOL(var_2[0].v.iv <= var_18.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_19 = LVI_BOOL(var_2[0].v.iv * var_18.v.irv.den <= var_18.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_19 = LVI_BOOL(var_2[0].v.iv <= var_18.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_19 = LVI_BOOL(var_2[0].v.irv.num <= var_18.v.iv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_19 = LVI_BOOL(var_2[0].v.irv.num * var_18.v.irv.den <= var_18.v.irv.num * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_19 = LVI_BOOL(var_2[0].v.irv.num <= var_18.v.rv * var_2[0].v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_19 = LVI_BOOL(var_2[0].v.rv <= var_18.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_19 = LVI_BOOL(var_2[0].v.rv * var_18.v.irv.den <= var_18.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_19 = LVI_BOOL(var_2[0].v.rv <= var_18.v.rv);
break;
default:
WILE_EX("<=", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_19)) {
lval var_20;
var_20 = LVI_BOOL(true);
var_14 = var_20;
break;
}
lval var_21;
var_21 = LVI_BOOL((var_2[0].v.iv)%2 == 0);
if (!LV_IS_FALSE(var_21)) {
lval var_22;
var_22 = LVI_BOOL(false);
var_14 = var_22;
break;
}
lval var_30;
var_30 = LVI_INT(1);
lval var_31;
{
lval var_33[2];
var_33[0] = var_2[0];
var_33[1] = var_30;
var_31 = wile_gen_list(2, var_33, NULL);
}
{
lval var_32[8];
var_32[0] = var_31;
// bld-rtl-dir/wile-rtl2-000082.scm:22
var_31 = wile_subtract(NULL, var_32, "bld-rtl-dir/wile-rtl2-000082.scm:22");
}
var_23 = var_31;
MK_CLOS(var_35,1);
lptr var_54 = new_lv(VT_UNINIT);
var_54->v.pair.car = &(var_24);
P_CLOS(var_35,0) = var_54;
var_24 = LVI_PROC(fn_34,var_35,2);
lval var_55;
var_55 = LVI_INT(0);
lval var_56;
{
lval var_57[2];
var_57[0] = var_23;
var_57[1] = var_55;
var_56 = wile_gen_list(2, var_57, NULL);
}
lval var_58;
{
lval var_59[2];
var_59[0] = var_24;
var_59[1] = var_56;
var_58 = wile_gen_list(2, var_59, NULL);
}
var_58 = wile_apply_function(&(var_58), LISP_WHENCE);
var_25 = var_58;
lval var_60;
if (var_25.vt != LV_PAIR) {
WILE_EX("car", "input is not a pair!");
}
var_60 = (var_25.v.pair.car ? *(var_25.v.pair.car) : LVI_NIL());
var_26 = var_60;
lval var_61;
var_61 = LVI_STRING("cadr");
lval var_62;
{
char* cp = strchr(var_61.v.str, 'r');
var_62 = var_25;
while (*(--cp) != 'c') {
if (var_62.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_62 = (var_62.v.pair.car ? *(var_62.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_62 = (var_62.v.pair.cdr ? *(var_62.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_61.v.str);
}
}
}
lval var_63;
var_63 = LVI_INT(1);
lval var_64;
{
lval var_66[2];
var_66[0] = var_62;
var_66[1] = var_63;
var_64 = wile_gen_list(2, var_66, NULL);
}
{
lval var_65[8];
var_65[0] = var_64;
// bld-rtl-dir/wile-rtl2-000082.scm:29
var_64 = wile_subtract(NULL, var_65, "bld-rtl-dir/wile-rtl2-000082.scm:29");
}
var_27 = var_64;
MK_CLOS(var_68,6);
lptr var_104 = new_lv(VT_UNINIT);
var_104->v.pair.car = &(var_28);
P_CLOS(var_68,5) = var_104;
lptr var_105 = new_lv(VT_UNINIT);
var_105->v.pair.car = &(var_27);
P_CLOS(var_68,4) = var_105;
lptr var_106 = new_lv(VT_UNINIT);
var_106->v.pair.car = &(var_29);
P_CLOS(var_68,3) = var_106;
lptr var_107 = new_lv(VT_UNINIT);
var_107->v.pair.car = &(var_23);
P_CLOS(var_68,2) = var_107;
lptr var_108 = new_lv(VT_UNINIT);
var_108->v.pair.car = &(var_26);
P_CLOS(var_68,1) = var_108;
lptr var_109 = new_lv(VT_UNINIT);
var_109->v.pair.car = &(var_2[0]);
P_CLOS(var_68,0) = var_109;
var_28 = LVI_PROC(fn_67,var_68,1);
MK_CLOS(var_111,4);
P_CLOS(var_111,3) = var_106;
P_CLOS(var_111,2) = var_104;
P_CLOS(var_111,1) = var_107;
P_CLOS(var_111,0) = var_109;
var_29 = LVI_PROC(fn_110,var_111,3);
lval var_142;
{
lval var_143[1];
var_143[0] = var_2[1];
var_142 = wile_gen_list(1, var_143, NULL);
}
lval var_144;
{
lval var_145[2];
var_145[0] = var_28;
var_145[1] = var_142;
var_144 = wile_gen_list(2, var_145, NULL);
}
var_144 = wile_apply_function(&(var_144), LISP_WHENCE);
*var_105 = var_27;
*var_54 = var_24;
*var_109 = var_2[0];
*var_106 = var_29;
*var_108 = var_26;
*var_104 = var_28;
*var_107 = var_23;
var_14 = var_144;
} while (0);
return var_14;
}
// end of function wile_mr_primality

// @@@ (next-prime n) @@@ bld-rtl-dir/wile-rtl2-000082.scm:50 @@@ wile_next_prime @@@
lval wile_next_prime(lptr* var_146, lptr var_147, const char* cloc)
{
lbl_148:;
lval var_149;
lval var_150;
lval var_151[8];
var_151[0] = var_147[0];
var_151[1] = LVI_NIL();
// bld-rtl-dir/wile-rtl2-000082.scm:51
var_150 = wile_mr_primality(NULL, var_151, "bld-rtl-dir/wile-rtl2-000082.scm:51");
if (LV_IS_FALSE(var_150)) {
lval var_154;
lval var_155;
var_155 = LVI_BOOL((var_147[0].v.iv)%2 == 0);
if (LV_IS_FALSE(var_155)) {
lval var_156;
var_156 = LVI_INT(2);
var_154 = var_156;
} else {
lval var_157;
var_157 = LVI_INT(1);
var_154 = var_157;
}
lval var_158;
{
lval var_160[2];
var_160[0] = var_147[0];
var_160[1] = var_154;
var_158 = wile_gen_list(2, var_160, NULL);
}
{
lval var_159[8];
var_159[0] = var_158;
// bld-rtl-dir/wile-rtl2-000082.scm:53
var_158 = wile_add(NULL, var_159, "bld-rtl-dir/wile-rtl2-000082.scm:53");
}
lval var_163[8];
var_163[0] = var_158;
var_147[0] = var_163[0];
// bld-rtl-dir/wile-rtl2-000082.scm:53
goto lbl_148;	// selfie
} else {
var_149 = var_147[0];
}
return var_149;
}
// end of function wile_next_prime
