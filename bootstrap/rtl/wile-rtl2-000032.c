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

// @@@ (* . vs) @@@ bld-rtl-dir/wile-rtl2-000032.scm:13 @@@ wile_multiply @@@
lval wile_multiply(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
var_5 = LVI_BOOL(var_2[0].vt == LV_NIL);
if (LV_IS_FALSE(var_5)) {
lval var_6;
lval var_7;
var_7 = LVI_INT(0);
var_6 = var_7;
lval var_8;
var_8 = var_2[0];
lval var_12;
lval var_13;
lval var_14;
var_14 = LVI_INT(0);
var_12 = var_14;
lptr var_15 = new_lv(VT_UNINIT);
var_15->v.pair.car = &(var_12);
lbl_10:
lval var_16;
var_16 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_16)) {
goto lbl_11;
}
lval var_17;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000032.scm:19", "input is not a pair!");
}
var_17 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_18;
switch (var_17.vt) {
case LV_INT:
var_18 = LVI_INT(0);
break;
case LV_RAT:
var_18 = LVI_INT(1);
break;
case LV_REAL:
var_18 = LVI_INT(2);
break;
case LV_CMPLX:
var_18 = LVI_INT(3);
break;
default:
var_18 = LVI_INT(4);
break;
}
lval var_19;
var_19 = LVI_INT((var_6.v.iv > var_18.v.iv) ? var_6.v.iv : var_18.v.iv);
var_6 = var_19;
lval var_20;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000032.scm:20", "input is not a pair!");
}
var_20 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_20;
lval var_21;
var_21 = LVI_INT(1);
lval var_22;
var_22 = LVI_INT(var_12.v.iv + var_21.v.iv);
var_13 = var_22;
var_12 = var_13;
goto lbl_10;
lbl_11:;
*var_15 = var_12;
var_8 = var_2[0];
lval var_23;
if (var_6.vt != LV_INT) {
wile_exception("case", "bld-rtl-dir/wile-rtl2-000032.scm:22", "case-value type does not match case type");
}
switch (var_6.v.iv) {
case 0:
{
lval var_24;
lval var_25;
var_25 = LVI_INT(1);
var_24 = var_25;
lval var_29;
lval var_30;
lval var_31;
var_31 = LVI_INT(0);
var_29 = var_31;
lptr var_32 = new_lv(VT_UNINIT);
var_32->v.pair.car = &(var_29);
lbl_27:
lval var_33;
var_33 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_33)) {
goto lbl_28;
}
lval var_34;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000032.scm:25", "input is not a pair!");
}
var_34 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_35;
var_35 = LVI_INT(var_24.v.iv * var_34.v.iv);
var_24 = var_35;
lval var_36;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000032.scm:26", "input is not a pair!");
}
var_36 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_36;
lval var_37;
var_37 = LVI_INT(1);
lval var_38;
var_38 = LVI_INT(var_29.v.iv + var_37.v.iv);
var_30 = var_38;
var_29 = var_30;
goto lbl_27;
lbl_28:;
*var_32 = var_29;
var_23 = var_24;
break;
}
case 1:
{
lval var_39;
lval var_40;
var_40 = LVI_INT(1);
lval var_41;
if (var_40.vt == LV_INT) {
var_41 = LVI_RAT(var_40.v.iv, 1);
} else {
var_41 = var_40;
}
var_39 = var_41;
lval var_45;
lval var_46;
lval var_47;
var_47 = LVI_INT(0);
var_45 = var_47;
lptr var_48 = new_lv(VT_UNINIT);
var_48->v.pair.car = &(var_45);
lbl_43:
lval var_49;
var_49 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_49)) {
goto lbl_44;
}
lval var_50;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000032.scm:30", "input is not a pair!");
}
var_50 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_51;
if (var_50.vt == LV_INT) {
var_51 = LVI_RAT(var_50.v.iv, 1);
} else {
var_51 = var_50;
}
lval var_52;
{
lisp_int_t n, d, g;
n = var_39.v.irv.num * var_51.v.irv.num;
d = var_39.v.irv.den * var_51.v.irv.den;
g = lgcd(n, d);
n /= g;
d /= g;
var_52 = LVI_RAT(n, d);
}
var_39 = var_52;
lval var_53;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000032.scm:31", "input is not a pair!");
}
var_53 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_53;
lval var_54;
var_54 = LVI_INT(1);
lval var_55;
var_55 = LVI_INT(var_45.v.iv + var_54.v.iv);
var_46 = var_55;
var_45 = var_46;
goto lbl_43;
lbl_44:;
*var_48 = var_45;
lval var_56;
lval var_57;
var_57 = LVI_INT(var_39.v.irv.den);
lval var_58;
var_58 = LVI_INT(1);
lval var_59;
switch (TYPE_COMBO(var_57.vt,var_58.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_59 = LVI_BOOL(var_57.v.iv == var_58.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_59 = LVI_BOOL(var_57.v.iv * var_58.v.irv.den == var_58.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_59 = LVI_BOOL(var_57.v.iv == var_58.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_59 = LVI_BOOL(var_57.v.irv.num == var_58.v.iv * var_57.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_59 = LVI_BOOL(var_57.v.irv.num * var_58.v.irv.den == var_58.v.irv.num * var_57.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_59 = LVI_BOOL(var_57.v.irv.num == var_58.v.rv * var_57.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_59 = LVI_BOOL(var_57.v.rv == var_58.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_59 = LVI_BOOL(var_57.v.rv * var_58.v.irv.den == var_58.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_59 = LVI_BOOL(var_57.v.rv == var_58.v.rv);
break;
default:
wile_exception("==", "bld-rtl-dir/wile-rtl2-000032.scm:32", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_59)) {
var_56 = var_39;
} else {
lval var_60;
var_60 = LVI_INT(var_39.v.irv.num);
var_56 = var_60;
}
var_23 = var_56;
break;
}
case 2:
{
lval var_61;
lval var_62;
var_62 = LVI_REAL(1.00000000000000000000000000000000000e+00Q);
var_61 = var_62;
lval var_66;
lval var_67;
lval var_68;
var_68 = LVI_INT(0);
var_66 = var_68;
lptr var_69 = new_lv(VT_UNINIT);
var_69->v.pair.car = &(var_66);
lbl_64:
lval var_70;
var_70 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_70)) {
goto lbl_65;
}
lval var_71;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000032.scm:37", "input is not a pair!");
}
var_71 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_72;
if (var_71.vt == LV_INT) {
var_72 = LVI_REAL((lisp_real_t) var_71.v.iv);
} else if (var_71.vt == LV_RAT) {
var_72 = LVI_REAL(LV_RAT2REAL(var_71));
} else {
var_72 = var_71;
}
lval var_73;
var_73 = LVI_REAL(var_61.v.rv * var_72.v.rv);
var_61 = var_73;
lval var_74;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000032.scm:38", "input is not a pair!");
}
var_74 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_74;
lval var_75;
var_75 = LVI_INT(1);
lval var_76;
var_76 = LVI_INT(var_66.v.iv + var_75.v.iv);
var_67 = var_76;
var_66 = var_67;
goto lbl_64;
lbl_65:;
*var_69 = var_66;
var_23 = var_61;
break;
}
case 3:
{
lval var_77;
lval var_78;
var_78 = LVI_REAL(1.00000000000000000000000000000000000e+00Q);
lval var_79;
var_79 = LVI_REAL(0.00000000000000000000000000000000000e+00Q);
lval var_81;
if (var_78.vt == LV_INT) {
var_81 = LVI_REAL((lisp_real_t) var_78.v.iv);
} else if (var_78.vt == LV_RAT) {
var_81 = LVI_REAL(LV_RAT2REAL(var_78));
} else if (var_78.vt == LV_REAL) {
var_81 = var_78;
} else {
wile_exception("cmplx", "bld-rtl-dir/wile-rtl2-000032.scm:40", "expects a real-valued input");
}
lval var_82;
if (var_79.vt == LV_INT) {
var_82 = LVI_REAL((lisp_real_t) var_79.v.iv);
} else if (var_79.vt == LV_RAT) {
var_82 = LVI_REAL(LV_RAT2REAL(var_79));
} else if (var_79.vt == LV_REAL) {
var_82 = var_79;
} else {
wile_exception("cmplx", "bld-rtl-dir/wile-rtl2-000032.scm:40", "expects a real-valued input");
}
lval var_80;
var_80 = LVI_CMPLX2(var_81.v.rv, var_82.v.rv);
var_77 = var_80;
lval var_86;
lval var_87;
lval var_88;
var_88 = LVI_INT(0);
var_86 = var_88;
lptr var_89 = new_lv(VT_UNINIT);
var_89->v.pair.car = &(var_86);
lbl_84:
lval var_90;
var_90 = LVI_BOOL(var_8.vt == LV_NIL);
if (!LV_IS_FALSE(var_90)) {
goto lbl_85;
}
lval var_91;
if (var_8.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000032.scm:42", "input is not a pair!");
}
var_91 = (var_8.v.pair.car ? *(var_8.v.pair.car) : LVI_NIL());
lval var_92;
switch (var_91.vt) {
case LV_INT:
var_92 = LVI_CMPLX2((lisp_real_t) var_91.v.iv, 0);
break;
case LV_RAT:
var_92 = LVI_CMPLX2(LV_RAT2REAL(var_91), 0);
break;
case LV_REAL:
var_92 = LVI_CMPLX2(var_91.v.rv, 0);
break;
default:
var_92 = var_91;
break;
}
lval var_93;
var_93 = LVI_CMPLX1(var_77.v.cv * var_92.v.cv);
var_77 = var_93;
lval var_94;
if (var_8.vt != LV_PAIR) {
wile_exception("cdr", "bld-rtl-dir/wile-rtl2-000032.scm:43", "input is not a pair!");
}
var_94 = (var_8.v.pair.cdr ? *(var_8.v.pair.cdr) : LVI_NIL());
var_8 = var_94;
lval var_95;
var_95 = LVI_INT(1);
lval var_96;
var_96 = LVI_INT(var_86.v.iv + var_95.v.iv);
var_87 = var_96;
var_86 = var_87;
goto lbl_84;
lbl_85:;
*var_89 = var_86;
var_23 = var_77;
break;
}
default:
{
lval var_97;
var_97 = LVI_STRING("\'*\' got a non-numeric argument");
lval var_98;
{
lval var_99[1];
var_99[0] = var_97;
var_98 = wile_gen_list(1, var_99, NULL);
}
if (var_98.vt == LV_PAIR && (var_98.v.pair.cdr == NULL || var_98.v.pair.cdr->vt == LV_NIL)) {
var_98 = (var_98.v.pair.car ? *(var_98.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_98;
cachalot->whence = "bld-rtl-dir/wile-rtl2-000032.scm:45";
longjmp(cachalot->cenv, 1);
var_23 = var_98;
break;
}
}
var_4 = var_23;
} else {
lval var_100;
var_100 = LVI_INT(1);
var_4 = var_100;
}
return var_4;
}
// end of function wile_multiply
