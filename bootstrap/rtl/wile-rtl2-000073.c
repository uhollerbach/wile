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
static lval fn_45(lptr*, lptr, const char*);

// definitions

// @@@ bytevector-length @@@ bld-rtl-dir/wile-rtl2-000073.scm:15 @@@ fn_7 @@@
static lval fn_7(lptr* var_8, lptr var_9, const char* cloc)
{
lval var_11;
{
if (var_9[0].vt != LV_BVECTOR) {
wile_exception("bytevector-length", "bld-rtl-dir/wile-rtl2-000073.scm:15", "input is not a bytevector");
}
var_11 = LVI_INT(var_9[0].v.bvec.capa);
}
return var_11;
}
// end of prim fn_7

// @@@ min @@@ bld-rtl-dir/wile-rtl2-000073.scm:16 @@@ fn_15 @@@
static lval fn_15(lptr* var_16, lptr var_17, const char* cloc)
{
lval var_19;
var_19 = var_17[0];
{
lval var_20[8];
var_20[0] = var_19;
// bld-rtl-dir/wile-rtl2-000073.scm:16
var_19 = wile_min(NULL, var_20, "bld-rtl-dir/wile-rtl2-000073.scm:16");
}
return var_19;
}
// end of prim fn_15

// @@@ max @@@ bld-rtl-dir/wile-rtl2-000073.scm:16 @@@ fn_23 @@@
static lval fn_23(lptr* var_24, lptr var_25, const char* cloc)
{
lval var_27;
var_27 = var_25[0];
{
lval var_28[8];
var_28[0] = var_27;
// bld-rtl-dir/wile-rtl2-000073.scm:16
var_27 = wile_max(NULL, var_28, "bld-rtl-dir/wile-rtl2-000073.scm:16");
}
return var_27;
}
// end of prim fn_23

// @@@ lambda (v) @@@ bld-rtl-dir/wile-rtl2-000073.scm:21 @@@ fn_45 @@@
static lval fn_45(lptr* var_46, lptr var_47, const char* cloc)
{
lval var_49;
{
if (var_47[0].vt != LV_BVECTOR) {
wile_exception("bytevector-ref", "bld-rtl-dir/wile-rtl2-000073.scm:21", "input is not a bytevector");
}
if (V_CLOS(var_46,0).vt != LV_INT || V_CLOS(var_46,0).v.iv < 0 || (size_t) V_CLOS(var_46,0).v.iv >= var_47[0].v.bvec.capa) {
wile_exception("bytevector-ref", "bld-rtl-dir/wile-rtl2-000073.scm:21", "got bad index value");
}
var_49 = LVI_INT(var_47[0].v.bvec.arr[V_CLOS(var_46,0).v.iv]);
}
return var_49;
}
// end of lambda fn_45

// @@@ (bytevector-for-each proc vec . vecs) @@@ bld-rtl-dir/wile-rtl2-000073.scm:13 @@@ wile_bytevector_foreach @@@
lval wile_bytevector_foreach(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
lval var_5;
{
lptr p1 = NULL, p2 = NULL;
if (var_2[1].vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_2[1];
}
if (var_2[2].vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_2[2];
}
var_5 = LVI_PAIR(p1, p2);
}
var_4 = var_5;
lval var_6;
lval var_12;
var_12 = LVI_NIL();
{
lval var_13[8];
var_13[0] = LVI_PROC(fn_7,NULL,1);
var_13[1] = var_4;
var_13[2] = var_12;
// bld-rtl-dir/wile-rtl2-000073.scm:15
var_12 = wile_map(NULL, var_13, "bld-rtl-dir/wile-rtl2-000073.scm:15");
}
var_6 = var_12;
lval var_21;
{
lval var_22[2];
var_22[0] = LVI_PROC(fn_15,NULL,-1);
var_22[1] = var_6;
var_21 = wile_gen_list(2, var_22, NULL);
}
var_21 = wile_apply_function(&(var_21), "bld-rtl-dir/wile-rtl2-000073.scm:16");
lval var_29;
{
lval var_30[2];
var_30[0] = LVI_PROC(fn_23,NULL,-1);
var_30[1] = var_6;
var_29 = wile_gen_list(2, var_30, NULL);
}
var_29 = wile_apply_function(&(var_29), "bld-rtl-dir/wile-rtl2-000073.scm:16");
lval var_31;
switch (TYPE_COMBO(var_21.vt,var_29.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_31 = LVI_BOOL(var_21.v.iv == var_29.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_31 = LVI_BOOL(var_21.v.iv * var_29.v.irv.den == var_29.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_31 = LVI_BOOL(var_21.v.iv == var_29.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_31 = LVI_BOOL(var_21.v.irv.num == var_29.v.iv * var_21.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_31 = LVI_BOOL(var_21.v.irv.num * var_29.v.irv.den == var_29.v.irv.num * var_21.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_31 = LVI_BOOL(var_21.v.irv.num == var_29.v.rv * var_21.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_31 = LVI_BOOL(var_21.v.rv == var_29.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_31 = LVI_BOOL(var_21.v.rv * var_29.v.irv.den == var_29.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_31 = LVI_BOOL(var_21.v.rv == var_29.v.rv);
break;
default:
wile_exception("==", "bld-rtl-dir/wile-rtl2-000073.scm:16", "inputs are not real-valued numbers");
break;
}
if (LV_IS_FALSE(var_31)) {
lval var_32;
var_32 = LVI_STRING("bytevector-for-each: unequal vector lengths");
lval var_33;
{
lval var_34[1];
var_34[0] = var_32;
var_33 = wile_gen_list(1, var_34, NULL);
}
if (var_33.vt == LV_PAIR && (var_33.v.pair.cdr == NULL || var_33.v.pair.cdr->vt == LV_NIL)) {
var_33 = (var_33.v.pair.car ? *(var_33.v.pair.car) : LVI_NIL());
}
cachalot->errval = new_lv(LV_NIL);
*(cachalot->errval) = var_33;
cachalot->whence = "bld-rtl-dir/wile-rtl2-000073.scm:17";
longjmp(cachalot->cenv, 1);
} else {
}
lval var_36;
lval var_37;
if (var_6.vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000073.scm:18", "input is not a pair!");
}
var_37 = (var_6.v.pair.car ? *(var_6.v.pair.car) : LVI_NIL());
var_36 = var_37;
lval var_39;
lval var_40;
lval var_41;
var_41 = LVI_INT(0);
var_39 = var_41;
lval var_38;
lptr var_42 = new_lv(VT_UNINIT);
var_42->v.pair.car = &(var_39); // i
do {
lval var_43;
switch (TYPE_COMBO(var_39.vt,var_36.vt)) {
case TYPE_COMBO(LV_INT,LV_INT):
var_43 = LVI_BOOL(var_39.v.iv == var_36.v.iv);
break;
case TYPE_COMBO(LV_INT,LV_RAT):
var_43 = LVI_BOOL(var_39.v.iv * var_36.v.irv.den == var_36.v.irv.num);
break;
case TYPE_COMBO(LV_INT,LV_REAL):
var_43 = LVI_BOOL(var_39.v.iv == var_36.v.rv);
break;
case TYPE_COMBO(LV_RAT,LV_INT):
var_43 = LVI_BOOL(var_39.v.irv.num == var_36.v.iv * var_39.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_RAT):
var_43 = LVI_BOOL(var_39.v.irv.num * var_36.v.irv.den == var_36.v.irv.num * var_39.v.irv.den);
break;
case TYPE_COMBO(LV_RAT,LV_REAL):
var_43 = LVI_BOOL(var_39.v.irv.num == var_36.v.rv * var_39.v.irv.den);
break;
case TYPE_COMBO(LV_REAL,LV_INT):
var_43 = LVI_BOOL(var_39.v.rv == var_36.v.iv);
break;
case TYPE_COMBO(LV_REAL,LV_RAT):
var_43 = LVI_BOOL(var_39.v.rv * var_36.v.irv.den == var_36.v.irv.num);
break;
case TYPE_COMBO(LV_REAL,LV_REAL):
var_43 = LVI_BOOL(var_39.v.rv == var_36.v.rv);
break;
default:
wile_exception("==", "bld-rtl-dir/wile-rtl2-000073.scm:20", "inputs are not real-valued numbers");
break;
}
if (!LV_IS_FALSE(var_43)) {
lval var_44;
var_44 = LVI_BOOL(true);
var_38 = var_44;
break;
}
MK_CLOS(var_46,1);
P_CLOS(var_46,0) = var_42;
lval var_50;
var_50 = LVI_NIL();
{
lval var_51[8];
var_51[0] = LVI_PROC(fn_45,var_46,1);
var_51[1] = var_4;
var_51[2] = var_50;
// bld-rtl-dir/wile-rtl2-000073.scm:21
var_50 = wile_map(NULL, var_51, "bld-rtl-dir/wile-rtl2-000073.scm:21");
}
lval var_52;
{
lval var_53[2];
var_53[0] = var_2[0];
var_53[1] = var_50;
var_52 = wile_gen_list(2, var_53, NULL);
}
var_52 = wile_apply_function(&(var_52), "bld-rtl-dir/wile-rtl2-000073.scm:21");
lval var_54;
var_54 = LVI_INT(1);
lval var_55;
{
lval var_57[2];
var_57[0] = var_39;
var_57[1] = var_54;
var_55 = wile_gen_list(2, var_57, NULL);
}
{
lval var_56[8];
var_56[0] = var_55;
// bld-rtl-dir/wile-rtl2-000073.scm:19
var_55 = wile_add(NULL, var_56, "bld-rtl-dir/wile-rtl2-000073.scm:19");
}
var_40 = var_55;
var_39 = var_40;
} while (1);
*var_42 = var_39;
return var_38;
}
// end of function wile_bytevector_foreach
