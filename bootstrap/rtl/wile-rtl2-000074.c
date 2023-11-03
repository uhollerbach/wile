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

// @@@ (wile-build-info) @@@ bld-rtl-dir/wile-rtl2-000074.scm:13 @@@ wile_build_info @@@
lval wile_build_info(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_INT(wile_binfo());
var_4 = var_5;
lval var_6;
var_6 = LVI_NIL();
lval var_7;
var_7 = LVI_NIL();
lval var_8;
var_8 = wile_sql_version(NULL, NULL);
lval var_9;
{
lptr p1 = NULL, p2 = NULL;
if (var_8.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_8;
}
if (var_7.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_7;
}
var_9 = LVI_PAIR(p1, p2);
}
lval var_10;
var_10 = LVI_SYMBOL("sqlite-version");
lval var_11;
{
lptr p1 = NULL, p2 = NULL;
if (var_10.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_10;
}
if (var_9.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_9;
}
var_11 = LVI_PAIR(p1, p2);
}
lval var_12;
{
lptr p1 = NULL, p2 = NULL;
if (var_11.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_11;
}
if (var_6.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_6;
}
var_12 = LVI_PAIR(p1, p2);
}
lval var_13;
var_13 = LVI_NIL();
lval var_14;
var_14 = wile_gc_version(NULL, NULL);
lval var_15;
{
lptr p1 = NULL, p2 = NULL;
if (var_14.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_14;
}
if (var_13.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_13;
}
var_15 = LVI_PAIR(p1, p2);
}
lval var_16;
var_16 = LVI_SYMBOL("garbage-collection-version");
lval var_17;
{
lptr p1 = NULL, p2 = NULL;
if (var_16.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_16;
}
if (var_15.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_15;
}
var_17 = LVI_PAIR(p1, p2);
}
lval var_18;
{
lptr p1 = NULL, p2 = NULL;
if (var_17.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_17;
}
if (var_12.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_12;
}
var_18 = LVI_PAIR(p1, p2);
}
lval var_19;
var_19 = LVI_NIL();
lval var_20;
var_20 = LVI_INT(96);
lval var_21;
var_21 = LVI_INT(var_4.v.iv & var_20.v.iv);
lval var_22;
var_22 = LVI_INT(-5);
lval var_23;
var_23 = LVI_INT((var_22.v.iv >= 0) ? (var_21.v.iv << var_22.v.iv) : (var_21.v.iv >> -var_22.v.iv));
lval var_24;
if (var_23.vt != LV_INT) {
wile_exception2("case", __FILE__, __LINE__, "case-value type does not match case type");
}
switch (var_23.v.iv) {
case 0:
{
lval var_25;
var_25 = LVI_SYMBOL("long-int");
var_24 = var_25;
break;
}
case 1:
{
lval var_26;
var_26 = LVI_SYMBOL("int-128");
var_24 = var_26;
break;
}
case 2:
{
lval var_27;
var_27 = LVI_SYMBOL("semi-big-int-untested");
var_24 = var_27;
break;
}
default:
{
lval var_28;
var_28 = LVI_SYMBOL("unknown-int-type!?!");
var_24 = var_28;
break;
}
}
lval var_29;
{
lptr p1 = NULL, p2 = NULL;
if (var_24.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_24;
}
if (var_19.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_19;
}
var_29 = LVI_PAIR(p1, p2);
}
lval var_30;
var_30 = LVI_SYMBOL("integer-type");
lval var_31;
{
lptr p1 = NULL, p2 = NULL;
if (var_30.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_30;
}
if (var_29.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_29;
}
var_31 = LVI_PAIR(p1, p2);
}
lval var_32;
{
lptr p1 = NULL, p2 = NULL;
if (var_31.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_31;
}
if (var_18.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_18;
}
var_32 = LVI_PAIR(p1, p2);
}
lval var_33;
var_33 = LVI_NIL();
lval var_34;
var_34 = LVI_INT(24);
lval var_35;
var_35 = LVI_INT(var_4.v.iv & var_34.v.iv);
lval var_36;
var_36 = LVI_INT(-3);
lval var_37;
var_37 = LVI_INT((var_36.v.iv >= 0) ? (var_35.v.iv << var_36.v.iv) : (var_35.v.iv >> -var_36.v.iv));
lval var_38;
if (var_37.vt != LV_INT) {
wile_exception2("case", __FILE__, __LINE__, "case-value type does not match case type");
}
switch (var_37.v.iv) {
case 0:
{
lval var_39;
var_39 = LVI_SYMBOL("double");
var_38 = var_39;
break;
}
case 1:
{
lval var_40;
var_40 = LVI_SYMBOL("long-double");
var_38 = var_40;
break;
}
case 2:
{
lval var_41;
var_41 = LVI_SYMBOL("quad-double");
var_38 = var_41;
break;
}
default:
{
lval var_42;
var_42 = LVI_SYMBOL("unknown-float-type!?!");
var_38 = var_42;
break;
}
}
lval var_43;
{
lptr p1 = NULL, p2 = NULL;
if (var_38.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_38;
}
if (var_33.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_33;
}
var_43 = LVI_PAIR(p1, p2);
}
lval var_44;
var_44 = LVI_SYMBOL("float-type");
lval var_45;
{
lptr p1 = NULL, p2 = NULL;
if (var_44.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_44;
}
if (var_43.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_43;
}
var_45 = LVI_PAIR(p1, p2);
}
lval var_46;
{
lptr p1 = NULL, p2 = NULL;
if (var_45.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_45;
}
if (var_32.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_32;
}
var_46 = LVI_PAIR(p1, p2);
}
lval var_47;
var_47 = LVI_NIL();
lval var_48;
var_48 = LVI_NIL();
lval var_49;
var_49 = LVI_INT(1);
lval var_50;
{
lptr p1 = NULL, p2 = NULL;
if (var_49.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_49;
}
if (var_48.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_48;
}
var_50 = LVI_PAIR(p1, p2);
}
lval var_51;
var_51 = LVI_INT(10);
lval var_52;
{
lptr p1 = NULL, p2 = NULL;
if (var_51.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_51;
}
if (var_50.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_50;
}
var_52 = LVI_PAIR(p1, p2);
}
lval var_53;
var_53 = LVI_INT(0);
lval var_54;
{
lptr p1 = NULL, p2 = NULL;
if (var_53.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_53;
}
if (var_52.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_52;
}
var_54 = LVI_PAIR(p1, p2);
}
lval var_55;
{
lptr p1 = NULL, p2 = NULL;
if (var_54.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_54;
}
if (var_47.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_47;
}
var_55 = LVI_PAIR(p1, p2);
}
lval var_56;
var_56 = LVI_SYMBOL("wile-version");
lval var_57;
{
lptr p1 = NULL, p2 = NULL;
if (var_56.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_56;
}
if (var_55.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_55;
}
var_57 = LVI_PAIR(p1, p2);
}
lval var_58;
{
lptr p1 = NULL, p2 = NULL;
if (var_57.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_57;
}
if (var_46.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_46;
}
var_58 = LVI_PAIR(p1, p2);
}
lval var_59;
var_59 = LVI_NIL();
lval var_60;
var_60 = wile_arch_name();
lval var_61;
{
lptr p1 = NULL, p2 = NULL;
if (var_60.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_60;
}
if (var_59.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_59;
}
var_61 = LVI_PAIR(p1, p2);
}
lval var_62;
var_62 = LVI_SYMBOL("machine-architecture");
lval var_63;
{
lptr p1 = NULL, p2 = NULL;
if (var_62.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_62;
}
if (var_61.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_61;
}
var_63 = LVI_PAIR(p1, p2);
}
lval var_64;
{
lptr p1 = NULL, p2 = NULL;
if (var_63.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_63;
}
if (var_58.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_58;
}
var_64 = LVI_PAIR(p1, p2);
}
lval var_65;
var_65 = LVI_NIL();
lval var_66;
var_66 = wile_os_name();
lval var_67;
{
lptr p1 = NULL, p2 = NULL;
if (var_66.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_66;
}
if (var_65.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_65;
}
var_67 = LVI_PAIR(p1, p2);
}
lval var_68;
var_68 = LVI_SYMBOL("operating-system");
lval var_69;
{
lptr p1 = NULL, p2 = NULL;
if (var_68.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_68;
}
if (var_67.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_67;
}
var_69 = LVI_PAIR(p1, p2);
}
lval var_70;
{
lptr p1 = NULL, p2 = NULL;
if (var_69.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_69;
}
if (var_64.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_64;
}
var_70 = LVI_PAIR(p1, p2);
}
return var_70;
}
// end of function wile_build_info
