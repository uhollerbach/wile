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
static lval var_1 = LVI_STRING_NOCPY("1.0.1");		// wile-version

// definitions

// @@@ (wile-build-info add-ctime?) @@@ bld-rtl-dir/wile-rtl2-000075.scm:21 @@@ wile_build_info @@@
lval wile_build_info(lptr* var_2, lptr var_3, const char* cloc)
{
lval var_5;
lval var_6;
var_6 = LVI_INT(wile_binfo());
var_5 = var_6;
lval var_7;
lval var_8;
var_8 = LVI_NIL();
lval var_9;
var_9 = LVI_NIL();
lval var_10;
// bld-rtl-dir/wile-rtl2-000075.scm:37
var_10 = wile_sql_version(NULL, NULL, "bld-rtl-dir/wile-rtl2-000075.scm:37");
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
var_12 = LVI_SYMBOL("sqlite-version");
lval var_13;
{
lptr p1 = NULL, p2 = NULL;
if (var_12.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_12;
}
if (var_11.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_11;
}
var_13 = LVI_PAIR(p1, p2);
}
lval var_14;
{
lptr p1 = NULL, p2 = NULL;
if (var_13.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_13;
}
if (var_8.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_8;
}
var_14 = LVI_PAIR(p1, p2);
}
lval var_15;
var_15 = LVI_NIL();
lval var_16;
// bld-rtl-dir/wile-rtl2-000075.scm:36
var_16 = wile_gc_version(NULL, NULL, "bld-rtl-dir/wile-rtl2-000075.scm:36");
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
var_18 = LVI_SYMBOL("garbage-collection-version");
lval var_19;
{
lptr p1 = NULL, p2 = NULL;
if (var_18.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_18;
}
if (var_17.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_17;
}
var_19 = LVI_PAIR(p1, p2);
}
lval var_20;
{
lptr p1 = NULL, p2 = NULL;
if (var_19.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_19;
}
if (var_14.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_14;
}
var_20 = LVI_PAIR(p1, p2);
}
lval var_21;
var_21 = LVI_NIL();
lval var_22;
var_22 = LVI_INT(96);
lval var_23;
var_23 = LVI_INT(var_5.v.iv & var_22.v.iv);
lval var_24;
var_24 = LVI_INT(-5);
lval var_25;
var_25 = LVI_INT((var_24.v.iv >= 0) ? (var_23.v.iv << var_24.v.iv) : (var_23.v.iv >> -var_24.v.iv));
lval var_26;
if (var_25.vt != LV_INT) {
wile_exception("case", LISP_WHENCE, "case-value type does not match case type");
}
switch (var_25.v.iv) {
case 0:
{
lval var_27;
var_27 = LVI_SYMBOL("long-int");
var_26 = var_27;
break;
}
case 1:
{
lval var_28;
var_28 = LVI_SYMBOL("int-128");
var_26 = var_28;
break;
}
case 2:
{
lval var_29;
var_29 = LVI_SYMBOL("semi-big-int-untested");
var_26 = var_29;
break;
}
default:
{
lval var_30;
var_30 = LVI_SYMBOL("unknown-int-type!?!");
var_26 = var_30;
break;
}
}
lval var_31;
{
lptr p1 = NULL, p2 = NULL;
if (var_26.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_26;
}
if (var_21.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_21;
}
var_31 = LVI_PAIR(p1, p2);
}
lval var_32;
var_32 = LVI_SYMBOL("integer-type");
lval var_33;
{
lptr p1 = NULL, p2 = NULL;
if (var_32.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_32;
}
if (var_31.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_31;
}
var_33 = LVI_PAIR(p1, p2);
}
lval var_34;
{
lptr p1 = NULL, p2 = NULL;
if (var_33.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_33;
}
if (var_20.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_20;
}
var_34 = LVI_PAIR(p1, p2);
}
lval var_35;
var_35 = LVI_NIL();
lval var_36;
var_36 = LVI_INT(24);
lval var_37;
var_37 = LVI_INT(var_5.v.iv & var_36.v.iv);
lval var_38;
var_38 = LVI_INT(-3);
lval var_39;
var_39 = LVI_INT((var_38.v.iv >= 0) ? (var_37.v.iv << var_38.v.iv) : (var_37.v.iv >> -var_38.v.iv));
lval var_40;
if (var_39.vt != LV_INT) {
wile_exception("case", LISP_WHENCE, "case-value type does not match case type");
}
switch (var_39.v.iv) {
case 0:
{
lval var_41;
var_41 = LVI_SYMBOL("double");
var_40 = var_41;
break;
}
case 1:
{
lval var_42;
var_42 = LVI_SYMBOL("long-double");
var_40 = var_42;
break;
}
case 2:
{
lval var_43;
var_43 = LVI_SYMBOL("quad-double");
var_40 = var_43;
break;
}
default:
{
lval var_44;
var_44 = LVI_SYMBOL("unknown-float-type!?!");
var_40 = var_44;
break;
}
}
lval var_45;
{
lptr p1 = NULL, p2 = NULL;
if (var_40.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_40;
}
if (var_35.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_35;
}
var_45 = LVI_PAIR(p1, p2);
}
lval var_46;
var_46 = LVI_SYMBOL("float-type");
lval var_47;
{
lptr p1 = NULL, p2 = NULL;
if (var_46.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_46;
}
if (var_45.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_45;
}
var_47 = LVI_PAIR(p1, p2);
}
lval var_48;
{
lptr p1 = NULL, p2 = NULL;
if (var_47.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_47;
}
if (var_34.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_34;
}
var_48 = LVI_PAIR(p1, p2);
}
lval var_49;
var_49 = LVI_NIL();
lval var_50;
{
lptr p1 = NULL, p2 = NULL;
if (var_1.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_1;
}
if (var_49.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_49;
}
var_50 = LVI_PAIR(p1, p2);
}
lval var_51;
var_51 = LVI_SYMBOL("wile-version");
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
{
lptr p1 = NULL, p2 = NULL;
if (var_52.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_52;
}
if (var_48.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_48;
}
var_53 = LVI_PAIR(p1, p2);
}
lval var_54;
var_54 = LVI_NIL();
lval var_55;
var_55 = wile_arch_name();
lval var_56;
{
lptr p1 = NULL, p2 = NULL;
if (var_55.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_55;
}
if (var_54.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_54;
}
var_56 = LVI_PAIR(p1, p2);
}
lval var_57;
var_57 = LVI_SYMBOL("machine-architecture");
lval var_58;
{
lptr p1 = NULL, p2 = NULL;
if (var_57.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_57;
}
if (var_56.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_56;
}
var_58 = LVI_PAIR(p1, p2);
}
lval var_59;
{
lptr p1 = NULL, p2 = NULL;
if (var_58.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_58;
}
if (var_53.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_53;
}
var_59 = LVI_PAIR(p1, p2);
}
lval var_60;
var_60 = LVI_NIL();
lval var_61;
var_61 = wile_os_name();
lval var_62;
{
lptr p1 = NULL, p2 = NULL;
if (var_61.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_61;
}
if (var_60.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_60;
}
var_62 = LVI_PAIR(p1, p2);
}
lval var_63;
var_63 = LVI_SYMBOL("operating-system");
lval var_64;
{
lptr p1 = NULL, p2 = NULL;
if (var_63.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_63;
}
if (var_62.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_62;
}
var_64 = LVI_PAIR(p1, p2);
}
lval var_65;
{
lptr p1 = NULL, p2 = NULL;
if (var_64.vt != LV_NIL) {
p1 = new_lv(LV_NIL);
*p1 = var_64;
}
if (var_59.vt != LV_NIL) {
p2 = new_lv(LV_NIL);
*p2 = var_59;
}
var_65 = LVI_PAIR(p1, p2);
}
var_7 = var_65;
return var_7;
}
// end of function wile_build_info
