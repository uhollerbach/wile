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
static lval fn_11(lptr*, lptr);
static lval fn_19(lptr*, lptr);
static lval fn_35(lptr*, lptr);
static lval fn_50(lptr*, lptr);
static lval fn_60(lptr*, lptr);

// definitions

// @@@ lambda (a1) @@@ bld-rtl-dir/wile-rtl2-000057.scm:16 @@@ fn_11 @@@
static lval fn_11(lptr* var_12, lptr var_13)
{
lval var_15;
var_15 = LVI_STRING("cadr");
lval var_16;
{
char* cp = strchr(var_15.v.str, 'r');
var_16 = var_13[0];
while (*(--cp) != 'c') {
if (var_16.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_16 = (var_16.v.pair.car ? *(var_16.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_16 = (var_16.v.pair.cdr ? *(var_16.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_15.v.str);
}
}
}
return var_16;
}
// end of lambda fn_11

// @@@ lambda (a1) @@@ bld-rtl-dir/wile-rtl2-000057.scm:17 @@@ fn_19 @@@
static lval fn_19(lptr* var_20, lptr var_21)
{
lval var_23;
var_23 = LVI_STRING("caddr");
lval var_24;
{
char* cp = strchr(var_23.v.str, 'r');
var_24 = var_21[0];
while (*(--cp) != 'c') {
if (var_24.vt != LV_PAIR) {
WILE_EX("cxr", "input does not have the right structure!");
}
if (*cp == 'a') {
var_24 = (var_24.v.pair.car ? *(var_24.v.pair.car) : LVI_NIL());
} else if (*cp == 'd') {
var_24 = (var_24.v.pair.cdr ? *(var_24.v.pair.cdr) : LVI_NIL());
} else {
WILE_EX("cxr", "got malformed control string '%s'", var_23.v.str);
}
}
}
return var_24;
}
// end of lambda fn_19

// @@@ lambda (s) @@@ bld-rtl-dir/wile-rtl2-000057.scm:21 @@@ fn_35 @@@
static lval fn_35(lptr* var_36, lptr var_37)
{
lval var_39;
lval var_40;
var_40 = LVI_STRING("^[-+]?[0-9]+$");
lval var_41;
{
lval vs[8];
vs[0] = var_40;
vs[1] = var_37[0];
var_41 = wile_regex_match(NULL, vs);
}
if (LV_IS_FALSE(var_41)) {
lval var_42;
var_42 = LVI_STRING("");
lval var_43;
var_43 = LVI_STRING("\'");
lval var_44;
var_44 = LVI_STRING("\'");
lval var_45;
{
lval vs[3];
vs[0] = var_43;
vs[1] = var_37[0];
vs[2] = var_44;
var_45 = wile_gen_list(3, vs, NULL);
}
{
lval vs[8];
vs[0] = var_42;
vs[1] = var_45;
var_45 = wile_string_join_by(NULL, vs);
}
var_39 = var_45;
} else {
var_39 = var_37[0];
}
return var_39;
}
// end of lambda fn_35

// @@@ lambda (n t) @@@ bld-rtl-dir/wile-rtl2-000057.scm:28 @@@ fn_50 @@@
static lval fn_50(lptr* var_51, lptr var_52)
{
lval var_54;
var_54 = LVI_STRING(" ");
lval var_55;
{
lval vs[2];
vs[0] = var_52[0];
vs[1] = var_52[1];
var_55 = wile_gen_list(2, vs, NULL);
}
{
lval vs[8];
vs[0] = var_54;
vs[1] = var_55;
var_55 = wile_string_join_by(NULL, vs);
}
return var_55;
}
// end of lambda fn_50

// @@@ lambda (v) @@@ bld-rtl-dir/wile-rtl2-000057.scm:31 @@@ fn_60 @@@
static lval fn_60(lptr* var_61, lptr var_62)
{
lval var_64;
var_64 = LVI_STRING("INSERT INTO ");
lval var_65;
var_65 = LVI_STRING(" VALUES (");
lval var_66;
var_66 = LVI_STRING(",");
lval var_67;
var_67 = LVI_NIL();
{
lval vs[8];
vs[0] = V_CLOS(var_61,2);
vs[1] = var_62[0];
vs[2] = var_67;
var_67 = wile_map(NULL, vs);
}
lval var_68;
{
lval vs[1];
vs[0] = var_67;
var_68 = wile_gen_list(1, vs, NULL);
}
{
lval vs[8];
vs[0] = var_66;
vs[1] = var_68;
var_68 = wile_string_join_by(NULL, vs);
}
lval var_69;
var_69 = LVI_STRING(");\n");
lval var_70;
{
lval vs[6];
vs[0] = V_CLOS(var_61,0);
vs[1] = var_64;
vs[2] = V_CLOS(var_61,1);
vs[3] = var_65;
vs[4] = var_68;
vs[5] = var_69;
var_70 = wile_gen_list(6, vs, NULL);
}
{
lval vs[8];
vs[0] = var_70;
var_70 = wile_write_string(NULL, vs);
}
return var_70;
}
// end of lambda fn_60

// @@@ (sqlite-dump-table sport tbl oport) @@@ bld-rtl-dir/wile-rtl2-000057.scm:13 @@@ wile_sql_dump_table @@@
lval wile_sql_dump_table(lptr* var_1, lptr var_2)
{
lval var_4;
lval var_5;
var_5 = LVI_STRING("");
lval var_6;
var_6 = LVI_STRING("pragma table_info(\'");
lval var_7;
var_7 = LVI_STRING("\')");
lval var_8;
{
lval vs[3];
vs[0] = var_6;
vs[1] = var_2[1];
vs[2] = var_7;
var_8 = wile_gen_list(3, vs, NULL);
}
{
lval vs[8];
vs[0] = var_5;
vs[1] = var_8;
var_8 = wile_string_join_by(NULL, vs);
}
lval var_9;
#ifdef WILE_USES_SQLITE
if (var_2[0].vt == LV_SQLITE_PORT && var_8.vt == LV_STRING) {
var_9 = wile_sql_run(var_2[0].v.sqlite_conn, var_8.v.str, __FILE__, __LINE__);
} else {
WILE_EX("sqlite-run", "expects one sqlite-port and one string");
}
#else
var_9 = LVI_BOOL(false);
#endif // WILE_USES_SQLITE
var_4 = var_9;
lval var_10;
MK_CLOS(var_12,0);
lval var_17;
var_17 = LVI_NIL();
{
lval vs[8];
vs[0] = LVI_PROC(fn_11,var_12,1);
vs[1] = var_4;
vs[2] = var_17;
var_17 = wile_map(NULL, vs);
}
var_10 = var_17;
lval var_18;
MK_CLOS(var_20,0);
lval var_25;
var_25 = LVI_NIL();
{
lval vs[8];
vs[0] = LVI_PROC(fn_19,var_20,1);
vs[1] = var_4;
vs[2] = var_25;
var_25 = wile_map(NULL, vs);
}
var_18 = var_25;
lval var_26;
lval var_27;
var_27 = LVI_STRING(" ");
lval var_28;
var_28 = LVI_STRING("select * from");
lval var_29;
{
lval vs[2];
vs[0] = var_28;
vs[1] = var_2[1];
var_29 = wile_gen_list(2, vs, NULL);
}
{
lval vs[8];
vs[0] = var_27;
vs[1] = var_29;
var_29 = wile_string_join_by(NULL, vs);
}
lval var_30;
#ifdef WILE_USES_SQLITE
if (var_2[0].vt == LV_SQLITE_PORT && var_29.vt == LV_STRING) {
var_30 = wile_sql_run(var_2[0].v.sqlite_conn, var_29.v.str, __FILE__, __LINE__);
} else {
WILE_EX("sqlite-run", "expects one sqlite-port and one string");
}
#else
var_30 = LVI_BOOL(false);
#endif // WILE_USES_SQLITE
var_26 = var_30;
lval var_32;
var_32 = LVI_STRING(",");
lval var_33;
{
lval vs[1];
vs[0] = var_10;
var_33 = wile_gen_list(1, vs, NULL);
}
{
lval vs[8];
vs[0] = var_32;
vs[1] = var_33;
var_33 = wile_string_join_by(NULL, vs);
}
lval var_34;
MK_CLOS(var_36,0);
var_34 = LVI_PROC(fn_35,var_36,1);
lval var_46;
var_46 = LVI_STRING("DROP TABLE IF EXISTS ");
lval var_47;
var_47 = LVI_STRING(";\n\nCREATE TABLE ");
lval var_48;
var_48 = LVI_STRING(" (");
lval var_49;
var_49 = LVI_STRING(", ");
MK_CLOS(var_51,0);
lval var_56;
{
lval vs[1];
vs[0] = var_18;
var_56 = wile_gen_list(1, vs, NULL);
}
{
lval vs[8];
vs[0] = LVI_PROC(fn_50,var_51,2);
vs[1] = var_10;
vs[2] = var_56;
var_56 = wile_map(NULL, vs);
}
lval var_57;
{
lval vs[1];
vs[0] = var_56;
var_57 = wile_gen_list(1, vs, NULL);
}
{
lval vs[8];
vs[0] = var_49;
vs[1] = var_57;
var_57 = wile_string_join_by(NULL, vs);
}
lval var_58;
var_58 = LVI_STRING(");\n\nBEGIN TRANSACTION;\n\n");
lval var_59;
{
lval vs[8];
vs[0] = var_2[2];
vs[1] = var_46;
vs[2] = var_2[1];
vs[3] = var_47;
vs[4] = var_2[1];
vs[5] = var_48;
vs[6] = var_57;
vs[7] = var_58;
var_59 = wile_gen_list(8, vs, NULL);
}
{
lval vs[8];
vs[0] = var_59;
var_59 = wile_write_string(NULL, vs);
}
MK_CLOS(var_61,3);
P_CLOS(var_61,2) = &(var_34);
P_CLOS(var_61,1) = &(var_2[1]);
P_CLOS(var_61,0) = &(var_2[2]);
lval var_71;
var_71 = LVI_NIL();
{
lval vs[8];
vs[0] = LVI_PROC(fn_60,var_61,1);
vs[1] = var_26;
vs[2] = var_71;
var_71 = wile_map(NULL, vs);
}
lval var_72;
var_72 = LVI_STRING("\nCOMMIT;\n");
lval var_73;
{
lval vs[2];
vs[0] = var_2[2];
vs[1] = var_72;
var_73 = wile_gen_list(2, vs, NULL);
}
{
lval vs[8];
vs[0] = var_73;
var_73 = wile_write_string(NULL, vs);
}
return var_73;
}
// end of function wile_sql_dump_table
