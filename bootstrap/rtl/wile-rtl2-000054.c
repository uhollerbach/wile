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

// @@@ car @@@ bld-rtl-dir/wile-rtl2-000054.scm:14 @@@ fn_4 @@@
static lval fn_4(lptr* var_5, lptr var_6, const char* cloc)
{
lval var_8;
if (var_6[0].vt != LV_PAIR) {
wile_exception("car", "bld-rtl-dir/wile-rtl2-000054.scm:14", "input is not a pair!");
}
var_8 = (var_6[0].v.pair.car ? *(var_6[0].v.pair.car) : LVI_NIL());
return var_8;
}
// end of prim fn_4

// @@@ (sqlite-meta-tables port) @@@ bld-rtl-dir/wile-rtl2-000054.scm:13 @@@ wile_sql_meta_tables @@@
lval wile_sql_meta_tables(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_9;
var_9 = LVI_STRING("select name from sqlite_schema");
lval var_10;
#ifdef WILE_USES_SQLITE
if (var_2[0].vt == LV_SQLITE_PORT && var_9.vt == LV_STRING) {
var_10 = wile_sql_run(var_2[0].v.sqlite_conn, var_9.v.str, "bld-rtl-dir/wile-rtl2-000054.scm:14");
} else {
wile_exception("sqlite-run", "bld-rtl-dir/wile-rtl2-000054.scm:14", "expects one sqlite-port and one string");
}
#else
var_10 = LVI_BOOL(false);
#endif // WILE_USES_SQLITE
lval var_11;
var_11 = LVI_NIL();
{
lval var_12[8];
var_12[0] = LVI_PROC(fn_4,NULL,1);
var_12[1] = var_10;
var_12[2] = var_11;
var_11 = wile_map(NULL, var_12, "bld-rtl-dir/wile-rtl2-000054.scm:14");
}
return var_11;
}
// end of function wile_sql_meta_tables
