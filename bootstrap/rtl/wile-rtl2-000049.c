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

// @@@ (string-trim drop? str) @@@ bld-rtl-dir/wile-rtl2-000049.scm:13 @@@ wile_string_trim @@@
lval wile_string_trim(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
{
lval var_5[8];
var_5[0] = var_2[0];
var_5[1] = var_2[1];
var_4 = wile_string_trim_right(NULL, var_5, "bld-rtl-dir/wile-rtl2-000049.scm:14");
}
lval var_6;
{
lval var_7[8];
var_7[0] = var_2[0];
var_7[1] = var_4;
var_6 = wile_string_trim_left(NULL, var_7, "bld-rtl-dir/wile-rtl2-000049.scm:14");
}
return var_6;
}
// end of function wile_string_trim
