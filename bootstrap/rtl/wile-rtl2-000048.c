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

// @@@ (string-trim-right drop? str) @@@ bld-rtl-dir/wile-rtl2-000048.scm:13 @@@ wile_string_trim_right @@@
lval wile_string_trim_right(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
{
lval var_5[8];
var_5[0] = var_2[1];
var_4 = wile_string_reverse(NULL, var_5, "bld-rtl-dir/wile-rtl2-000048.scm:14");
}
lval var_6;
{
lval var_7[8];
var_7[0] = var_2[0];
var_7[1] = var_4;
var_6 = wile_string_trim_left(NULL, var_7, "bld-rtl-dir/wile-rtl2-000048.scm:14");
}
lval var_8;
{
lval var_9[8];
var_9[0] = var_6;
var_8 = wile_string_reverse(NULL, var_9, "bld-rtl-dir/wile-rtl2-000048.scm:14");
}
return var_8;
}
// end of function wile_string_trim_right
