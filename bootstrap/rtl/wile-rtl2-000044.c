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

// @@@ (string-trim drop? str) @@@ bld-rtl-dir/wile-rtl2-000044.scm:13 @@@ wile_string_trim @@@
lval wile_string_trim(lptr* var_1, lptr var_2)
{
lval var_4;
{
lval vs[8];
vs[0] = var_2[0];
vs[1] = var_2[1];
var_4 = wile_string_trim_right(NULL, vs);
}
lval var_5;
{
lval vs[8];
vs[0] = var_2[0];
vs[1] = var_4;
var_5 = wile_string_trim_left(NULL, vs);
}
return var_5;
}
// end of function wile_string_trim
