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

// @@@ (list-length<? n lst) @@@ bld-rtl-dir/wile-rtl2-000005.scm:13 @@@ wile_list_length_lt @@@
lval wile_list_length_lt(lptr* var_1, lptr var_2)
{
lval var_4;
{
lval var_5[8];
var_5[0] = var_2[0];
var_5[1] = var_2[1];
var_4 = wile_list_length_ge(NULL, var_5);
}
lval var_6;
var_6 = LVI_BOOL(LV_IS_FALSE(var_4));
return var_6;
}
// end of function wile_list_length_lt
