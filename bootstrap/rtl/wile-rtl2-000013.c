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

// @@@ (list-untail lst n) @@@ bld-rtl-dir/wile-rtl2-000013.scm:13 @@@ wile_list_untail @@@
lval wile_list_untail(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
{
lval var_5[8];
var_5[0] = var_2[0];
// bld-rtl-dir/wile-rtl2-000013.scm:14
var_4 = wile_list_length(NULL, var_5, "bld-rtl-dir/wile-rtl2-000013.scm:14");
}
lval var_6;
var_6 = LVI_INT(var_4.v.iv - var_2[1].v.iv);
lval var_7;
{
lval var_8[8];
var_8[0] = var_2[0];
var_8[1] = var_6;
// bld-rtl-dir/wile-rtl2-000013.scm:14
var_7 = wile_list_head(NULL, var_8, "bld-rtl-dir/wile-rtl2-000013.scm:14");
}
return var_7;
}
// end of function wile_list_untail
