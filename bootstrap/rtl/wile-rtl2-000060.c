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

// @@@ (delta-dates y1 m1 d1 y2 m2 d2) @@@ bld-rtl-dir/wile-rtl2-000060.scm:15 @@@ wile_delta_dates @@@
lval wile_delta_dates(lptr* var_1, lptr var_2, const char* cloc)
{
lval var_4;
{
lval var_5[8];
var_5[0] = var_2[3];
var_5[1] = var_2[4];
var_5[2] = var_2[5];
// bld-rtl-dir/wile-rtl2-000060.scm:16
var_4 = wile_julian_day(NULL, var_5, "bld-rtl-dir/wile-rtl2-000060.scm:16");
}
lval var_6;
{
lval var_7[8];
var_7[0] = var_2[0];
var_7[1] = var_2[1];
var_7[2] = var_2[2];
// bld-rtl-dir/wile-rtl2-000060.scm:16
var_6 = wile_julian_day(NULL, var_7, "bld-rtl-dir/wile-rtl2-000060.scm:16");
}
lval var_8;
var_8 = LVI_INT(var_4.v.iv - var_6.v.iv);
return var_8;
}
// end of function wile_delta_dates
