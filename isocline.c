/*
The following is an amalgamation of the isocline.[ch] code by Daan
Leijen, with a couple of tiny edits. The original is available at
https://github.com/daanx/isocline. The amalgamation here is included
in wile with Daan's permission, for which many thanks!
*/

/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

//-------------------------------------------------------------
// Usually we include all sources one file so no internal 
// symbols are public in the libray.
// 
// You can compile the entire library just as: 
// $ gcc -c src/isocline.c 
//-------------------------------------------------------------
#if !defined(IC_SEPARATE_OBJS)
# ifndef _CRT_NONSTDC_NO_WARNINGS
#  define _CRT_NONSTDC_NO_WARNINGS // for msvc
# endif
# ifndef _CRT_SECURE_NO_WARNINGS
#  define _CRT_SECURE_NO_WARNINGS  // for msvc
# endif
# define _XOPEN_SOURCE   700      // for wcwidth
//uh: protect since I already #define this
#ifndef _DEFAULT_SOURCE
# define _DEFAULT_SOURCE          // ensure usleep stays visible with _XOPEN_SOURCE >= 700
#endif // _DEFAULT_SOURCE
// # include "attr.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>

// #include "common.h"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

// #pragma once
#ifndef IC_COMMON_H
#define IC_COMMON_H

//-------------------------------------------------------------
// Headers and defines
//-------------------------------------------------------------

#include <sys/types.h>  // ssize_t
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
//uh: remove path ../include
#include "isocline.h"  // ic_malloc_fun_t, ic_color_t etc.

# ifdef __cplusplus
#  define ic_extern_c   extern "C"
# else
#  define ic_extern_c
# endif

#if defined(IC_SEPARATE_OBJS)
#  define ic_public     ic_extern_c 
# if defined(__GNUC__) // includes clang and icc      
#  define ic_private    __attribute__((visibility("hidden")))
# else
#  define ic_private  
# endif
#else
# define ic_private     static
# define ic_public      ic_extern_c
#endif

#define ic_unused(x)    (void)(x)


//-------------------------------------------------------------
// ssize_t
//-------------------------------------------------------------

#if defined(_MSC_VER)
typedef intptr_t ssize_t;
#endif

#define ssizeof(tp)   (ssize_t)(sizeof(tp))
static inline size_t  to_size_t(ssize_t sz) { return (sz >= 0 ? (size_t)sz : 0); }
static inline ssize_t to_ssize_t(size_t sz) { return (sz <= SIZE_MAX/2 ? (ssize_t)sz : 0); }

ic_private void    ic_memmove(void* dest, const void* src, ssize_t n);
ic_private void    ic_memcpy(void* dest, const void* src, ssize_t n);
ic_private void    ic_memset(void* dest, uint8_t value, ssize_t n);
ic_private bool    ic_memnmove(void* dest, ssize_t dest_size, const void* src, ssize_t n);

ic_private ssize_t ic_strlen(const char* s);
ic_private bool    ic_strcpy(char* dest, ssize_t dest_size /* including 0 */, const char* src);
ic_private bool    ic_strncpy(char* dest, ssize_t dest_size /* including 0 */, const char* src, ssize_t n);

ic_private bool    ic_contains(const char* big, const char* s);
ic_private bool    ic_icontains(const char* big, const char* s);
ic_private char    ic_tolower(char c);
ic_private void    ic_str_tolower(char* s);
ic_private int     ic_stricmp(const char* s1, const char* s2);
ic_private int     ic_strnicmp(const char* s1, const char* s2, ssize_t n);



//---------------------------------------------------------------------
// Unicode
//
// We use "qutf-8" (quite like utf-8) encoding and decoding. 
// Internally we always use valid utf-8. If we encounter invalid
// utf-8 bytes (or bytes >= 0x80 from any other encoding) we encode
// these as special code points in the "raw plane" (0xEE000 - 0xEE0FF).
// When decoding we are then able to restore such raw bytes as-is.
// See <https://github.com/koka-lang/koka/blob/master/kklib/include/kklib/string.h>
//---------------------------------------------------------------------

typedef uint32_t  unicode_t;

ic_private void      unicode_to_qutf8(unicode_t u, uint8_t buf[5]);
ic_private unicode_t unicode_from_qutf8(const uint8_t* s, ssize_t len, ssize_t* nread); // validating

ic_private unicode_t unicode_from_raw(uint8_t c);
ic_private bool      unicode_is_raw(unicode_t u, uint8_t* c);

ic_private bool      utf8_is_cont(uint8_t c);


//-------------------------------------------------------------
// Colors
//-------------------------------------------------------------

// A color is either RGB or an ANSI code.
// (RGB colors have bit 24 set to distinguish them from the ANSI color palette colors.)
// (Isocline will automatically convert from RGB on terminals that do not support full colors)
typedef uint32_t ic_color_t;

// Create a color from a 24-bit color value.
ic_private ic_color_t ic_rgb(uint32_t hex);

// Create a color from a 8-bit red/green/blue components.
// The value of each component is capped between 0 and 255.
ic_private ic_color_t ic_rgbx(ssize_t r, ssize_t g, ssize_t b);

#define IC_COLOR_NONE     (0)
#define IC_RGB(rgb)       (0x1000000 | (uint32_t)(rgb)) // ic_rgb(rgb)  // define to it can be used as a constant

// ANSI colors.
// The actual colors used is usually determined by the terminal theme
// See <https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit>
#define IC_ANSI_BLACK     (30)
#define IC_ANSI_MAROON    (31)
#define IC_ANSI_GREEN     (32)
#define IC_ANSI_OLIVE     (33)
#define IC_ANSI_NAVY      (34)
#define IC_ANSI_PURPLE    (35)
#define IC_ANSI_TEAL      (36)
#define IC_ANSI_SILVER    (37)
#define IC_ANSI_DEFAULT   (39)

#define IC_ANSI_GRAY      (90)
#define IC_ANSI_RED       (91)
#define IC_ANSI_LIME      (92)
#define IC_ANSI_YELLOW    (93)
#define IC_ANSI_BLUE      (94)
#define IC_ANSI_FUCHSIA   (95)
#define IC_ANSI_AQUA      (96)
#define IC_ANSI_WHITE     (97)

#define IC_ANSI_DARKGRAY  IC_ANSI_GRAY
#define IC_ANSI_LIGHTGRAY IC_ANSI_SILVER
#define IC_ANSI_MAGENTA   IC_ANSI_FUCHSIA
#define IC_ANSI_CYAN      IC_ANSI_AQUA



//-------------------------------------------------------------
// Debug
//-------------------------------------------------------------

#if defined(IC_NO_DEBUG_MSG) 
#define debug_msg(fmt,...)   (void)(0)
#else
ic_private void debug_msg( const char* fmt, ... );
#endif


//-------------------------------------------------------------
// Abstract environment
//-------------------------------------------------------------
struct ic_env_s;
typedef struct ic_env_s ic_env_t;


//-------------------------------------------------------------
// Allocation
//-------------------------------------------------------------

typedef struct alloc_s {
  ic_malloc_fun_t*  malloc;
  ic_realloc_fun_t* realloc;
  ic_free_fun_t*    free;
} alloc_t;


ic_private void* mem_malloc( alloc_t* mem, ssize_t sz );
ic_private void* mem_zalloc( alloc_t* mem, ssize_t sz );
ic_private void* mem_realloc( alloc_t* mem, void* p, ssize_t newsz );
ic_private void  mem_free( alloc_t* mem, const void* p );
ic_private char* mem_strdup( alloc_t* mem, const char* s);
ic_private char* mem_strndup( alloc_t* mem, const char* s, ssize_t n);

#define mem_zalloc_tp(mem,tp)        (tp*)mem_zalloc(mem,ssizeof(tp))
#define mem_malloc_tp_n(mem,tp,n)    (tp*)mem_malloc(mem,(n)*ssizeof(tp))
#define mem_zalloc_tp_n(mem,tp,n)    (tp*)mem_zalloc(mem,(n)*ssizeof(tp))
#define mem_realloc_tp(mem,tp,p,n)   (tp*)mem_realloc(mem,p,(n)*ssizeof(tp))


#endif // IC_COMMON_H
// #include "stringbuf.h" // str_next_ofs
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
// #pragma once
#ifndef IC_STRINGBUF_H
#define IC_STRINGBUF_H

#include <stdarg.h>
// skipping dup #include "common.h"

//-------------------------------------------------------------
// string buffer
// in-place modified buffer with edit operations 
// that grows on demand.
//-------------------------------------------------------------

// abstract string buffer
struct stringbuf_s;
typedef struct stringbuf_s stringbuf_t;

ic_private stringbuf_t*  sbuf_new( alloc_t* mem );
ic_private void    sbuf_free( stringbuf_t* sbuf );
ic_private char*   sbuf_free_dup(stringbuf_t* sbuf);
ic_private ssize_t sbuf_len(const stringbuf_t* s);

ic_private const char* sbuf_string_at( stringbuf_t* sbuf, ssize_t pos );
ic_private const char* sbuf_string( stringbuf_t* sbuf );
ic_private char    sbuf_char_at(stringbuf_t* sbuf, ssize_t pos);
ic_private char*   sbuf_strdup_at( stringbuf_t* sbuf, ssize_t pos );
ic_private char*   sbuf_strdup( stringbuf_t* sbuf );
ic_private char*   sbuf_strdup_from_utf8(stringbuf_t* sbuf);  // decode to locale


ic_private ssize_t sbuf_appendf(stringbuf_t* sb, const char* fmt, ...);
ic_private ssize_t sbuf_append_vprintf(stringbuf_t* sb, const char* fmt, va_list args);

ic_private stringbuf_t* sbuf_split_at( stringbuf_t* sb, ssize_t pos );

// primitive edit operations (inserts return the new position)
ic_private void    sbuf_clear(stringbuf_t* sbuf);
ic_private void    sbuf_replace(stringbuf_t* sbuf, const char* s);
ic_private void    sbuf_delete_at(stringbuf_t* sbuf, ssize_t pos, ssize_t count);
ic_private void    sbuf_delete_from_to(stringbuf_t* sbuf, ssize_t pos, ssize_t end);
ic_private void    sbuf_delete_from(stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_insert_at_n(stringbuf_t* sbuf, const char* s, ssize_t n, ssize_t pos );
ic_private ssize_t sbuf_insert_at(stringbuf_t* sbuf, const char* s, ssize_t pos );
ic_private ssize_t sbuf_insert_char_at(stringbuf_t* sbuf, char c, ssize_t pos );
ic_private ssize_t sbuf_insert_unicode_at(stringbuf_t* sbuf, unicode_t u, ssize_t pos);
ic_private ssize_t sbuf_append_n(stringbuf_t* sbuf, const char* s, ssize_t n);
ic_private ssize_t sbuf_append(stringbuf_t* sbuf, const char* s);
ic_private ssize_t sbuf_append_char(stringbuf_t* sbuf, char c);

// high level edit operations (return the new position)
ic_private ssize_t sbuf_next( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth );
ic_private ssize_t sbuf_prev( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth );
ic_private ssize_t sbuf_next_ofs(stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth);

ic_private ssize_t sbuf_delete_char_before( stringbuf_t* sbuf, ssize_t pos );
ic_private void    sbuf_delete_char_at( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_swap_char( stringbuf_t* sbuf, ssize_t pos );

ic_private ssize_t sbuf_find_line_start( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_find_line_end( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_find_word_start( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_find_word_end( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_find_ws_word_start( stringbuf_t* sbuf, ssize_t pos );
ic_private ssize_t sbuf_find_ws_word_end( stringbuf_t* sbuf, ssize_t pos );

// parse a decimal 
ic_private bool ic_atoz(const char* s, ssize_t* i);
// parse two decimals separated by a semicolon
ic_private bool ic_atoz2(const char* s, ssize_t* i, ssize_t* j);
ic_private bool ic_atou32(const char* s, uint32_t* pu);

// row/column info
typedef struct rowcol_s {
  ssize_t row;
  ssize_t col;
  ssize_t row_start;
  ssize_t row_len;
  bool    first_on_row;
  bool    last_on_row;
} rowcol_t;

// find row/col position
ic_private ssize_t sbuf_get_pos_at_rc( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t cpromptw, 
                                       ssize_t row, ssize_t col );
// get row/col for a given position
ic_private ssize_t sbuf_get_rc_at_pos( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t cpromptw, 
                                       ssize_t pos, rowcol_t* rc );

ic_private ssize_t sbuf_get_wrapped_rc_at_pos( stringbuf_t* sbuf, ssize_t termw, ssize_t newtermw, ssize_t promptw, ssize_t cpromptw, 
                                       ssize_t pos, rowcol_t* rc );
                                       
// row iteration
typedef bool (row_fun_t)(const char* s,
                          ssize_t row, ssize_t row_start, ssize_t row_len, 
                          ssize_t startw, // prompt width
                          bool is_wrap, const void* arg, void* res);

ic_private ssize_t sbuf_for_each_row( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t cpromptw, 
                                      row_fun_t* fun, void* arg, void* res );


//-------------------------------------------------------------
// Strings
//-------------------------------------------------------------

// skip a single CSI sequence (ESC [ ...)
ic_private bool    skip_csi_esc( const char* s, ssize_t len, ssize_t* esclen ); // used in term.c

ic_private ssize_t str_column_width( const char* s );
ic_private ssize_t str_prev_ofs( const char* s, ssize_t pos, ssize_t* cwidth );
ic_private ssize_t str_next_ofs( const char* s, ssize_t len, ssize_t pos, ssize_t* cwidth );
ic_private ssize_t str_skip_until_fit( const char* s, ssize_t max_width);  // tail that fits
ic_private ssize_t str_take_while_fit( const char* s, ssize_t max_width);  // prefix that fits

#endif // IC_STRINGBUF_H
// #include "attr.h"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
// #pragma once
#ifndef IC_ATTR_H
#define IC_ATTR_H

// skipping dup #include "common.h"
// skipping dup #include "stringbuf.h"

//-------------------------------------------------------------
// text attributes
//-------------------------------------------------------------

#define IC_ON   (1)
#define IC_OFF  (-1)
#define IC_NONE (0)

// try to fit in 64 bits 
// note: order is important for some compilers
// note: each color can actually be 25 bits
typedef union attr_s {
  struct {
    unsigned int  color:28;
    signed int    bold:2;
    signed int    reverse:2;
    unsigned int  bgcolor:28;
    signed int    underline:2;
    signed int    italic:2;
  } x;
  uint64_t        value;
} attr_t;

ic_private attr_t attr_none(void);
ic_private attr_t attr_default(void);
ic_private attr_t attr_from_color( ic_color_t color );

ic_private bool attr_is_none(attr_t attr);
ic_private bool attr_is_eq(attr_t attr1, attr_t attr2);

ic_private attr_t attr_update_with( attr_t attr, attr_t newattr );

ic_private attr_t attr_from_sgr( const char* s, ssize_t len);
ic_private attr_t attr_from_esc_sgr( const char* s, ssize_t len);

//-------------------------------------------------------------
// attribute buffer used for rich rendering
//-------------------------------------------------------------

struct attrbuf_s;
typedef struct attrbuf_s attrbuf_t;

ic_private attrbuf_t*     attrbuf_new( alloc_t* mem );
ic_private void           attrbuf_free( attrbuf_t* ab );  // ab can be NULL
ic_private void           attrbuf_clear( attrbuf_t* ab ); // ab can be NULL
ic_private ssize_t        attrbuf_len( attrbuf_t* ab);    // ab can be NULL
ic_private const attr_t*  attrbuf_attrs( attrbuf_t* ab, ssize_t expected_len );
ic_private ssize_t        attrbuf_append_n( stringbuf_t* sb, attrbuf_t* ab, const char* s, ssize_t len, attr_t attr );

ic_private void           attrbuf_set_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr );
ic_private void           attrbuf_update_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr );
ic_private void           attrbuf_insert_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr );

ic_private attr_t         attrbuf_attr_at( attrbuf_t* ab, ssize_t pos );   
ic_private void           attrbuf_delete_at( attrbuf_t* ab, ssize_t pos, ssize_t count );

#endif // IC_ATTR_H
// #include "term.h"      // color_from_ansi256
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
// #pragma once
#ifndef IC_TERM_H
#define IC_TERM_H

// skipping dup #include "common.h"
// #include "tty.h"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
// #pragma once
#ifndef IC_TTY_H
#define IC_TTY_H

// skipping dup #include "common.h"

//-------------------------------------------------------------
// TTY/Keyboard input 
//-------------------------------------------------------------

// Key code
typedef uint32_t  code_t;

// TTY interface
struct tty_s;
typedef struct tty_s tty_t;


ic_private tty_t* tty_new(alloc_t* mem, int fd_in);
ic_private void   tty_free(tty_t* tty);

ic_private bool   tty_is_utf8(const tty_t* tty);
ic_private bool   tty_start_raw(tty_t* tty);
ic_private void   tty_end_raw(tty_t* tty);
ic_private code_t tty_read(tty_t* tty);
ic_private bool   tty_read_timeout(tty_t* tty, long timeout_ms, code_t* c );

ic_private void   tty_code_pushback( tty_t* tty, code_t c );
ic_private bool   code_is_ascii_char(code_t c, char* chr );
ic_private bool   code_is_unicode(code_t c, unicode_t* uchr);
ic_private bool   code_is_virt_key(code_t c );

ic_private bool   tty_term_resize_event(tty_t* tty); // did the terminal resize?
ic_private bool   tty_async_stop(const tty_t* tty);  // unblock the read asynchronously
ic_private void   tty_set_esc_delay(tty_t* tty, long initial_delay_ms, long followup_delay_ms);

// shared between tty.c and tty_esc.c: low level character push
ic_private void   tty_cpush_char(tty_t* tty, uint8_t c);
ic_private bool   tty_cpop(tty_t* tty, uint8_t* c);
ic_private bool   tty_readc_noblock(tty_t* tty, uint8_t* c, long timeout_ms);
ic_private code_t tty_read_esc(tty_t* tty, long esc_initial_timeout, long esc_timeout); // in tty_esc.c

// used by term.c to read back ANSI escape responses
ic_private bool   tty_read_esc_response(tty_t* tty, char esc_start, bool final_st, char* buf, ssize_t buflen ); 


//-------------------------------------------------------------
// Key codes: a code_t is 32 bits.
// we use the bottom 24 (nah, 21) bits for unicode (up to x0010FFFF)
// The codes after x01000000 are for virtual keys 
// and events use  x02000000.
// The top 4 bits are used for modifiers.
//-------------------------------------------------------------

static inline code_t key_char( char c ) {
  // careful about signed character conversion (negative char ~> 0x80 - 0xFF)
  return ((uint8_t)c);
}

static inline code_t key_unicode( unicode_t u ) {
  return u;
}


#define KEY_MOD_SHIFT     (0x10000000U)
#define KEY_MOD_ALT       (0x20000000U)
#define KEY_MOD_CTRL      (0x40000000U)

#define KEY_NO_MODS(k)    (k & 0x0FFFFFFFU)
#define KEY_MODS(k)       (k & 0xF0000000U)

#define WITH_SHIFT(x)     (x | KEY_MOD_SHIFT)
#define WITH_ALT(x)       (x | KEY_MOD_ALT)
#define WITH_CTRL(x)      (x | KEY_MOD_CTRL)

#define KEY_NONE          (0)
#define KEY_CTRL_A        (1)
#define KEY_CTRL_B        (2)
#define KEY_CTRL_C        (3)
#define KEY_CTRL_D        (4)
#define KEY_CTRL_E        (5)
#define KEY_CTRL_F        (6)
#define KEY_BELL          (7)
#define KEY_BACKSP        (8)
#define KEY_TAB           (9)
#define KEY_LINEFEED      (10)   // ctrl/shift + enter is considered KEY_LINEFEED
#define KEY_CTRL_K        (11)
#define KEY_CTRL_L        (12)
#define KEY_ENTER         (13)
#define KEY_CTRL_N        (14)
#define KEY_CTRL_O        (15)
#define KEY_CTRL_P        (16)
#define KEY_CTRL_Q        (17)
#define KEY_CTRL_R        (18)
#define KEY_CTRL_S        (19)
#define KEY_CTRL_T        (20)
#define KEY_CTRL_U        (21)
#define KEY_CTRL_V        (22)
#define KEY_CTRL_W        (23)
#define KEY_CTRL_X        (24)
#define KEY_CTRL_Y        (25)
#define KEY_CTRL_Z        (26)
#define KEY_ESC           (27)
#define KEY_SPACE         (32)
#define KEY_RUBOUT        (127)  // always translated to KEY_BACKSP
#define KEY_UNICODE_MAX   (0x0010FFFFU)


#define KEY_VIRT          (0x01000000U)  
#define KEY_UP            (KEY_VIRT+0)
#define KEY_DOWN          (KEY_VIRT+1)
#define KEY_LEFT          (KEY_VIRT+2)
#define KEY_RIGHT         (KEY_VIRT+3)
#define KEY_HOME          (KEY_VIRT+4)
#define KEY_END           (KEY_VIRT+5)
#define KEY_DEL           (KEY_VIRT+6)
#define KEY_PAGEUP        (KEY_VIRT+7)
#define KEY_PAGEDOWN      (KEY_VIRT+8)
#define KEY_INS           (KEY_VIRT+9)

#define KEY_F1            (KEY_VIRT+11)
#define KEY_F2            (KEY_VIRT+12)
#define KEY_F3            (KEY_VIRT+13)
#define KEY_F4            (KEY_VIRT+14)
#define KEY_F5            (KEY_VIRT+15)
#define KEY_F6            (KEY_VIRT+16)
#define KEY_F7            (KEY_VIRT+17)
#define KEY_F8            (KEY_VIRT+18)
#define KEY_F9            (KEY_VIRT+19)
#define KEY_F10           (KEY_VIRT+20)
#define KEY_F11           (KEY_VIRT+21)
#define KEY_F12           (KEY_VIRT+22)
#define KEY_F(n)          (KEY_F1 + (n) - 1)

#define KEY_EVENT_BASE    (0x02000000U)
#define KEY_EVENT_RESIZE  (KEY_EVENT_BASE+1)
#define KEY_EVENT_AUTOTAB (KEY_EVENT_BASE+2)
#define KEY_EVENT_STOP    (KEY_EVENT_BASE+3)

// Convenience
#define KEY_CTRL_UP       (WITH_CTRL(KEY_UP))
#define KEY_CTRL_DOWN     (WITH_CTRL(KEY_DOWN))
#define KEY_CTRL_LEFT     (WITH_CTRL(KEY_LEFT))
#define KEY_CTRL_RIGHT    (WITH_CTRL(KEY_RIGHT))
#define KEY_CTRL_HOME     (WITH_CTRL(KEY_HOME))
#define KEY_CTRL_END      (WITH_CTRL(KEY_END))
#define KEY_CTRL_DEL      (WITH_CTRL(KEY_DEL))
#define KEY_CTRL_PAGEUP   (WITH_CTRL(KEY_PAGEUP))
#define KEY_CTRL_PAGEDOWN (WITH_CTRL(KEY_PAGEDOWN)))
#define KEY_CTRL_INS      (WITH_CTRL(KEY_INS))

#define KEY_SHIFT_TAB     (WITH_SHIFT(KEY_TAB))

#endif // IC_TTY_H
// skipping dup #include "stringbuf.h"
// skipping dup #include "attr.h"

struct term_s;
typedef struct term_s term_t;

typedef enum buffer_mode_e {
  UNBUFFERED,
  LINEBUFFERED,
  BUFFERED,
} buffer_mode_t;

// Primitives
ic_private term_t* term_new(alloc_t* mem, tty_t* tty, bool nocolor, bool silent, int fd_out);
ic_private void term_free(term_t* term);

ic_private bool term_is_interactive(const term_t* term);
ic_private void term_start_raw(term_t* term);
ic_private void term_end_raw(term_t* term, bool force);

ic_private bool term_enable_beep(term_t* term, bool enable);
ic_private bool term_enable_color(term_t* term, bool enable);

ic_private void term_flush(term_t* term);
ic_private buffer_mode_t term_set_buffer_mode(term_t* term, buffer_mode_t mode);

ic_private void term_write_n(term_t* term, const char* s, ssize_t n);
ic_private void term_write(term_t* term, const char* s);
ic_private void term_writeln(term_t* term, const char* s);
ic_private void term_write_char(term_t* term, char c);

ic_private void term_write_repeat(term_t* term, const char* s, ssize_t count );
ic_private void term_beep(term_t* term);

ic_private bool term_update_dim(term_t* term);

ic_private ssize_t term_get_width(term_t* term);
ic_private ssize_t term_get_height(term_t* term);
ic_private int  term_get_color_bits(term_t* term);

// Helpers
ic_private void term_writef(term_t* term, const char* fmt, ...);
ic_private void term_vwritef(term_t* term, const char* fmt, va_list args);

ic_private void term_left(term_t* term, ssize_t n);
ic_private void term_right(term_t* term, ssize_t n);
ic_private void term_up(term_t* term, ssize_t n);
ic_private void term_down(term_t* term, ssize_t n);
ic_private void term_start_of_line(term_t* term );
ic_private void term_clear_line(term_t* term);
ic_private void term_clear_to_end_of_line(term_t* term);
// ic_private void term_clear_lines_to_end(term_t* term);


ic_private void term_attr_reset(term_t* term);
ic_private void term_underline(term_t* term, bool on);
ic_private void term_reverse(term_t* term, bool on);
ic_private void term_bold(term_t* term, bool on);
ic_private void term_italic(term_t* term, bool on);

ic_private void term_color(term_t* term, ic_color_t color);
ic_private void term_bgcolor(term_t* term, ic_color_t color);

// Formatted output

ic_private attr_t term_get_attr( const term_t* term );
ic_private void   term_set_attr( term_t* term, attr_t attr );
ic_private void   term_write_formatted( term_t* term, const char* s, const attr_t* attrs );
ic_private void   term_write_formatted_n( term_t* term, const char* s, const attr_t* attrs, ssize_t n );

ic_private ic_color_t color_from_ansi256(ssize_t i);

#endif // IC_TERM_H

//-------------------------------------------------------------
// Attributes
//-------------------------------------------------------------

ic_private attr_t attr_none(void) {
  attr_t attr;
  attr.value = 0;
  return attr;
}

ic_private attr_t attr_default(void) {
  attr_t attr = attr_none();
  attr.x.color = IC_ANSI_DEFAULT;
  attr.x.bgcolor = IC_ANSI_DEFAULT;
  attr.x.bold = IC_OFF;
  attr.x.underline = IC_OFF; 
  attr.x.reverse = IC_OFF;
  attr.x.italic = IC_OFF; 
  return attr;
}

ic_private bool attr_is_none(attr_t attr) {
  return (attr.value == 0);
}

ic_private bool attr_is_eq(attr_t attr1, attr_t attr2) {
  return (attr1.value == attr2.value);
}

ic_private attr_t attr_from_color( ic_color_t color ) {
  attr_t attr = attr_none();
  attr.x.color = color;
  return attr;
}


ic_private attr_t attr_update_with( attr_t oldattr, attr_t newattr ) {
  attr_t attr = oldattr;
  if (newattr.x.color != IC_COLOR_NONE)   { attr.x.color = newattr.x.color; }
  if (newattr.x.bgcolor != IC_COLOR_NONE) { attr.x.bgcolor = newattr.x.bgcolor; }
  if (newattr.x.bold != IC_NONE)          { attr.x.bold = newattr.x.bold; }
  if (newattr.x.italic != IC_NONE)        { attr.x.italic = newattr.x.italic; }
  if (newattr.x.reverse != IC_NONE)       { attr.x.reverse = newattr.x.reverse; }
  if (newattr.x.underline != IC_NONE)     { attr.x.underline = newattr.x.underline; }
  return attr;
}

static bool sgr_is_digit(char c) {
  return (c >= '0' && c <= '9');
}

static bool sgr_is_sep( char c ) {
  return (c==';' || c==':');
}

static bool sgr_next_par(const char* s, ssize_t* pi, ssize_t* par) {
  const ssize_t i = *pi;
  ssize_t n = 0;
  while( sgr_is_digit(s[i+n])) { 
    n++; 
  }
  if (n==0) { 
    *par = 0;
    return true;
  }
  else {
    *pi = i+n;
    return ic_atoz(s+i, par);    
  }
}

static bool sgr_next_par3(const char* s, ssize_t* pi, ssize_t* p1, ssize_t* p2, ssize_t* p3) {
  bool ok = false;
  ssize_t i = *pi;
  if (sgr_next_par(s,&i,p1) && sgr_is_sep(s[i])) {
    i++;
    if (sgr_next_par(s,&i,p2) && sgr_is_sep(s[i])) {
      i++;
      if (sgr_next_par(s,&i,p3)) {
        ok = true;
      };
    }
  }
  *pi = i;
  return ok;
}

ic_private attr_t attr_from_sgr( const char* s, ssize_t len) {
  attr_t attr = attr_none();
  for( ssize_t i = 0; i < len && s[i] != 0; i++) {
    ssize_t cmd = 0;
    if (!sgr_next_par(s,&i,&cmd)) continue;
    switch(cmd) {
      case  0: attr = attr_default(); break;
      case  1: attr.x.bold = IC_ON; break;
      case  3: attr.x.italic = IC_ON; break;
      case  4: attr.x.underline = IC_ON; break;
      case  7: attr.x.reverse = IC_ON; break;
      case 22: attr.x.bold = IC_OFF; break;
      case 23: attr.x.italic = IC_OFF; break;
      case 24: attr.x.underline = IC_OFF; break;
      case 27: attr.x.reverse = IC_OFF; break;
      case 39: attr.x.color = IC_ANSI_DEFAULT; break;
      case 49: attr.x.bgcolor = IC_ANSI_DEFAULT; break;
      default: {
        if (cmd >= 30 && cmd <= 37) {
          attr.x.color = IC_ANSI_BLACK + (unsigned)(cmd - 30);
        }
        else if (cmd >= 40 && cmd <= 47) {
          attr.x.bgcolor = IC_ANSI_BLACK + (unsigned)(cmd - 40);
        }
        else if (cmd >= 90 && cmd <= 97) {
          attr.x.color = IC_ANSI_DARKGRAY + (unsigned)(cmd - 90);
        }
        else if (cmd >= 100 && cmd <= 107) {
          attr.x.bgcolor = IC_ANSI_DARKGRAY + (unsigned)(cmd - 100);
        }
        else if ((cmd == 38 || cmd == 48) && sgr_is_sep(s[i])) {
          // non-associative SGR :-(          
          ssize_t par = 0;
          i++;
          if (sgr_next_par(s, &i, &par)) {
            if (par==5 && sgr_is_sep(s[i])) {
              // ansi 256 index
              i++;
              if (sgr_next_par(s, &i, &par) && par >= 0 && par <= 0xFF) {
                ic_color_t color = color_from_ansi256(par);
                if (cmd==38) { attr.x.color = color; }
                        else { attr.x.bgcolor = color; }
              }
            }
            else if (par == 2 && sgr_is_sep(s[i])) {
              // rgb value
              i++;
              ssize_t r,g,b;
              if (sgr_next_par3(s, &i, &r,&g,&b)) {
                ic_color_t color = ic_rgbx(r,g,b);
                if (cmd==38) { attr.x.color = color; }
                        else { attr.x.bgcolor = color; }
              }
            }
          }
        }
        else {
          debug_msg("attr: unknow ANSI SGR code: %zd\n", cmd );
        }
      }
    }
  }
  return attr;
}

ic_private attr_t attr_from_esc_sgr( const char* s, ssize_t len) {
  if (len <= 2 || s[0] != '\x1B' || s[1] != '[' || s[len-1] != 'm') return attr_none();
  return attr_from_sgr(s+2, len-2);
}


//-------------------------------------------------------------
// Attribute buffer
//-------------------------------------------------------------
struct attrbuf_s {
  attr_t*  attrs;
  ssize_t  capacity;
  ssize_t  count;
  alloc_t* mem;
};

static bool attrbuf_ensure_capacity( attrbuf_t* ab, ssize_t needed ) {
  if (needed <= ab->capacity) return true;
  ssize_t newcap = (ab->capacity <= 0 ? 240 : (ab->capacity > 1000 ? ab->capacity + 1000 : 2*ab->capacity));
  if (needed > newcap) { newcap = needed; }
  attr_t* newattrs = mem_realloc_tp( ab->mem, attr_t, ab->attrs, newcap );
  if (newattrs == NULL) return false;
  ab->attrs = newattrs;
  ab->capacity = newcap;
  assert(needed <= ab->capacity);
  return true;
}

static bool attrbuf_ensure_extra( attrbuf_t* ab, ssize_t extra ) {
  const ssize_t needed = ab->count + extra;
  return attrbuf_ensure_capacity( ab, needed );
}


ic_private attrbuf_t* attrbuf_new( alloc_t* mem ) {
  attrbuf_t* ab = mem_zalloc_tp(mem,attrbuf_t);
  if (ab == NULL) return NULL;
  ab->mem = mem;
  attrbuf_ensure_extra(ab,1);
  return ab;
}

ic_private void attrbuf_free( attrbuf_t* ab ) {
  if (ab==NULL) return;
  mem_free(ab->mem, ab->attrs);
  mem_free(ab->mem, ab);
}

ic_private void attrbuf_clear(attrbuf_t* ab) {
  if (ab == NULL) return;
  ab->count = 0;
}

ic_private ssize_t attrbuf_len( attrbuf_t* ab ) {
  return (ab==NULL ? 0 : ab->count);
}

ic_private const attr_t* attrbuf_attrs( attrbuf_t* ab, ssize_t expected_len ) {
  assert(expected_len <= ab->count );
  // expand if needed
  if (ab->count < expected_len) {    
    if (!attrbuf_ensure_capacity(ab,expected_len)) return NULL;
    for(ssize_t i = ab->count; i < expected_len; i++) {
      ab->attrs[i] = attr_none();  
    }
    ab->count = expected_len;
  }
  return ab->attrs;
}



static void attrbuf_update_set_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr, bool update ) {
  const ssize_t end = pos + count;
  if (!attrbuf_ensure_capacity(ab, end)) return;
  ssize_t i;
  // initialize if end is beyond the count (todo: avoid duplicate init and set if update==false?)
  if (ab->count < end) {
    for(i = ab->count; i < end; i++) {
      ab->attrs[i] = attr_none();  
    }
    ab->count = end;
  }
  // fill pos to end with attr 
  for(i = pos; i < end; i++) {
    ab->attrs[i] = (update ? attr_update_with(ab->attrs[i],attr) : attr);    
  }  
}

ic_private void attrbuf_set_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr ) {
  attrbuf_update_set_at(ab, pos, count, attr, false);
}

ic_private void attrbuf_update_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr ) {
  attrbuf_update_set_at(ab, pos, count, attr, true);  
}

ic_private void attrbuf_insert_at( attrbuf_t* ab, ssize_t pos, ssize_t count, attr_t attr ) {
  if (pos < 0 || pos > ab->count || count <= 0) return;
  if (!attrbuf_ensure_extra(ab,count)) return;  
  ic_memmove( ab->attrs + pos + count, ab->attrs + pos, (ab->count - pos)*ssizeof(attr_t) );
  ab->count += count;
  attrbuf_set_at( ab, pos, count, attr );
}


// note: must allow ab == NULL!
ic_private ssize_t attrbuf_append_n( stringbuf_t* sb, attrbuf_t* ab, const char* s, ssize_t len, attr_t attr ) {
  if (s == NULL || len == 0) return sbuf_len(sb);
  if (ab != NULL) {
    if (!attrbuf_ensure_extra(ab,len)) return sbuf_len(sb);
    attrbuf_set_at(ab, ab->count, len, attr);
  }
  return sbuf_append_n(sb,s,len);
}

ic_private attr_t attrbuf_attr_at( attrbuf_t* ab, ssize_t pos ) {
  if (ab==NULL || pos < 0 || pos > ab->count) return attr_none();
  return ab->attrs[pos];
}

ic_private void attrbuf_delete_at( attrbuf_t* ab, ssize_t pos, ssize_t count ) {
  if (ab==NULL || pos < 0 || pos > ab->count) return;
  if (pos + count > ab->count) { count = ab->count - pos; }
  if (count == 0) return;
  assert(pos + count <= ab->count);
  ic_memmove( ab->attrs + pos, ab->attrs + pos + count, ab->count - (pos + count) );
  ab->count -= count;
}
// # include "bbcode.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>  

// skipping dup #include "common.h"
// skipping dup #include "attr.h"
// skipping dup #include "term.h"
// #include "bbcode.h" 
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
// #pragma once
#ifndef IC_BBCODE_H
#define IC_BBCODE_H

#include <stdarg.h>
// skipping dup #include "common.h"
// skipping dup #include "term.h"

struct bbcode_s;
typedef struct bbcode_s bbcode_t;

ic_private bbcode_t* bbcode_new( alloc_t* mem, term_t* term );
ic_private void bbcode_free( bbcode_t* bb );

ic_private void bbcode_style_add( bbcode_t* bb, const char* style_name, attr_t attr );
ic_private void bbcode_style_def( bbcode_t* bb, const char* style_name, const char* s );
ic_private void bbcode_style_open( bbcode_t* bb, const char* fmt );
ic_private void bbcode_style_close( bbcode_t* bb, const char* fmt );
ic_private attr_t bbcode_style( bbcode_t* bb, const char* style_name );

ic_private void bbcode_print( bbcode_t* bb, const char* s );
ic_private void bbcode_println( bbcode_t* bb, const char* s );
ic_private void bbcode_printf( bbcode_t* bb, const char* fmt, ... );
ic_private void bbcode_vprintf( bbcode_t* bb, const char* fmt, va_list args );

ic_private ssize_t bbcode_column_width( bbcode_t* bb, const char* s );

// allows `attr_out == NULL`.
ic_private void bbcode_append( bbcode_t* bb, const char* s, stringbuf_t* out, attrbuf_t* attr_out );

#endif // IC_BBCODE_H

//-------------------------------------------------------------
// HTML color table
//-------------------------------------------------------------

// #include "bbcode_colors.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

// This file is included from "bbcode.c" and contains html color names

// skipping dup #include "common.h"

typedef struct style_color_s {
  const char* name;
  ic_color_t color;
} style_color_t;

#define IC_HTML_COLOR_COUNT (172)

// ordered list of HTML color names (so we can use binary search)
static style_color_t html_colors[IC_HTML_COLOR_COUNT+1] = {
  { "aliceblue",      IC_RGB(0xf0f8ff) },
  { "ansi-aqua",      IC_ANSI_AQUA },
  { "ansi-black",     IC_ANSI_BLACK },
  { "ansi-blue",      IC_ANSI_BLUE },
  { "ansi-cyan",      IC_ANSI_CYAN },
  { "ansi-darkgray",  IC_ANSI_DARKGRAY },
  { "ansi-darkgrey",  IC_ANSI_DARKGRAY },
  { "ansi-default",   IC_ANSI_DEFAULT },
  { "ansi-fuchsia",   IC_ANSI_FUCHSIA },
  { "ansi-gray",      IC_ANSI_GRAY },
  { "ansi-green",     IC_ANSI_GREEN },
  { "ansi-grey",      IC_ANSI_GRAY },
  { "ansi-lightgray", IC_ANSI_LIGHTGRAY },
  { "ansi-lightgrey", IC_ANSI_LIGHTGRAY },
  { "ansi-lime" ,     IC_ANSI_LIME },
  { "ansi-magenta",   IC_ANSI_MAGENTA },
  { "ansi-maroon",    IC_ANSI_MAROON },
  { "ansi-navy",      IC_ANSI_NAVY },
  { "ansi-olive",     IC_ANSI_OLIVE },
  { "ansi-purple",    IC_ANSI_PURPLE },
  { "ansi-red",       IC_ANSI_RED },
  { "ansi-silver",    IC_ANSI_SILVER },
  { "ansi-teal",      IC_ANSI_TEAL },
  { "ansi-white",     IC_ANSI_WHITE },
  { "ansi-yellow",    IC_ANSI_YELLOW },
  { "antiquewhite",   IC_RGB(0xfaebd7) },
  { "aqua", IC_RGB(0x00ffff) },
  { "aquamarine", IC_RGB(0x7fffd4) },
  { "azure", IC_RGB(0xf0ffff) },
  { "beige", IC_RGB(0xf5f5dc) },
  { "bisque", IC_RGB(0xffe4c4) },
  { "black", IC_RGB(0x000000) },
  { "blanchedalmond", IC_RGB(0xffebcd) },
  { "blue", IC_RGB(0x0000ff) },
  { "blueviolet", IC_RGB(0x8a2be2) },
  { "brown", IC_RGB(0xa52a2a) },
  { "burlywood", IC_RGB(0xdeb887) },
  { "cadetblue", IC_RGB(0x5f9ea0) },
  { "chartreuse", IC_RGB(0x7fff00) },
  { "chocolate", IC_RGB(0xd2691e) },
  { "coral", IC_RGB(0xff7f50) },
  { "cornflowerblue", IC_RGB(0x6495ed) },
  { "cornsilk", IC_RGB(0xfff8dc) },
  { "crimson", IC_RGB(0xdc143c) },
  { "cyan", IC_RGB(0x00ffff) },
  { "darkblue", IC_RGB(0x00008b) },
  { "darkcyan", IC_RGB(0x008b8b) },
  { "darkgoldenrod", IC_RGB(0xb8860b) },
  { "darkgray", IC_RGB(0xa9a9a9) },
  { "darkgreen", IC_RGB(0x006400) },
  { "darkgrey", IC_RGB(0xa9a9a9) },
  { "darkkhaki", IC_RGB(0xbdb76b) },
  { "darkmagenta", IC_RGB(0x8b008b) },
  { "darkolivegreen", IC_RGB(0x556b2f) },
  { "darkorange", IC_RGB(0xff8c00) },
  { "darkorchid", IC_RGB(0x9932cc) },
  { "darkred", IC_RGB(0x8b0000) },
  { "darksalmon", IC_RGB(0xe9967a) },
  { "darkseagreen", IC_RGB(0x8fbc8f) },
  { "darkslateblue", IC_RGB(0x483d8b) },
  { "darkslategray", IC_RGB(0x2f4f4f) },
  { "darkslategrey", IC_RGB(0x2f4f4f) },
  { "darkturquoise", IC_RGB(0x00ced1) },
  { "darkviolet", IC_RGB(0x9400d3) },
  { "deeppink", IC_RGB(0xff1493) },
  { "deepskyblue", IC_RGB(0x00bfff) },
  { "dimgray", IC_RGB(0x696969) },
  { "dimgrey", IC_RGB(0x696969) },
  { "dodgerblue", IC_RGB(0x1e90ff) },
  { "firebrick", IC_RGB(0xb22222) },
  { "floralwhite", IC_RGB(0xfffaf0) },
  { "forestgreen", IC_RGB(0x228b22) },
  { "fuchsia", IC_RGB(0xff00ff) },
  { "gainsboro", IC_RGB(0xdcdcdc) },
  { "ghostwhite", IC_RGB(0xf8f8ff) },
  { "gold", IC_RGB(0xffd700) },
  { "goldenrod", IC_RGB(0xdaa520) },
  { "gray", IC_RGB(0x808080) },
  { "green", IC_RGB(0x008000) },
  { "greenyellow", IC_RGB(0xadff2f) },
  { "grey", IC_RGB(0x808080) },
  { "honeydew", IC_RGB(0xf0fff0) },
  { "hotpink", IC_RGB(0xff69b4) },
  { "indianred", IC_RGB(0xcd5c5c) },
  { "indigo", IC_RGB(0x4b0082) },
  { "ivory", IC_RGB(0xfffff0) },
  { "khaki", IC_RGB(0xf0e68c) },
  { "lavender", IC_RGB(0xe6e6fa) },
  { "lavenderblush", IC_RGB(0xfff0f5) },
  { "lawngreen", IC_RGB(0x7cfc00) },
  { "lemonchiffon", IC_RGB(0xfffacd) },
  { "lightblue", IC_RGB(0xadd8e6) },
  { "lightcoral", IC_RGB(0xf08080) },
  { "lightcyan", IC_RGB(0xe0ffff) },
  { "lightgoldenrodyellow", IC_RGB(0xfafad2) },
  { "lightgray", IC_RGB(0xd3d3d3) },
  { "lightgreen", IC_RGB(0x90ee90) },
  { "lightgrey", IC_RGB(0xd3d3d3) },
  { "lightpink", IC_RGB(0xffb6c1) },
  { "lightsalmon", IC_RGB(0xffa07a) },
  { "lightseagreen", IC_RGB(0x20b2aa) },
  { "lightskyblue", IC_RGB(0x87cefa) },
  { "lightslategray", IC_RGB(0x778899) },
  { "lightslategrey", IC_RGB(0x778899) },
  { "lightsteelblue", IC_RGB(0xb0c4de) },
  { "lightyellow", IC_RGB(0xffffe0) },
  { "lime", IC_RGB(0x00ff00) },
  { "limegreen", IC_RGB(0x32cd32) },
  { "linen", IC_RGB(0xfaf0e6) },
  { "magenta", IC_RGB(0xff00ff) },
  { "maroon", IC_RGB(0x800000) },
  { "mediumaquamarine", IC_RGB(0x66cdaa) },
  { "mediumblue", IC_RGB(0x0000cd) },
  { "mediumorchid", IC_RGB(0xba55d3) },
  { "mediumpurple", IC_RGB(0x9370db) },
  { "mediumseagreen", IC_RGB(0x3cb371) },
  { "mediumslateblue", IC_RGB(0x7b68ee) },
  { "mediumspringgreen", IC_RGB(0x00fa9a) },
  { "mediumturquoise", IC_RGB(0x48d1cc) },
  { "mediumvioletred", IC_RGB(0xc71585) },
  { "midnightblue", IC_RGB(0x191970) },
  { "mintcream", IC_RGB(0xf5fffa) },
  { "mistyrose", IC_RGB(0xffe4e1) },
  { "moccasin", IC_RGB(0xffe4b5) },
  { "navajowhite", IC_RGB(0xffdead) },
  { "navy", IC_RGB(0x000080) },
  { "oldlace", IC_RGB(0xfdf5e6) },
  { "olive", IC_RGB(0x808000) },
  { "olivedrab", IC_RGB(0x6b8e23) },
  { "orange", IC_RGB(0xffa500) },
  { "orangered", IC_RGB(0xff4500) },
  { "orchid", IC_RGB(0xda70d6) },
  { "palegoldenrod", IC_RGB(0xeee8aa) },
  { "palegreen", IC_RGB(0x98fb98) },
  { "paleturquoise", IC_RGB(0xafeeee) },
  { "palevioletred", IC_RGB(0xdb7093) },
  { "papayawhip", IC_RGB(0xffefd5) },
  { "peachpuff", IC_RGB(0xffdab9) },
  { "peru", IC_RGB(0xcd853f) },
  { "pink", IC_RGB(0xffc0cb) },
  { "plum", IC_RGB(0xdda0dd) },
  { "powderblue", IC_RGB(0xb0e0e6) },
  { "purple", IC_RGB(0x800080) },
  { "rebeccapurple", IC_RGB(0x663399) },
  { "red", IC_RGB(0xff0000) },
  { "rosybrown", IC_RGB(0xbc8f8f) },
  { "royalblue", IC_RGB(0x4169e1) },
  { "saddlebrown", IC_RGB(0x8b4513) },
  { "salmon", IC_RGB(0xfa8072) },
  { "sandybrown", IC_RGB(0xf4a460) },
  { "seagreen", IC_RGB(0x2e8b57) },
  { "seashell", IC_RGB(0xfff5ee) },
  { "sienna", IC_RGB(0xa0522d) },
  { "silver", IC_RGB(0xc0c0c0) },
  { "skyblue", IC_RGB(0x87ceeb) },
  { "slateblue", IC_RGB(0x6a5acd) },
  { "slategray", IC_RGB(0x708090) },
  { "slategrey", IC_RGB(0x708090) },
  { "snow", IC_RGB(0xfffafa) },
  { "springgreen", IC_RGB(0x00ff7f) },
  { "steelblue", IC_RGB(0x4682b4) },
  { "tan", IC_RGB(0xd2b48c) },
  { "teal", IC_RGB(0x008080) },
  { "thistle", IC_RGB(0xd8bfd8) },
  { "tomato", IC_RGB(0xff6347) },
  { "turquoise", IC_RGB(0x40e0d0) },
  { "violet", IC_RGB(0xee82ee) },
  { "wheat", IC_RGB(0xf5deb3) },
  { "white", IC_RGB(0xffffff) },
  { "whitesmoke", IC_RGB(0xf5f5f5) },
  { "yellow", IC_RGB(0xffff00) },
  { "yellowgreen", IC_RGB(0x9acd32) },
  {NULL, 0}
}; 

//-------------------------------------------------------------
// Types
//-------------------------------------------------------------

typedef struct style_s {
  const char*  name;  // name of the style
  attr_t  attr;  // attribute to apply
} style_t;

typedef enum align_e {
  IC_ALIGN_LEFT,
  IC_ALIGN_CENTER,
  IC_ALIGN_RIGHT
} align_t;

typedef struct width_s {
  ssize_t w;     // > 0
  align_t align;
  bool    dots;  // "..."  (e.g. "sentence...")
  char    fill;  // " "    (e.g. "hello      ")
} width_t;

typedef struct tag_s {  
  const char* name;   // tag open name
  attr_t  attr;       // the saved attribute before applying the style
  width_t width;      // start sequence of at most "width" columns
  ssize_t pos;        // start position in the output (used for width restriction)
} tag_t;


static void tag_init(tag_t* tag) {
  memset(tag,0,sizeof(*tag));  
}

struct bbcode_s {
  tag_t*       tags;              // stack of tags; one entry for each open tag
  ssize_t      tags_capacity;
  ssize_t      tags_nesting;   
  style_t*     styles;            // list of used defined styles
  ssize_t      styles_capacity;
  ssize_t      styles_count;
  term_t*      term;              // terminal
  alloc_t*     mem;               // allocator
  // caches
  stringbuf_t*  out;              // print buffer
  attrbuf_t*    out_attrs;
  stringbuf_t*  vout;             // vprintf buffer 
};


//-------------------------------------------------------------
// Create, helpers
//-------------------------------------------------------------

ic_private bbcode_t* bbcode_new( alloc_t* mem, term_t* term ) {
  bbcode_t* bb = mem_zalloc_tp(mem,bbcode_t);
  if (bb==NULL) return NULL;
  bb->mem = mem;
  bb->term = term;
  bb->out = sbuf_new(mem);
  bb->out_attrs = attrbuf_new(mem);
  bb->vout = sbuf_new(mem);
  return bb;
}

ic_private void bbcode_free( bbcode_t* bb ) {
  for(ssize_t i = 0; i < bb->styles_count; i++) {
    mem_free(bb->mem, bb->styles[i].name);
  }
  mem_free(bb->mem, bb->tags);
  mem_free(bb->mem, bb->styles);
  sbuf_free(bb->vout);
  sbuf_free(bb->out);
  attrbuf_free(bb->out_attrs);
  mem_free(bb->mem, bb);
}

ic_private void bbcode_style_add( bbcode_t* bb, const char* style_name, attr_t attr ) {
  if (bb->styles_count >= bb->styles_capacity) {
    ssize_t newlen = bb->styles_capacity + 32;
    style_t* p = mem_realloc_tp( bb->mem, style_t, bb->styles, newlen );
    if (p == NULL) return;
    bb->styles = p;
    bb->styles_capacity = newlen;
  }
  assert(bb->styles_count < bb->styles_capacity);
  bb->styles[bb->styles_count].name = mem_strdup( bb->mem, style_name );
  bb->styles[bb->styles_count].attr = attr;
  bb->styles_count++;
}

static ssize_t bbcode_tag_push( bbcode_t* bb, const tag_t* tag ) {
  if (bb->tags_nesting >= bb->tags_capacity) {
    ssize_t newcap = bb->tags_capacity + 32;
    tag_t* p = mem_realloc_tp( bb->mem, tag_t, bb->tags, newcap );
    if (p == NULL) return -1;
    bb->tags = p;
    bb->tags_capacity = newcap;    
  }
  assert(bb->tags_nesting < bb->tags_capacity);
  bb->tags[bb->tags_nesting] = *tag;
  bb->tags_nesting++;
  return (bb->tags_nesting-1);
}

static void bbcode_tag_pop( bbcode_t* bb, tag_t* tag ) {
  if (bb->tags_nesting <= 0) {
    if (tag != NULL) { tag_init(tag); }
  }
  else {
    bb->tags_nesting--;
    if (tag != NULL) {
      *tag = bb->tags[bb->tags_nesting];
    }    
  }
}

//-------------------------------------------------------------
// Invalid parse/values/balance
//-------------------------------------------------------------

static void bbcode_invalid(const char* fmt, ... ) {
  if (getenv("ISOCLINE_BBCODE_DEBUG") != NULL) {
    va_list args;
    va_start(args,fmt);
    vfprintf(stderr,fmt,args);
    va_end(args);
  }
}

//-------------------------------------------------------------
// Set attributes
//-------------------------------------------------------------


static attr_t bbcode_open( bbcode_t* bb, ssize_t out_pos, const tag_t* tag, attr_t current ) { 
  // save current and set
  tag_t cur;
  tag_init(&cur);
  cur.name  = tag->name;
  cur.attr  = current;  
  cur.width = tag->width;
  cur.pos   = out_pos;
  bbcode_tag_push(bb,&cur);
  return attr_update_with( current, tag->attr );
}

static bool bbcode_close( bbcode_t* bb, ssize_t base, const char* name, tag_t* pprev ) {
  // pop until match
  while (bb->tags_nesting > base) {
    tag_t prev;
    bbcode_tag_pop(bb,&prev);
    if (name==NULL || prev.name==NULL || ic_stricmp(prev.name,name) == 0) {
      // matched
      if (pprev != NULL) { *pprev = prev; }
      return true;
    }
    else {
      // unbalanced: we either continue popping or restore the tags depending if there is a matching open tag in our tags.
      bool has_open_tag = false;
      if (name != NULL) {
        for( ssize_t i = bb->tags_nesting - 1; i > base; i--) {
          if (bb->tags[i].name != NULL && ic_stricmp(bb->tags[i].name, name) == 0) {
            has_open_tag = true;
            break;
          }
        }
      }
      bbcode_invalid("bbcode: unbalanced tags: open [%s], close [/%s]\n", prev.name, name);            
      if (!has_open_tag) {
        bbcode_tag_push( bb, &prev ); // restore the tags and ignore this close tag
        break;
      }
      else {
        // continue until we hit our open tag
      }
    }
  }
  if (pprev != NULL) { memset(pprev,0,sizeof(*pprev)); }
  return false;
}

//-------------------------------------------------------------
// Update attributes
//-------------------------------------------------------------

static const char* attr_update_bool( const char* fname, signed int* field, const char* value ) {
  if (value == NULL || value[0] == 0 || strcmp(value,"on") || strcmp(value,"true") || strcmp(value,"1")) {
    *field = IC_ON;
  }
  else if (strcmp(value,"off") || strcmp(value,"false") || strcmp(value,"0")) {
    *field = IC_OFF;
  }
  else {
    bbcode_invalid("bbcode: invalid %s value: %s\n", fname, value );
  }
  return fname;
}

static const char* attr_update_color( const char* fname, ic_color_t* field, const char* value ) {
  if (value == NULL || value[0] == 0 || strcmp(value,"none") == 0) {
    *field = IC_COLOR_NONE;
    return fname;
  }
  
  // hex value
  if (value[0] == '#') {
    uint32_t rgb = 0;
    if (sscanf(value,"#%x",&rgb) == 1) {
      *field = ic_rgb(rgb);
    } 
    else {
      bbcode_invalid("bbcode: invalid color code: %s\n", value);
    }
    return fname;
  }

  // search color names
  ssize_t lo = 0;
  ssize_t hi = IC_HTML_COLOR_COUNT-1;
  while( lo <= hi ) {
    ssize_t mid = (lo + hi) / 2;
    style_color_t* info = &html_colors[mid];
    int cmp = strcmp(info->name,value);
    if (cmp < 0) {
      lo = mid+1;
    }
    else if (cmp > 0) {
      hi = mid-1;
    }
    else { 
      *field = info->color;
      return fname;
    }    
  }
  bbcode_invalid("bbcode: unknown %s: %s\n", fname, value);
  *field = IC_COLOR_NONE;
  return fname;
}

static const char* attr_update_sgr( const char* fname, attr_t* attr, const char* value ) {
  *attr = attr_update_with(*attr, attr_from_sgr(value, ic_strlen(value)));
  return fname;
}

static void attr_update_width( width_t* pwidth, char default_fill, const char* value ) {
  // parse width value: <width>;<left|center|right>;<fill>;<cut>
  width_t width;
  memset(&width, 0, sizeof(width));
  width.fill = default_fill;   // use 0 for no-fill (as for max-width)
  if (ic_atoz(value, &width.w)) {     
    ssize_t i = 0;
    while( value[i] != ';' && value[i] != 0 ) { i++; }
    if (value[i] == ';') {
      i++;
      ssize_t len = 0;    
      while( value[i+len] != ';' && value[i+len] != 0 ) { len++; }
      if (len == 4 && ic_istarts_with(value+i,"left")) {
        width.align = IC_ALIGN_LEFT;
      }
      else if (len == 5 && ic_istarts_with(value+i,"right")) {
        width.align = IC_ALIGN_RIGHT;
      }
      else if (len == 6 && ic_istarts_with(value+i,"center")) {
        width.align = IC_ALIGN_CENTER;
      }
      i += len;
      if (value[i] == ';') {
        i++; len = 0;
        while( value[i+len] != ';' && value[i+len] != 0 ) { len++; }
        if (len == 1) { width.fill = value[i]; }
        i+= len;
        if (value[i] == ';') {
          i++; len = 0;
          while( value[i+len] != ';' && value[i+len] != 0 ) { len++; }
          if ((len == 2 && ic_istarts_with(value+i,"on")) || (len == 1 && value[i] == '1')) { width.dots = true; }
          i += len;
        }
      }
    }
  }
  else {
    bbcode_invalid("bbcode: illegal width: %s\n", value);
  }
  *pwidth = width;
}

static const char* attr_update_ansi_color( const char* fname, ic_color_t* color, const char* value ) {
  ssize_t num = 0;
  if (ic_atoz(value, &num) && num >= 0 && num <= 256) {
    *color = color_from_ansi256(num);
  }
  return fname;
}


static const char* attr_update_property( tag_t* tag, const char* attr_name, const char* value ) {
  const char* fname = NULL;
  fname = "bold";
  if (strcmp(attr_name,fname) == 0) {    
    signed int b = IC_NONE;    
    attr_update_bool(fname,&b, value); 
    if (b != IC_NONE) { tag->attr.x.bold = b; }
    return fname;
  }
  fname = "italic";
  if (strcmp(attr_name,fname) == 0) {    
    signed int b = IC_NONE;      
    attr_update_bool(fname,&b, value); 
    if (b != IC_NONE) { tag->attr.x.italic = b; }
    return fname;
  }
  fname = "underline";
  if (strcmp(attr_name,fname) == 0) {  
    signed int b = IC_NONE;        
    attr_update_bool(fname,&b, value); 
    if (b != IC_NONE) { tag->attr.x.underline = b; }
    return fname;
  }
  fname = "reverse";
  if (strcmp(attr_name,fname) == 0) {
    signed int b = IC_NONE;          
    attr_update_bool(fname,&b, value); 
    if (b != IC_NONE) { tag->attr.x.reverse = b; }
    return fname;
  }
  fname = "color";
  if (strcmp(attr_name,fname) == 0) {
    unsigned int color = IC_COLOR_NONE;
    attr_update_color(fname, &color, value);
    if (color != IC_COLOR_NONE) { tag->attr.x.color = color; }
    return fname;
  }
  fname = "bgcolor";
  if (strcmp(attr_name,fname) == 0) {
    unsigned int color = IC_COLOR_NONE;
    attr_update_color(fname, &color, value);
    if (color != IC_COLOR_NONE) { tag->attr.x.bgcolor = color; }
    return fname;
  }
  fname = "ansi-sgr";
  if (strcmp(attr_name,fname) == 0) {
    attr_update_sgr(fname, &tag->attr, value);
    return fname;
  }
  fname = "ansi-color";
  if (strcmp(attr_name,fname) == 0) {
    ic_color_t color = IC_COLOR_NONE;;
    attr_update_ansi_color(fname, &color, value);
    if (color != IC_COLOR_NONE) { tag->attr.x.color = color; }
    return fname;
  }
  fname = "ansi-bgcolor";
  if (strcmp(attr_name,fname) == 0) {
    ic_color_t color = IC_COLOR_NONE;;
    attr_update_ansi_color(fname, &color, value);
    if (color != IC_COLOR_NONE) { tag->attr.x.bgcolor = color; }
    return fname;
  }  
  fname = "width";
  if (strcmp(attr_name,fname) == 0) {
    attr_update_width(&tag->width, ' ', value);
    return fname;
  }
  fname = "max-width";
  if (strcmp(attr_name,fname) == 0) {
    attr_update_width(&tag->width, 0, value);
    return "width";
  }    
  else {
    return NULL;
  }
}

static const style_t builtin_styles[] = {
  { "b",  { { IC_COLOR_NONE, IC_ON  , IC_NONE, IC_COLOR_NONE, IC_NONE, IC_NONE } } },
  { "r",  { { IC_COLOR_NONE, IC_NONE, IC_ON  , IC_COLOR_NONE, IC_NONE, IC_NONE } } },
  { "u",  { { IC_COLOR_NONE, IC_NONE, IC_NONE, IC_COLOR_NONE, IC_ON  , IC_NONE } } },
  { "i",  { { IC_COLOR_NONE, IC_NONE, IC_NONE, IC_COLOR_NONE, IC_NONE, IC_ON   } } },
  { "em", { { IC_COLOR_NONE, IC_ON  , IC_NONE, IC_COLOR_NONE, IC_NONE, IC_NONE } } }, // bold
  { "url",{ { IC_COLOR_NONE, IC_NONE, IC_NONE, IC_COLOR_NONE, IC_ON,   IC_NONE } } }, // underline
  { NULL, { { IC_COLOR_NONE, IC_NONE, IC_NONE, IC_COLOR_NONE, IC_NONE, IC_NONE } } }
};

static void attr_update_with_styles( tag_t* tag, const char* attr_name, const char* value, 
                                             bool usebgcolor, const style_t* styles, ssize_t count ) 
{
  // direct hex color?
  if (attr_name[0] == '#' && (value == NULL || value[0]==0)) {
    value = attr_name;
    attr_name = (usebgcolor ? "bgcolor" : "color");
  }
  // first try if it is a builtin property
  const char* name;
  if ((name = attr_update_property(tag,attr_name,value)) != NULL) {
    if (tag->name != NULL) tag->name = name;
    return;
  }
  // then check all styles
  while( count-- > 0 ) {
    const style_t* style = styles + count;
    if (strcmp(style->name,attr_name) == 0) {
      tag->attr = attr_update_with(tag->attr,style->attr);
      if (tag->name != NULL) tag->name = style->name;
      return;
    }    
  }
  // check builtin styles; todo: binary search?
  for( const style_t* style = builtin_styles; style->name != NULL; style++) {
    if (strcmp(style->name,attr_name) == 0) {
      tag->attr = attr_update_with(tag->attr,style->attr);
      if (tag->name != NULL) tag->name = style->name;
      return;
    }
  }
  // check colors as a style
  ssize_t lo = 0;
  ssize_t hi = IC_HTML_COLOR_COUNT-1;
  while( lo <= hi ) {
    ssize_t mid = (lo + hi) / 2;
    style_color_t* info = &html_colors[mid];
    int cmp = strcmp(info->name,attr_name);    
    if (cmp < 0) {
      lo = mid+1;
    }
    else if (cmp > 0) {
      hi = mid-1;
    }
    else {
      attr_t cattr = attr_none();
      if (usebgcolor) { cattr.x.bgcolor = info->color; }
                else  { cattr.x.color = info->color; }
      tag->attr = attr_update_with(tag->attr,cattr);
      if (tag->name != NULL) tag->name = info->name;
      return;
    }
  }
  // not found
  bbcode_invalid("bbcode: unknown style: %s\n", attr_name);  
}


ic_private attr_t bbcode_style( bbcode_t* bb, const char* style_name ) {
  tag_t tag;
  tag_init(&tag);
  attr_update_with_styles( &tag, style_name, NULL, false, bb->styles, bb->styles_count );
  return tag.attr;
}

//-------------------------------------------------------------
// Parse tags
//-------------------------------------------------------------

ic_private const char* parse_skip_white(const char* s) {
  while( *s != 0 && *s != ']') {
    if (!(*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r')) break;
    s++;
  }
  return s;
}

ic_private const char* parse_skip_to_white(const char* s) {
  while( *s != 0 && *s != ']') {  
    if (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') break;
    s++;
  }
  return parse_skip_white(s);
}

ic_private const char* parse_skip_to_end(const char* s) {
  while( *s != 0 && *s != ']' ) { s++; }    
  return s;
}

ic_private const char* parse_attr_name(const char* s) {
  if (*s == '#') {
    s++; // hex rgb color as id
    while( *s != 0 && *s != ']') {
      if (!((*s >= 'a' && *s <= 'f') || (*s >= 'A' && *s <= 'Z') || (*s >= '0' && *s <= '9'))) break;
      s++;
    }
  }
  else {
    while( *s != 0 && *s != ']') {
      if (!((*s >= 'a' && *s <= 'z') || (*s >= 'A' && *s <= 'Z') || 
            (*s >= '0' && *s <= '9') || *s == '_' || *s == '-')) break;
      s++;
    }
  }    
  return s;
}

ic_private const char* parse_value(const char* s, const char** start, const char** end) {
  if (*s == '"') {
    s++;
    *start = s;
    while( *s != 0 ) {
      if (*s == '"') break;
      s++;
    }
    *end = s;      
    if (*s == '"') { s++; }
  }
  else if (*s == '#') {
    *start = s;
    s++;
    while( *s != 0 ) {
      if (!((*s >= 'a' && *s <= 'f') || (*s >= 'A' && *s <= 'Z') || (*s >= '0' && *s <= '9'))) break;
      s++;
    }
    *end = s;
  }
  else {
    *start = s;
    while( *s != 0 ) {
      if (!((*s >= 'a' && *s <= 'z') || (*s >= 'A' && *s <= 'F') || (*s >= '0' && *s <= '9') || *s == '-' || *s == '_')) break;
      s++;
    }
    *end = s;
  }  
  return s;  
}

ic_private const char* parse_tag_value( tag_t* tag, char* idbuf, const char* s, const style_t* styles, ssize_t scount ) {
  // parse: \s*[\w-]+\s*(=\s*<value>)
  bool usebgcolor = false;
  const char* id = s;
  const char* idend = parse_attr_name(id);
  const char* val = NULL;
  const char* valend = NULL;  
  if (id == idend) {
    bbcode_invalid("bbcode: empty identifier? %.10s...\n", id );
    return parse_skip_to_white(id);
  }
  // "on" bgcolor?
  s = parse_skip_white(idend);
  if (idend - id == 2 && ic_strnicmp(id,"on",2) == 0 && *s != '=') {
    usebgcolor = true;
    id = s;
    idend = parse_attr_name(id);
    if (id == idend) {
      bbcode_invalid("bbcode: empty identifier follows 'on'? %.10s...\n", id );
      return parse_skip_to_white(id);
    }
    s = parse_skip_white(idend);      
  }
  // value
  if (*s == '=') {
    s++;
    s = parse_skip_white(s);
    s = parse_value(s, &val, &valend);
    s = parse_skip_white(s);
  }  
  // limit name and attr to 128 bytes
  char valbuf[128];
  ic_strncpy( idbuf, 128, id, idend - id);
  ic_strncpy( valbuf, 128, val, valend - val);
  ic_str_tolower(idbuf);
  ic_str_tolower(valbuf);
  attr_update_with_styles( tag, idbuf, valbuf, usebgcolor, styles, scount );  
  return s;
}

static const char* parse_tag_values( tag_t* tag, char* idbuf, const char* s, const style_t* styles, ssize_t scount ) {
  s = parse_skip_white(s);  
  idbuf[0] = 0;
  ssize_t count = 0;
  while( *s != 0 && *s != ']') {
    char idbuf_next[128];
    s = parse_tag_value(tag, (count==0 ? idbuf : idbuf_next), s, styles, scount);
    count++;
  }
  if (*s == ']') { s++; }
  return s;
}

static const char* parse_tag( tag_t* tag, char* idbuf, bool* open, bool* pre, const char* s, const style_t* styles, ssize_t scount ) {
  *open = true;
  *pre = false;
  if (*s != '[') return s;
  s = parse_skip_white(s+1);
  if (*s == '!') { // pre
    *pre = true;
    s = parse_skip_white(s+1);  
  }  
  else if (*s == '/') { 
    *open = false; 
    s = parse_skip_white(s+1); 
  };
  s = parse_tag_values( tag, idbuf, s, styles, scount);
  return s;
}


//---------------------------------------------------------
// Styles
//---------------------------------------------------------

static void bbcode_parse_tag_content( bbcode_t* bb, const char* s, tag_t* tag ) {
  tag_init(tag);
  if (s != NULL) { 
    char idbuf[128];
    parse_tag_values(tag, idbuf, s, bb->styles, bb->styles_count);
  }
}

ic_private void bbcode_style_def( bbcode_t* bb, const char* style_name, const char* s ) {
  tag_t tag;
  bbcode_parse_tag_content( bb, s, &tag);
  bbcode_style_add(bb, style_name, tag.attr);
}

ic_private void bbcode_style_open( bbcode_t* bb, const char* fmt ) {
  tag_t tag;
  bbcode_parse_tag_content(bb, fmt, &tag);
  term_set_attr( bb->term, bbcode_open(bb, 0, &tag, term_get_attr(bb->term)) );
}

ic_private void bbcode_style_close( bbcode_t* bb, const char* fmt ) {
  const ssize_t base = bb->tags_nesting - 1; // as we end a style
  tag_t tag;
  bbcode_parse_tag_content(bb, fmt, &tag);  
  tag_t prev;
  if (bbcode_close(bb, base, tag.name, &prev)) {
    term_set_attr( bb->term, prev.attr );
  }
}

//---------------------------------------------------------
// Restrict to width
//---------------------------------------------------------

static void bbcode_restrict_width( ssize_t start, width_t width, stringbuf_t* out, attrbuf_t* attr_out ) {
  if (width.w <= 0) return;
  assert(start <= sbuf_len(out));
  assert(attr_out == NULL || sbuf_len(out) == attrbuf_len(attr_out));
  const char*   s   = sbuf_string(out) + start;
  const ssize_t len = sbuf_len(out) - start;
  const ssize_t w   = str_column_width(s);
  if (w == width.w) return; // fits exactly
  if (w > width.w) {
    // too large
    ssize_t innerw = (width.dots && width.w > 3 ? width.w-3 : width.w);
    if (width.align == IC_ALIGN_RIGHT) {
      // right align
      const ssize_t ndel = str_skip_until_fit( s, innerw );
      sbuf_delete_at( out, start, ndel );
      attrbuf_delete_at( attr_out, start, ndel );
      if (innerw < width.w) {
        // add dots
        sbuf_insert_at( out, "...", start );
        attr_t attr = attrbuf_attr_at(attr_out, start);
        attrbuf_insert_at( attr_out, start, 3, attr);
      }
    }
    else {
      // left or center align
      ssize_t count = str_take_while_fit( s, innerw );
      sbuf_delete_at( out, start + count, len - count );
      attrbuf_delete_at( attr_out, start + count, len - count );
      if (innerw < width.w) {
        // add dots
        attr_t attr = attrbuf_attr_at(attr_out,start);
        attrbuf_append_n( out, attr_out, "...", 3, attr );
      }
    }
  }
  else {
    // too short, pad to width
    const ssize_t diff = (width.w - w);
    const ssize_t pad_left  = (width.align == IC_ALIGN_RIGHT ? diff : (width.align == IC_ALIGN_LEFT  ? 0 : diff / 2));
    const ssize_t pad_right = (width.align == IC_ALIGN_LEFT  ? diff : (width.align == IC_ALIGN_RIGHT ? 0 : diff - pad_left));
    if (width.fill != 0 && pad_left > 0) {
      const attr_t attr = attrbuf_attr_at(attr_out,start);
      for( ssize_t i = 0; i < pad_left; i++) {  // todo: optimize
        sbuf_insert_char_at(out, width.fill, start);
      }
      attrbuf_insert_at( attr_out, start, pad_left, attr );
    }
    if (width.fill != 0 && pad_right > 0) {
      const attr_t attr = attrbuf_attr_at(attr_out,sbuf_len(out) - 1);
      char buf[2];
      buf[0] = width.fill;
      buf[1] = 0;        
      for( ssize_t i = 0; i < pad_right; i++) {  // todo: optimize
        attrbuf_append_n( out, attr_out, buf, 1, attr );
      }      
    }
  }
}

//---------------------------------------------------------
// Print
//---------------------------------------------------------

ic_private ssize_t bbcode_process_tag( bbcode_t* bb, const char* s, const ssize_t nesting_base, 
                                        stringbuf_t* out, attrbuf_t* attr_out, attr_t* cur_attr ) {
  assert(*s == '[');
  tag_t tag;
  tag_init(&tag);  
  bool open = true;
  bool ispre = false;
  char idbuf[128];
  const char* end = parse_tag( &tag, idbuf, &open, &ispre, s, bb->styles, bb->styles_count ); // todo: styles
  assert(end > s);
  if (open) {
    if (!ispre) {
      // open tag
      *cur_attr = bbcode_open( bb, sbuf_len(out), &tag, *cur_attr );
    }
    else {
      // scan pre to end tag
      attr_t attr = attr_update_with(*cur_attr, tag.attr);
      char pre[132];
      if (snprintf(pre, 132, "[/%s]", idbuf) < ssizeof(pre)) {
        const char* etag = strstr(end,pre);
        if (etag == NULL) {
          const ssize_t len = ic_strlen(end);
          attrbuf_append_n(out, attr_out, end, len, attr);
          end += len;
        }
        else {
          attrbuf_append_n(out, attr_out, end, (etag - end), attr);
          end = etag + ic_strlen(pre);
        }
      }
    }
  }
  else {
    // pop the tag
    tag_t prev;
    if (bbcode_close( bb, nesting_base, tag.name, &prev)) {
      *cur_attr = prev.attr;
      if (prev.width.w > 0) {
        // closed a width tag; restrict the output to width
        bbcode_restrict_width( prev.pos, prev.width, out, attr_out);
      }
    }
  }  
  return (end - s);
}

ic_private void bbcode_append( bbcode_t* bb, const char* s, stringbuf_t* out, attrbuf_t* attr_out ) {
  if (bb == NULL || s == NULL) return;
  attr_t attr = attr_none();
  const ssize_t base = bb->tags_nesting; // base; will not be popped
  ssize_t i = 0;
  while( s[i] != 0 ) {
    // handle no tags in bulk
    ssize_t nobb = 0;
    char c;
    while( (c = s[i+nobb]) != 0) {
      if (c == '[' || c == '\\') { break; }
      if (c == '\x1B' && s[i+nobb+1] == '[') {
        nobb++; // don't count 'ESC[' as a tag opener
      }
      nobb++;
    }
    if (nobb > 0) { attrbuf_append_n(out, attr_out, s+i, nobb, attr); }
    i += nobb;
    // tag
    if (s[i] == '[') {
      i += bbcode_process_tag(bb, s+i, base, out, attr_out, &attr);
    }
    else if (s[i] == '\\') {
      if (s[i+1] == '\\' || s[i+1] == '[') {
        attrbuf_append_n(out, attr_out, s+i+1, 1, attr); // escape '\[' and '\\' 
        i += 2;
      }
      else {
        attrbuf_append_n(out, attr_out, s+i, 1, attr);  // pass '\\' as is
        i++;
      }
    }
  }
  // pop unclosed openings
  assert(bb->tags_nesting >= base);
  while( bb->tags_nesting > base ) {
    bbcode_tag_pop(bb,NULL);
  };
}

ic_private void bbcode_print( bbcode_t* bb, const char* s ) {
  if (bb->out == NULL || bb->out_attrs == NULL || s == NULL) return;
  assert(sbuf_len(bb->out) == 0 && attrbuf_len(bb->out_attrs) == 0);
  bbcode_append( bb, s, bb->out, bb->out_attrs );
  term_write_formatted( bb->term, sbuf_string(bb->out), attrbuf_attrs(bb->out_attrs,sbuf_len(bb->out)) );
  attrbuf_clear(bb->out_attrs);
  sbuf_clear(bb->out);
}

ic_private void bbcode_println( bbcode_t* bb, const char* s ) {
  bbcode_print(bb,s);
  term_writeln(bb->term, "");
}

ic_private void bbcode_vprintf( bbcode_t* bb, const char* fmt, va_list args  ) {
  if (bb->vout == NULL || fmt == NULL) return;
  assert(sbuf_len(bb->vout) == 0);
  sbuf_append_vprintf(bb->vout,fmt,args);
  bbcode_print(bb, sbuf_string(bb->vout));
  sbuf_clear(bb->vout);
}

ic_private void bbcode_printf( bbcode_t* bb, const char* fmt, ... ) {
  va_list args;
  va_start(args,fmt);
  bbcode_vprintf(bb,fmt,args);
  va_end(args);
}

ic_private ssize_t bbcode_column_width( bbcode_t* bb, const char* s ) {
  if (s==NULL || s[0] == 0) return 0;
  if (bb->vout == NULL) { return str_column_width(s); }
  assert(sbuf_len(bb->vout) == 0); 
  bbcode_append( bb, s, bb->vout, NULL);
  const ssize_t w = str_column_width(sbuf_string(bb->vout));
  sbuf_clear(bb->vout);
  return w;
}
// # include "editline.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <string.h>

// skipping dup #include "common.h"
// skipping dup #include "term.h"
// skipping dup #include "tty.h"
// #include "env.h"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
// #pragma once
#ifndef IC_ENV_H
#define IC_ENV_H

// skipping dup #include "../include/isocline.h"
// skipping dup #include "common.h"
// skipping dup #include "term.h"
// skipping dup #include "tty.h"
// skipping dup #include "stringbuf.h"
// #include "history.h"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
// #pragma once
#ifndef IC_HISTORY_H
#define IC_HISTORY_H

// skipping dup #include "common.h"

//-------------------------------------------------------------
// History
//-------------------------------------------------------------

struct history_s;
typedef struct history_s history_t;

ic_private history_t* history_new(alloc_t* mem);
ic_private void     history_free(history_t* h);
ic_private void     history_clear(history_t* h);
ic_private bool     history_enable_duplicates( history_t* h, bool enable );
ic_private ssize_t  history_count(const history_t* h);

ic_private void     history_load_from(history_t* h, const char* fname, long max_entries);
ic_private void     history_load( history_t* h );
ic_private void     history_save( const history_t* h );

ic_private bool     history_push( history_t* h, const char* entry );
ic_private bool     history_update( history_t* h, const char* entry );
ic_private const char* history_get( const history_t* h, ssize_t n );
ic_private void     history_remove_last(history_t* h);

ic_private bool     history_search( const history_t* h, ssize_t from, const char* search, bool backward, ssize_t* hidx, ssize_t* hpos);


#endif // IC_HISTORY_H
// #include "completions.h"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
// #pragma once
#ifndef IC_COMPLETIONS_H
#define IC_COMPLETIONS_H

// skipping dup #include "common.h"
// skipping dup #include "stringbuf.h"


//-------------------------------------------------------------
// Completions
//-------------------------------------------------------------
#define IC_MAX_COMPLETIONS_TO_SHOW  (1000)
#define IC_MAX_COMPLETIONS_TO_TRY   (IC_MAX_COMPLETIONS_TO_SHOW/4)

typedef struct completions_s completions_t;

ic_private completions_t* completions_new(alloc_t* mem);
ic_private void        completions_free(completions_t* cms);
ic_private void        completions_clear(completions_t* cms);
ic_private bool        completions_add(completions_t* cms , const char* replacement, const char* display, const char* help, ssize_t delete_before, ssize_t delete_after);
ic_private ssize_t     completions_count(completions_t* cms);
ic_private ssize_t     completions_generate(struct ic_env_s* env, completions_t* cms , const char* input, ssize_t pos, ssize_t max);
ic_private void        completions_sort(completions_t* cms);
ic_private void        completions_set_completer(completions_t* cms, ic_completer_fun_t* completer, void* arg);
ic_private const char* completions_get_display(completions_t* cms , ssize_t index, const char** help);
ic_private const char* completions_get_hint(completions_t* cms, ssize_t index, const char** help);
ic_private void        completions_get_completer(completions_t* cms, ic_completer_fun_t** completer, void** arg);

ic_private ssize_t     completions_apply(completions_t* cms, ssize_t index, stringbuf_t* sbuf, ssize_t pos);
ic_private ssize_t     completions_apply_longest_prefix(completions_t* cms, stringbuf_t* sbuf, ssize_t pos);

//-------------------------------------------------------------
// Completion environment
//-------------------------------------------------------------
typedef bool (ic_completion_fun_t)( ic_env_t* env, void* funenv, const char* replacement, const char* display, const char* help, long delete_before, long delete_after );

struct ic_completion_env_s {
  ic_env_t*   env;       // the isocline environment
  const char* input;     // current full input
  long        cursor;    // current cursor position
  void*       arg;       // argument given to `ic_set_completer`
  void*       closure;   // free variables for function composition
  ic_completion_fun_t* complete;  // function that adds a completion
};

#endif // IC_COMPLETIONS_H
// skipping dup #include "bbcode.h"

//-------------------------------------------------------------
// Environment
//-------------------------------------------------------------

struct ic_env_s {
  alloc_t*        mem;              // potential custom allocator
  ic_env_t*       next;             // next environment (used for proper deallocation)
  term_t*         term;             // terminal
  tty_t*          tty;              // keyboard (NULL if stdin is a pipe, file, etc)
  completions_t*  completions;      // current completions
  history_t*      history;          // edit history
  bbcode_t*       bbcode;           // print with bbcodes
  const char*     prompt_marker;    // the prompt marker (defaults to "> ")
  const char*     cprompt_marker;   // prompt marker for continuation lines (defaults to `prompt_marker`)
  ic_highlight_fun_t* highlighter;  // highlight callback
  void*           highlighter_arg;  // user state for the highlighter.
  const char*     match_braces;     // matching braces, e.g "()[]{}"
  const char*     auto_braces;      // auto insertion braces, e.g "()[]{}\"\"''"
  char            multiline_eol;    // character used for multiline input ("\") (set to 0 to disable)
  bool            initialized;      // are we initialized?
  bool            noedit;           // is rich editing possible (tty != NULL)
  bool            singleline_only;  // allow only single line editing?
  bool            complete_nopreview; // do not show completion preview for each selection in the completion menu?
  bool            complete_autotab; // try to keep completing after a completion?
  bool            no_multiline_indent; // indent continuation lines to line up under the initial prompt 
  bool            no_help;          // show short help line for history search etc.
  bool            no_hint;          // allow hinting?
  bool            no_highlight;     // enable highlighting?
  bool            no_bracematch;    // enable brace matching?
  bool            no_autobrace;     // enable automatic brace insertion?
  bool            no_lscolors;      // use LSCOLORS/LS_COLORS to colorize file name completions?
  long            hint_delay;       // delay before displaying a hint in milliseconds
};

ic_private char*        ic_editline(ic_env_t* env, const char* prompt_text);

ic_private ic_env_t*    ic_get_env(void);
ic_private const char*  ic_env_get_auto_braces(ic_env_t* env);
ic_private const char*  ic_env_get_match_braces(ic_env_t* env);

#endif // IC_ENV_H
// skipping dup #include "stringbuf.h"
// skipping dup #include "history.h"
// skipping dup #include "completions.h"
// #include "undo.h"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
// #pragma once
#ifndef IC_UNDO_H
#define IC_UNDO_H

// skipping dup #include "common.h"

//-------------------------------------------------------------
// Edit state
//-------------------------------------------------------------
struct editstate_s;
typedef struct editstate_s editstate_t;

ic_private void editstate_init( editstate_t** es );
ic_private void editstate_done( alloc_t* mem, editstate_t** es );
ic_private void editstate_capture( alloc_t* mem, editstate_t** es, const char* input, ssize_t pos);
ic_private bool editstate_restore( alloc_t* mem, editstate_t** es, const char** input, ssize_t* pos ); // caller needs to free input

#endif // IC_UNDO_H
// #include "highlight.h"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
// #pragma once
#ifndef IC_HIGHLIGHT_H
#define IC_HIGHLIGHT_H

// skipping dup #include "common.h"
// skipping dup #include "attr.h"
// skipping dup #include "term.h"
// skipping dup #include "bbcode.h"

//-------------------------------------------------------------
// Syntax highlighting
//-------------------------------------------------------------

ic_private void highlight( alloc_t* mem, bbcode_t* bb, const char* s, attrbuf_t* attrs, ic_highlight_fun_t* highlighter, void* arg );
ic_private void highlight_match_braces(const char* s, attrbuf_t* attrs, ssize_t cursor_pos, const char* braces, attr_t match_attr, attr_t error_attr);
ic_private ssize_t find_matching_brace(const char* s, ssize_t cursor_pos, const char* braces, bool* is_balanced);

#endif // IC_HIGHLIGHT_H

//-------------------------------------------------------------
// The editor state
//-------------------------------------------------------------



// editor state
typedef struct editor_s {
  stringbuf_t*  input;        // current user input
  stringbuf_t*  extra;        // extra displayed info (for completion menu etc)
  stringbuf_t*  hint;         // hint displayed as part of the input
  stringbuf_t*  hint_help;    // help for a hint.
  ssize_t       pos;          // current cursor position in the input
  ssize_t       cur_rows;     // current used rows to display our content (including extra content)
  ssize_t       cur_row;      // current row that has the cursor (0 based, relative to the prompt)
  ssize_t       termw;
  bool          modified;     // has a modification happened? (used for history navigation for example)  
  bool          disable_undo; // temporarily disable auto undo (for history search)
  ssize_t       history_idx;  // current index in the history 
  editstate_t*  undo;         // undo buffer  
  editstate_t*  redo;         // redo buffer
  const char*   prompt_text;  // text of the prompt before the prompt marker    
  alloc_t*      mem;          // allocator
  // caches
  attrbuf_t*    attrs;        // reuse attribute buffers 
  attrbuf_t*    attrs_extra; 
} editor_t;





//-------------------------------------------------------------
// Main edit line 
//-------------------------------------------------------------
static char* edit_line( ic_env_t* env, const char* prompt_text );  // defined at bottom
static void edit_refresh(ic_env_t* env, editor_t* eb);

ic_private char* ic_editline(ic_env_t* env, const char* prompt_text) {
  tty_start_raw(env->tty);
  term_start_raw(env->term);
  char* line = edit_line(env,prompt_text);
  term_end_raw(env->term,false);
  tty_end_raw(env->tty);
  term_writeln(env->term,"");
  term_flush(env->term);
  return line;
}


//-------------------------------------------------------------
// Undo/Redo
//-------------------------------------------------------------

// capture the current edit state
static void editor_capture(editor_t* eb, editstate_t** es ) {
  if (!eb->disable_undo) {
    editstate_capture( eb->mem, es, sbuf_string(eb->input), eb->pos );
  }
}

static void editor_undo_capture(editor_t* eb ) {
  editor_capture(eb, &eb->undo );
}

static void editor_undo_forget(editor_t* eb) {
  if (eb->disable_undo) return;
  const char* input = NULL;
  ssize_t pos = 0;
  editstate_restore(eb->mem, &eb->undo, &input, &pos);
  mem_free(eb->mem, input);
}

static void editor_restore(editor_t* eb, editstate_t** from, editstate_t** to ) {
  if (eb->disable_undo) return;
  if (*from == NULL) return;
  const char* input;
  if (to != NULL) { editor_capture( eb, to ); }
  if (!editstate_restore( eb->mem, from, &input, &eb->pos )) return;
  sbuf_replace( eb->input, input );
  mem_free(eb->mem, input);
  eb->modified = false;
}

static void editor_undo_restore(editor_t* eb, bool with_redo ) {
  editor_restore(eb, &eb->undo, (with_redo ? &eb->redo : NULL));
}

static void editor_redo_restore(editor_t* eb ) {
  editor_restore(eb, &eb->redo, &eb->undo);
  eb->modified = false;
}

static void editor_start_modify(editor_t* eb ) {
  editor_undo_capture(eb);
  editstate_done(eb->mem, &eb->redo);  // clear redo
  eb->modified = true;
}



static bool editor_pos_is_at_end(editor_t* eb ) {
  return (eb->pos == sbuf_len(eb->input));  
}

//-------------------------------------------------------------
// Row/Column width and positioning
//-------------------------------------------------------------


static void edit_get_prompt_width( ic_env_t* env, editor_t* eb, bool in_extra, ssize_t* promptw, ssize_t* cpromptw ) {
  if (in_extra) {
    *promptw = 0;
    *cpromptw = 0;
  }
  else {
    // todo: cache prompt widths
    ssize_t textw = bbcode_column_width(env->bbcode, eb->prompt_text);
    ssize_t markerw = bbcode_column_width(env->bbcode, env->prompt_marker);
    ssize_t cmarkerw = bbcode_column_width(env->bbcode, env->cprompt_marker);
    *promptw = markerw + textw;
    *cpromptw = (env->no_multiline_indent || *promptw < cmarkerw ? cmarkerw : *promptw);
  }
}

static ssize_t edit_get_rowcol( ic_env_t* env, editor_t* eb, rowcol_t* rc ) {
  ssize_t promptw, cpromptw;
  edit_get_prompt_width(env, eb, false, &promptw, &cpromptw);
  return sbuf_get_rc_at_pos( eb->input, eb->termw, promptw, cpromptw, eb->pos, rc );
}

static void edit_set_pos_at_rowcol( ic_env_t* env, editor_t* eb, ssize_t row, ssize_t col ) {
  ssize_t promptw, cpromptw;
  edit_get_prompt_width(env, eb, false, &promptw, &cpromptw);
  ssize_t pos = sbuf_get_pos_at_rc( eb->input, eb->termw, promptw, cpromptw, row, col );
  if (pos < 0) return;
  eb->pos = pos;
  edit_refresh(env, eb);
}

static bool edit_pos_is_at_row_end( ic_env_t* env, editor_t* eb ) {
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc );
  return rc.last_on_row;
}

static void edit_write_prompt( ic_env_t* env, editor_t* eb, ssize_t row, bool in_extra ) {
  if (in_extra) return;
  bbcode_style_open(env->bbcode, "ic-prompt");
  if (row==0) {
    // regular prompt text    
    bbcode_print( env->bbcode, eb->prompt_text );
  }
  else if (!env->no_multiline_indent) {
    // multiline continuation indentation
    // todo: cache prompt widths
    ssize_t textw = bbcode_column_width(env->bbcode, eb->prompt_text );
    ssize_t markerw = bbcode_column_width(env->bbcode, env->prompt_marker);
    ssize_t cmarkerw = bbcode_column_width(env->bbcode, env->cprompt_marker);      
    if (cmarkerw < markerw + textw) {
      term_write_repeat(env->term, " ", markerw + textw - cmarkerw );
    }
  }
  // the marker
  bbcode_print(env->bbcode, (row == 0 ? env->prompt_marker : env->cprompt_marker ));   
  bbcode_style_close(env->bbcode,NULL);    
}

//-------------------------------------------------------------
// Refresh
//-------------------------------------------------------------

typedef struct refresh_info_s {
  ic_env_t*   env;
  editor_t*   eb;
  attrbuf_t*  attrs;
  bool        in_extra;
  ssize_t     first_row;
  ssize_t     last_row;
} refresh_info_t;

static bool edit_refresh_rows_iter(
    const char* s,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    ssize_t startw, bool is_wrap, const void* arg, void* res)
{
  ic_unused(res); ic_unused(startw);
  const refresh_info_t* info = (const refresh_info_t*)(arg);
  term_t* term = info->env->term;

  // debug_msg("edit: line refresh: row %zd, len: %zd\n", row, row_len);
  if (row < info->first_row) return false;
  if (row > info->last_row)  return true; // should not occur
  
  // term_clear_line(term);
  edit_write_prompt(info->env, info->eb, row, info->in_extra);

  //' write output
  if (info->attrs == NULL || (info->env->no_highlight && info->env->no_bracematch)) {
    term_write_n( term, s + row_start, row_len );
  }
  else {
    term_write_formatted_n( term, s + row_start, attrbuf_attrs(info->attrs, row_start + row_len) + row_start, row_len );
  }

  // write line ending
  if (row < info->last_row) {
    if (is_wrap && tty_is_utf8(info->env->tty)) {       
      #ifndef __APPLE__
      bbcode_print( info->env->bbcode, "[ic-dim]\xE2\x86\x90");  // left arrow 
      #else
      bbcode_print( info->env->bbcode, "[ic-dim]\xE2\x86\xB5" ); // return symbol
      #endif
    }
    term_clear_to_end_of_line(term);
    term_writeln(term, "");
  }
  else {
    term_clear_to_end_of_line(term);
  }
  return (row >= info->last_row);  
}

static void edit_refresh_rows(ic_env_t* env, editor_t* eb, stringbuf_t* input, attrbuf_t* attrs,
                               ssize_t promptw, ssize_t cpromptw, bool in_extra, 
                                ssize_t first_row, ssize_t last_row) 
{
  if (input == NULL) return;
  refresh_info_t info;
  info.env        = env;
  info.eb         = eb;
  info.attrs      = attrs;
  info.in_extra   = in_extra;
  info.first_row  = first_row;
  info.last_row   = last_row;
  sbuf_for_each_row( input, eb->termw, promptw, cpromptw, &edit_refresh_rows_iter, &info, NULL);
}


static void edit_refresh(ic_env_t* env, editor_t* eb) 
{
  // calculate the new cursor row and total rows needed
  ssize_t promptw, cpromptw;
  edit_get_prompt_width( env, eb, false, &promptw, &cpromptw );
  
  if (eb->attrs != NULL) {
    highlight( env->mem, env->bbcode, sbuf_string(eb->input), eb->attrs, 
                 (env->no_highlight ? NULL : env->highlighter), env->highlighter_arg );
  }

  // highlight matching braces
  if (eb->attrs != NULL && !env->no_bracematch) {
    highlight_match_braces(sbuf_string(eb->input), eb->attrs, eb->pos, ic_env_get_match_braces(env),  
                              bbcode_style(env->bbcode,"ic-bracematch"), bbcode_style(env->bbcode,"ic-error"));
  }

  // insert hint  
  if (sbuf_len(eb->hint) > 0) {
    if (eb->attrs != NULL) {
      attrbuf_insert_at( eb->attrs, eb->pos, sbuf_len(eb->hint), bbcode_style(env->bbcode, "ic-hint") );
    }
    sbuf_insert_at(eb->input, sbuf_string(eb->hint), eb->pos );
  }

  // render extra (like a completion menu)
  stringbuf_t* extra = NULL;
  if (sbuf_len(eb->extra) > 0) {
    extra = sbuf_new(eb->mem);
    if (extra != NULL) {
      if (sbuf_len(eb->hint_help) > 0) {
        bbcode_append(env->bbcode, sbuf_string(eb->hint_help), extra, eb->attrs_extra);
      }
      bbcode_append(env->bbcode, sbuf_string(eb->extra), extra, eb->attrs_extra);
    }
  }

  // calculate rows and row/col position
  rowcol_t rc = { 0 };
  const ssize_t rows_input = sbuf_get_rc_at_pos( eb->input, eb->termw, promptw, cpromptw, eb->pos, &rc );
  rowcol_t rc_extra = { 0 };
  ssize_t rows_extra = 0;
  if (extra != NULL) { 
    rows_extra = sbuf_get_rc_at_pos( extra, eb->termw, 0, 0, 0 /*pos*/, &rc_extra ); 
  }
  const ssize_t rows = rows_input + rows_extra; 
  debug_msg("edit: refresh: rows %zd, cursor: %zd,%zd (previous rows %zd, cursor row %zd)\n", rows, rc.row, rc.col, eb->cur_rows, eb->cur_row);
  
  // only render at most terminal height rows
  const ssize_t termh = term_get_height(env->term);
  ssize_t first_row = 0;                 // first visible row 
  ssize_t last_row = rows - 1;           // last visible row
  if (rows > termh) {
    first_row = rc.row - termh + 1;      // ensure cursor is visible
    if (first_row < 0) first_row = 0;
    last_row = first_row + termh - 1;
  }
  assert(last_row - first_row < termh);
  
  // reduce flicker
  buffer_mode_t bmode = term_set_buffer_mode(env->term, BUFFERED);        

  // back up to the first line
  term_start_of_line(env->term);
  term_up(env->term, (eb->cur_row >= termh ? termh-1 : eb->cur_row) );
  // term_clear_lines_to_end(env->term);  // gives flicker in old Windows cmd prompt 

  // render rows
  edit_refresh_rows( env, eb, eb->input, eb->attrs, promptw, cpromptw, false, first_row, last_row );  
  if (rows_extra > 0) {
    assert(extra != NULL);
    const ssize_t first_rowx = (first_row > rows_input ? first_row - rows_input : 0);
    const ssize_t last_rowx = last_row - rows_input; assert(last_rowx >= 0);
    edit_refresh_rows(env, eb, extra, eb->attrs_extra, 0, 0, true, first_rowx, last_rowx);
  }
    
  // overwrite trailing rows we do not use anymore  
  ssize_t rrows = last_row - first_row + 1;  // rendered rows
  if (rrows < termh && rows < eb->cur_rows) {
    ssize_t clear = eb->cur_rows - rows;
    while (rrows < termh && clear > 0) {
      clear--;
      rrows++;
      term_writeln(env->term,"");
      term_clear_line(env->term);
    }
  }
  
  // move cursor back to edit position
  term_start_of_line(env->term);
  term_up(env->term, first_row + rrows - 1 - rc.row );
  term_right(env->term, rc.col + (rc.row == 0 ? promptw : cpromptw));

  // and refresh
  term_flush(env->term);

  // stop buffering
  term_set_buffer_mode(env->term, bmode);

  // restore input by removing the hint
  sbuf_delete_at(eb->input, eb->pos, sbuf_len(eb->hint));
  sbuf_delete_at(eb->extra, 0, sbuf_len(eb->hint_help));
  attrbuf_clear(eb->attrs);
  attrbuf_clear(eb->attrs_extra);
  sbuf_free(extra);

  // update previous
  eb->cur_rows = rows;
  eb->cur_row = rc.row;
}

// clear current output
static void edit_clear(ic_env_t* env, editor_t* eb ) {
  term_attr_reset(env->term);  
  term_up(env->term, eb->cur_row);
  
  // overwrite all rows
  for( ssize_t i = 0; i < eb->cur_rows; i++) {
    term_clear_line(env->term);
    term_writeln(env->term, "");    
  }
  
  // move cursor back 
  term_up(env->term, eb->cur_rows - eb->cur_row );  
}


// clear screen and refresh
static void edit_clear_screen(ic_env_t* env, editor_t* eb ) {
  ssize_t cur_rows = eb->cur_rows;
  eb->cur_rows = term_get_height(env->term) - 1;
  edit_clear(env,eb);
  eb->cur_rows = cur_rows;
  edit_refresh(env,eb);
}


// refresh after a terminal window resized (but before doing further edit operations!)
static bool edit_resize(ic_env_t* env, editor_t* eb ) {
  // update dimensions
  term_update_dim(env->term);
  ssize_t newtermw = term_get_width(env->term);
  if (eb->termw == newtermw) return false;
  
  // recalculate the row layout assuming the hardwrapping for the new terminal width
  ssize_t promptw, cpromptw;
  edit_get_prompt_width( env, eb, false, &promptw, &cpromptw );
  sbuf_insert_at(eb->input, sbuf_string(eb->hint), eb->pos); // insert used hint    
  
  // render extra (like a completion menu)
  stringbuf_t* extra = NULL;
  if (sbuf_len(eb->extra) > 0) {
    extra = sbuf_new(eb->mem);
    if (extra != NULL) {
      if (sbuf_len(eb->hint_help) > 0) {
        bbcode_append(env->bbcode, sbuf_string(eb->hint_help), extra, NULL);
      }
      bbcode_append(env->bbcode, sbuf_string(eb->extra), extra, NULL);
    }
  }
  rowcol_t rc = { 0 };
  const ssize_t rows_input = sbuf_get_wrapped_rc_at_pos( eb->input, eb->termw, newtermw, promptw, cpromptw, eb->pos, &rc );
  rowcol_t rc_extra = { 0 };
  ssize_t rows_extra = 0;
  if (extra != NULL) {
    rows_extra = sbuf_get_wrapped_rc_at_pos(extra, eb->termw, newtermw, 0, 0, 0 /*pos*/, &rc_extra);
  }  
  ssize_t rows = rows_input + rows_extra;
  debug_msg("edit: resize: new rows: %zd, cursor row: %zd (previous: rows: %zd, cursor row %zd)\n", rows, rc.row, eb->cur_rows, eb->cur_row);
  
  // update the newly calculated row and rows
  eb->cur_row = rc.row;
  if (rows > eb->cur_rows) {
    eb->cur_rows = rows;
  }
  eb->termw = newtermw;     
  edit_refresh(env,eb); 

  // remove hint again
  sbuf_delete_at(eb->input, eb->pos, sbuf_len(eb->hint));
  sbuf_free(extra);
  return true;
} 

static void editor_append_hint_help(editor_t* eb, const char* help) {
  sbuf_clear(eb->hint_help);
  if (help != NULL) {
    sbuf_replace(eb->hint_help, "[ic-info]");
    sbuf_append(eb->hint_help, help);
    sbuf_append(eb->hint_help, "[/ic-info]\n");
  }
}

// refresh with possible hint
static void edit_refresh_hint(ic_env_t* env, editor_t* eb) {
  if (env->no_hint || env->hint_delay > 0) {
    // refresh without hint first
    edit_refresh(env, eb);
    if (env->no_hint) return;
  }
    
  // and see if we can construct a hint (displayed after a delay)
  ssize_t count = completions_generate(env, env->completions, sbuf_string(eb->input), eb->pos, 2);
  if (count == 1) {
    const char* help = NULL;
    const char* hint = completions_get_hint(env->completions, 0, &help);
    if (hint != NULL) {
      sbuf_replace(eb->hint, hint); 
      editor_append_hint_help(eb, help);
      // do auto-tabbing?
      if (env->complete_autotab) {
        stringbuf_t* sb = sbuf_new(env->mem);  // temporary buffer for completion
        if (sb != NULL) { 
          sbuf_replace( sb, sbuf_string(eb->input) ); 
          ssize_t pos = eb->pos;
          const char* extra_hint = hint;
          do {
            ssize_t newpos = sbuf_insert_at( sb, extra_hint, pos );
            if (newpos <= pos) break;
            pos = newpos;
            count = completions_generate(env, env->completions, sbuf_string(sb), pos, 2);
            if (count == 1) {
              const char* extra_help = NULL;
              extra_hint = completions_get_hint(env->completions, 0, &extra_help);
              if (extra_hint != NULL) {
                editor_append_hint_help(eb, extra_help);
                sbuf_append(eb->hint, extra_hint);
              }
            }
          }
          while(count == 1);       
          sbuf_free(sb);
        }          
      }      
    }
  }

  if (env->hint_delay <= 0) {
    // refresh with hint directly
    edit_refresh(env, eb);
  }
}

//-------------------------------------------------------------
// Edit operations
//-------------------------------------------------------------

static void edit_history_prev(ic_env_t* env, editor_t* eb);
static void edit_history_next(ic_env_t* env, editor_t* eb);

static void edit_undo_restore(ic_env_t* env, editor_t* eb) {
  editor_undo_restore(eb, true);
  edit_refresh(env,eb);
}

static void edit_redo_restore(ic_env_t* env, editor_t* eb) {
  editor_redo_restore(eb);
  edit_refresh(env,eb);
}

static void edit_cursor_left(ic_env_t* env, editor_t* eb) {
  ssize_t cwidth = 1;
  ssize_t prev = sbuf_prev(eb->input,eb->pos,&cwidth);
  if (prev < 0) return;
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc);  
  eb->pos = prev;  
  edit_refresh(env,eb);  
}

static void edit_cursor_right(ic_env_t* env, editor_t* eb) {
  ssize_t cwidth = 1;
  ssize_t next = sbuf_next(eb->input,eb->pos,&cwidth);
  if (next < 0) return;
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc);  
  eb->pos = next;  
  edit_refresh(env,eb);
}

static void edit_cursor_line_end(ic_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;  
  eb->pos = end; 
  edit_refresh(env,eb);
}

static void edit_cursor_line_start(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_cursor_next_word(ic_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_word_end(eb->input,eb->pos);
  if (end < 0) return;
  eb->pos = end;
  edit_refresh(env,eb);
}

static void edit_cursor_prev_word(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_word_start(eb->input,eb->pos);
  if (start < 0) return;
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_cursor_next_ws_word(ic_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_ws_word_end(eb->input, eb->pos);
  if (end < 0) return;
  eb->pos = end;
  edit_refresh(env, eb);
}

static void edit_cursor_prev_ws_word(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_ws_word_start(eb->input, eb->pos);
  if (start < 0) return;
  eb->pos = start;
  edit_refresh(env, eb);
}

static void edit_cursor_to_start(ic_env_t* env, editor_t* eb) {
  eb->pos = 0; 
  edit_refresh(env,eb);
}

static void edit_cursor_to_end(ic_env_t* env, editor_t* eb) {
  eb->pos = sbuf_len(eb->input); 
  edit_refresh(env,eb);
}


static void edit_cursor_row_up(ic_env_t* env, editor_t* eb) {
  rowcol_t rc;
  edit_get_rowcol( env, eb, &rc);
  if (rc.row == 0) {
    edit_history_prev(env,eb);
  }
  else {
    edit_set_pos_at_rowcol( env, eb, rc.row - 1, rc.col );
  }
}

static void edit_cursor_row_down(ic_env_t* env, editor_t* eb) {
  rowcol_t rc;
  ssize_t rows = edit_get_rowcol( env, eb, &rc);
  if (rc.row + 1 >= rows) {
    edit_history_next(env,eb);
  }
  else {
    edit_set_pos_at_rowcol( env, eb, rc.row + 1, rc.col );
  }
}


static void edit_cursor_match_brace(ic_env_t* env, editor_t* eb) {
  ssize_t match = find_matching_brace( sbuf_string(eb->input), eb->pos, ic_env_get_match_braces(env), NULL );
  if (match < 0) return;
  eb->pos = match;
  edit_refresh(env,eb);
}

static void edit_backspace(ic_env_t* env, editor_t* eb) {
  if (eb->pos <= 0) return;
  editor_start_modify(eb);
  eb->pos = sbuf_delete_char_before(eb->input,eb->pos);
  edit_refresh(env,eb);
}

static void edit_delete_char(ic_env_t* env, editor_t* eb) {
  if (eb->pos >= sbuf_len(eb->input)) return;
  editor_start_modify(eb);
  sbuf_delete_char_at(eb->input,eb->pos);
  edit_refresh(env,eb);
}

static void edit_delete_all(ic_env_t* env, editor_t* eb) {
  if (sbuf_len(eb->input) <= 0) return;
  editor_start_modify(eb);
  sbuf_clear(eb->input);
  eb->pos = 0;
  edit_refresh(env,eb);
}

static void edit_delete_to_end_of_line(ic_env_t* env, editor_t* eb) { 
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  ssize_t end = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  // if on an empty line, remove it completely    
  if (start == end && sbuf_char_at(eb->input,end) == '\n') {
    end++;
  }
  else if (start == end && sbuf_char_at(eb->input,start - 1) == '\n') {
    eb->pos--;
  }
  sbuf_delete_from_to( eb->input, eb->pos, end );
  edit_refresh(env,eb);
}

static void edit_delete_to_start_of_line(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  ssize_t end   = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  // delete start newline if it was an empty line
  bool goright = false;
  if (start > 0 && sbuf_char_at(eb->input,start-1) == '\n' && start == end) {
    // if it is an empty line remove it
    start--;
    // afterwards, move to start of next line if it exists (so the cursor stays on the same row)
    goright = true;
  }
  sbuf_delete_from_to( eb->input, start, eb->pos );
  eb->pos = start;
  if (goright) edit_cursor_right(env,eb); 
  edit_refresh(env,eb);
}

static void edit_delete_line(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_line_start(eb->input,eb->pos);
  if (start < 0) return;
  ssize_t end   = sbuf_find_line_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  // delete newline as well so no empty line is left;
  bool goright = false;
  if (start > 0 && sbuf_char_at(eb->input,start-1) == '\n') {
    start--;
    // afterwards, move to start of next line if it exists (so the cursor stays on the same row)
    goright = true;
  }
  else if (sbuf_char_at(eb->input,end) == '\n') {
    end++;
  }
  sbuf_delete_from_to(eb->input,start,end);
  eb->pos = start;
  if (goright) edit_cursor_right(env,eb); 
  edit_refresh(env,eb);
}
 
static void edit_delete_to_start_of_word(ic_env_t* env, editor_t* eb) {
   ssize_t start = sbuf_find_word_start(eb->input,eb->pos);
  if (start < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to( eb->input, start, eb->pos );
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_delete_to_end_of_word(ic_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_word_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to( eb->input, eb->pos, end );
  edit_refresh(env,eb);
}

static void edit_delete_to_start_of_ws_word(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_ws_word_start(eb->input, eb->pos);
  if (start < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to(eb->input, start, eb->pos);
  eb->pos = start;
  edit_refresh(env, eb);
}

static void edit_delete_to_end_of_ws_word(ic_env_t* env, editor_t* eb) {
  ssize_t end = sbuf_find_ws_word_end(eb->input, eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);
  sbuf_delete_from_to(eb->input, eb->pos, end);
  edit_refresh(env, eb);
}


static void edit_delete_word(ic_env_t* env, editor_t* eb) {
  ssize_t start = sbuf_find_word_start(eb->input,eb->pos);
  if (start < 0) return;
  ssize_t end   = sbuf_find_word_end(eb->input,eb->pos);
  if (end < 0) return;
  editor_start_modify(eb);  
  sbuf_delete_from_to(eb->input,start,end);
  eb->pos = start;
  edit_refresh(env,eb);
}

static void edit_swap_char( ic_env_t* env, editor_t* eb ) { 
  if (eb->pos <= 0 || eb->pos == sbuf_len(eb->input)) return;
  editor_start_modify(eb);
  eb->pos = sbuf_swap_char(eb->input,eb->pos);
  edit_refresh(env,eb);
}

static void edit_multiline_eol(ic_env_t* env, editor_t* eb) {
  if (eb->pos <= 0) return;
  if (sbuf_string(eb->input)[eb->pos-1] != env->multiline_eol) return;
  editor_start_modify(eb);
  // replace line continuation with a real newline
  sbuf_delete_at( eb->input, eb->pos-1, 1);
  sbuf_insert_at( eb->input, "\n", eb->pos-1);  
  edit_refresh(env,eb);
}

static void edit_insert_unicode(ic_env_t* env, editor_t* eb, unicode_t u) {
  editor_start_modify(eb);
  ssize_t nextpos = sbuf_insert_unicode_at(eb->input, u, eb->pos);
  if (nextpos >= 0) eb->pos = nextpos;  
  edit_refresh_hint(env, eb);
}

static void edit_auto_brace(ic_env_t* env, editor_t* eb, char c) {
  if (env->no_autobrace) return;
  const char* braces = ic_env_get_auto_braces(env);
  for (const char* b = braces; *b != 0; b += 2) {
    if (*b == c) {
      const char close = b[1];
      //if (sbuf_char_at(eb->input, eb->pos) != close) {
        sbuf_insert_char_at(eb->input, close, eb->pos);
        bool balanced = false;
        find_matching_brace(sbuf_string(eb->input), eb->pos, braces, &balanced );
        if (!balanced) {
          // don't insert if it leads to an unbalanced expression.
          sbuf_delete_char_at(eb->input, eb->pos);
        }
      //}
      return;
    }
    else if (b[1] == c) {
      // close brace, check if there we don't overwrite to the right
      if (sbuf_char_at(eb->input, eb->pos) == c) {
        sbuf_delete_char_at(eb->input, eb->pos);
      }
      return;
    }
  }
}

static void editor_auto_indent(editor_t* eb, const char* pre, const char* post ) {
  assert(eb->pos > 0 && sbuf_char_at(eb->input,eb->pos-1) == '\n');
  ssize_t prelen = ic_strlen(pre);
  if (prelen > 0) {
    if (eb->pos - 1 < prelen) return;
    if (!ic_starts_with(sbuf_string(eb->input) + eb->pos - 1 - prelen, pre)) return;
    if (!ic_starts_with(sbuf_string(eb->input) + eb->pos, post)) return;
    eb->pos = sbuf_insert_at(eb->input, "  ", eb->pos);
    sbuf_insert_char_at(eb->input, '\n', eb->pos);
  }
}

static void edit_insert_char(ic_env_t* env, editor_t* eb, char c) {
  editor_start_modify(eb);
  ssize_t nextpos = sbuf_insert_char_at( eb->input, c, eb->pos );
  if (nextpos >= 0) eb->pos = nextpos;
  edit_auto_brace(env, eb, c);
  if (c=='\n') {
    editor_auto_indent(eb, "{", "}");  // todo: custom auto indent tokens?
  }
  edit_refresh_hint(env,eb);  
}

//-------------------------------------------------------------
// Help
//-------------------------------------------------------------

// #include "editline_help.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

//-------------------------------------------------------------
// Help: this is included into editline.c
//-------------------------------------------------------------

static const char* help[] = {
  "","Navigation:",
  "left,"
  "^b",         "go one character to the left",
  "right,"
  "^f",         "go one character to the right",
  "up",         "go one row up, or back in the history",
  "down",       "go one row down, or forward in the history",
  #ifdef __APPLE__
  "shift-left",
  #else
  "^left",
  #endif
                "go to the start of the previous word",
  #ifdef __APPLE__
  "shift-right",
  #else
  "^right",
  #endif
                "go to the end the current word",
  "home,"
  "^a",         "go to the start of the current line",
  "end,"
  "^e",         "go to the end of the current line",
  "pgup,"
  "^home",       "go to the start of the current input",
  "pgdn,"
  "^end",       "go to the end of the current input",
  "alt-m",      "jump to matching brace",
  "^p",         "go back in the history",
  "^n",         "go forward in the history",
  "^r,^s",      "search the history starting with the current word",
  "","",

  "", "Deletion:",
  "del,^d",     "delete the current character",
  "backsp,^h",  "delete the previous character",
  "^w",         "delete to preceding white space",
  "alt-backsp", "delete to the start of the current word",
  "alt-d",      "delete to the end of the current word",
  "^u",         "delete to the start of the current line",
  "^k",         "delete to the end of the current line",
  "esc",        "delete the current input, or done with empty input",
  "","",

  "", "Editing:",
  "enter",      "accept current input",
  #ifndef __APPLE__
  "^enter, ^j", "",
  "shift-tab",
  #else
  "shift-tab,^j",
  #endif
                "create a new line for multi-line input",
  //" ",          "(or type '\\' followed by enter)",
  "^l",         "clear screen",
  "^t",         "swap with previous character (move character backward)",
  "^z,^_",      "undo",
  "^y",         "redo",
  //"^C",         "done with empty input",
  //"F1",         "show this help",
  "tab",        "try to complete the current input",
  "","",
  "","In the completion menu:",
  "enter,left", "use the currently selected completion",
  "1 - 9",      "use completion N from the menu",
  "tab,down",   "select the next completion",
  "shift-tab,up","select the previous completion",
  "esc",        "exit menu without completing",
  "pgdn,^j",    "show all further possible completions",
  "","",
  "","In incremental history search:",
  "enter",      "use the currently found history entry",
  "backsp,"
  "^z",         "go back to the previous match (undo)",
  "tab,"
  "^r",         "find the next match",
  "shift-tab,"
  "^s",         "find an earlier match",
  "esc",        "exit search",
  " ","",
  NULL, NULL
};

static const char* help_initial = 
  "[ic-info]"
  "Isocline v1.0, copyright (c) 2021 Daan Leijen.\n"
  "This is free software; you can redistribute it and/or\n"
  "modify it under the terms of the MIT License.\n"
  "See <[url]https://github.com/daanx/isocline[/url]> for further information.\n"
  "We use ^<key> as a shorthand for ctrl-<key>.\n"
  "\n"
  "Overview:\n"
  "\n[ansi-lightgray]"
  "       home,ctrl-a      cursor     end,ctrl-e\n"
  "         ┌────────────────┼───────────────┐    (navigate)\n"
  //"       │                │               │\n"
  #ifndef __APPLE__
  "         │    ctrl-left   │  ctrl-right   │\n"
  #else
  "         │     alt-left   │   alt-right   │\n"
  #endif
  "         │        ┌───────┼──────┐        │    ctrl-r   : search history\n"
  "         ▼        ▼       ▼      ▼        ▼    tab      : complete word\n"
  "  prompt> [ansi-darkgray]it's the quintessential language[/]     shift-tab: insert new line\n"
  "         ▲        ▲              ▲        ▲    esc      : delete input, done\n"
  "         │        └──────────────┘        │    ctrl-z   : undo\n"
  "         │   alt-backsp        alt-d      │\n"
  //"       │                │               │\n"
  "         └────────────────────────────────┘    (delete)\n"
  "       ctrl-u                          ctrl-k\n"
  "[/ansi-lightgray][/ic-info]\n";

static void edit_show_help(ic_env_t* env, editor_t* eb) {
  edit_clear(env, eb);
  bbcode_println(env->bbcode, help_initial);
  for (ssize_t i = 0; help[i] != NULL && help[i+1] != NULL; i += 2) {
    if (help[i][0] == 0) {  
      bbcode_printf(env->bbcode, "[ic-info]%s[/]\n", help[i+1]);
    }
    else {
      bbcode_printf(env->bbcode, "  [ic-emphasis]%-13s[/][ansi-lightgray]%s%s[/]\n", help[i], (help[i+1][0] == 0 ? "" : ": "), help[i+1]);
    }
  }

  eb->cur_rows = 0;
  eb->cur_row = 0;
  edit_refresh(env, eb);
}

//-------------------------------------------------------------
// History
//-------------------------------------------------------------

// #include "editline_history.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

//-------------------------------------------------------------
// History search: this file is included in editline.c
//-------------------------------------------------------------

static void edit_history_at(ic_env_t* env, editor_t* eb, int ofs ) 
{
  if (eb->modified) { 
    history_update(env->history, sbuf_string(eb->input)); // update first entry if modified
    eb->history_idx = 0;          // and start again 
    eb->modified = false;    
  }
  const char* entry = history_get(env->history,eb->history_idx + ofs);
  // debug_msg( "edit: history: at: %d + %d, found: %s\n", eb->history_idx, ofs, entry);
  if (entry == NULL) {
    term_beep(env->term);
  }
  else {
    eb->history_idx += ofs;
    sbuf_replace(eb->input, entry);
    if (ofs > 0) {
      // at end of first line when scrolling up
      ssize_t end = sbuf_find_line_end(eb->input,0);
      eb->pos = (end < 0 ? 0 : end);
    }
    else {
      eb->pos = sbuf_len(eb->input);    // at end of last line when scrolling down
    }
    edit_refresh(env, eb);
  }
}

static void edit_history_prev(ic_env_t* env, editor_t* eb) {
  edit_history_at(env,eb, 1 );
}

static void edit_history_next(ic_env_t* env, editor_t* eb) {
  edit_history_at(env,eb, -1 );
}

typedef struct hsearch_s {
  struct hsearch_s* next;
  ssize_t hidx;
  ssize_t match_pos;
  ssize_t match_len;
  bool cinsert;
} hsearch_t;

static void hsearch_push( alloc_t* mem, hsearch_t** hs, ssize_t hidx, ssize_t mpos, ssize_t mlen, bool cinsert ) {
  hsearch_t* h = mem_zalloc_tp( mem, hsearch_t );
  if (h == NULL) return;
  h->hidx = hidx;
  h->match_pos = mpos;
  h->match_len = mlen;
  h->cinsert = cinsert;
  h->next = *hs;
  *hs = h;
}

static bool hsearch_pop( alloc_t* mem, hsearch_t** hs, ssize_t* hidx, ssize_t* match_pos, ssize_t* match_len, bool* cinsert ) {
  hsearch_t* h = *hs;
  if (h == NULL) return false;
  *hs = h->next;
  if (hidx != NULL)      *hidx = h->hidx;
  if (match_pos != NULL) *match_pos = h->match_pos;
  if (match_len != NULL) *match_len = h->match_len;
  if (cinsert != NULL)   *cinsert = h->cinsert;
  mem_free(mem, h);
  return true;
}

static void hsearch_done( alloc_t* mem, hsearch_t* hs ) {
  while (hs != NULL) {
    hsearch_t* next = hs->next;
    mem_free(mem, hs);
    hs = next;
  }
}

static void edit_history_search(ic_env_t* env, editor_t* eb, char* initial ) {
  if (history_count( env->history ) <= 0) {
    term_beep(env->term);
    return;
  }

  // update history
  if (eb->modified) { 
    history_update(env->history, sbuf_string(eb->input)); // update first entry if modified
    eb->history_idx = 0;               // and start again 
    eb->modified = false;
  }

  // set a search prompt and remember the previous state
  editor_undo_capture(eb);
  eb->disable_undo = true;
  bool old_hint = ic_enable_hint(false);  
  const char* prompt_text = eb->prompt_text;
  eb->prompt_text = "history search";
  
  // search state
  hsearch_t* hs = NULL;        // search undo 
  ssize_t hidx = 1;            // current history entry
  ssize_t match_pos = 0;       // current matched position
  ssize_t match_len = 0;       // length of the match
  const char* hentry = NULL;   // current history entry
  
  // Simulate per character searches for each letter in `initial` (so backspace works)
  if (initial != NULL) {
    const ssize_t initial_len = ic_strlen(initial);
    ssize_t ipos = 0;
    while( ipos < initial_len ) {
      ssize_t next = str_next_ofs( initial, initial_len, ipos, NULL );
      if (next < 0) break;
      hsearch_push( eb->mem, &hs, hidx, match_pos, match_len, true);
      char c = initial[ipos + next];  // terminate temporarily
      initial[ipos + next] = 0;
      if (history_search( env->history, hidx, initial, true, &hidx, &match_pos )) {
        match_len = ipos + next;
      }      
      else if (ipos + next >= initial_len) {
        term_beep(env->term);
      }
      initial[ipos + next] = c;       // restore
      ipos += next;
    }
    sbuf_replace( eb->input, initial);
    eb->pos = ipos;
  }
  else {
    sbuf_clear( eb->input );
    eb->pos = 0;
  }

  // Incremental search
again:
  hentry = history_get(env->history,hidx);
  if (hentry != NULL) {
    sbuf_appendf(eb->extra, "[ic-info]%zd. [/][ic-diminish][!pre]", hidx);
    sbuf_append_n( eb->extra, hentry, match_pos );      
    sbuf_append(eb->extra, "[/pre][u ic-emphasis][!pre]" ); 
    sbuf_append_n( eb->extra, hentry + match_pos, match_len );
    sbuf_append(eb->extra, "[/pre][/u][!pre]" ); 
    sbuf_append(eb->extra, hentry + match_pos + match_len );
    sbuf_append(eb->extra, "[/pre][/ic-diminish]");
    if (!env->no_help) {
      sbuf_append(eb->extra, "\n[ic-info](use tab for the next match)[/]");
    }
    sbuf_append(eb->extra, "\n" );
  }
  edit_refresh(env, eb);

  // Wait for input
  code_t c = (hentry == NULL ? KEY_ESC : tty_read(env->tty));
  if (tty_term_resize_event(env->tty)) {
    edit_resize(env, eb);
  }
  sbuf_clear(eb->extra);

  // Process commands
  if (c == KEY_ESC || c == KEY_BELL /* ^G */ || c == KEY_CTRL_C) {
    c = 0;  
    eb->disable_undo = false;
    editor_undo_restore(eb, false);
  } 
  else if (c == KEY_ENTER) {
    c = 0;
    editor_undo_forget(eb);
    sbuf_replace( eb->input, hentry );
    eb->pos = sbuf_len(eb->input);
    eb->modified = false;
    eb->history_idx = hidx;
  }  
  else if (c == KEY_BACKSP || c == KEY_CTRL_Z) {
    // undo last search action
    bool cinsert;
    if (hsearch_pop(env->mem,&hs, &hidx, &match_pos, &match_len, &cinsert)) {
      if (cinsert) edit_backspace(env,eb);
    }
    goto again;
  }
  else if (c == KEY_CTRL_R || c == KEY_TAB || c == KEY_UP) {    
    // search backward
    hsearch_push(env->mem, &hs, hidx, match_pos, match_len, false);
    if (!history_search( env->history, hidx+1, sbuf_string(eb->input), true, &hidx, &match_pos )) {
      hsearch_pop(env->mem,&hs,NULL,NULL,NULL,NULL);
      term_beep(env->term);
    };
    goto again;
  }  
  else if (c == KEY_CTRL_S || c == KEY_SHIFT_TAB || c == KEY_DOWN) {    
    // search forward
    hsearch_push(env->mem, &hs, hidx, match_pos, match_len, false);
    if (!history_search( env->history, hidx-1, sbuf_string(eb->input), false, &hidx, &match_pos )) {
      hsearch_pop(env->mem, &hs,NULL,NULL,NULL,NULL);
      term_beep(env->term);
    };
    goto again;
  }
  else if (c == KEY_F1) {
    edit_show_help(env, eb);
    goto again;
  }
  else {
    // insert character and search further backward
    char chr;
    unicode_t uchr;
    if (code_is_ascii_char(c,&chr)) {
      hsearch_push(env->mem, &hs, hidx, match_pos, match_len, true);
      edit_insert_char(env,eb,chr);      
    }
    else if (code_is_unicode(c,&uchr)) {
      hsearch_push(env->mem, &hs, hidx, match_pos, match_len, true);
      edit_insert_unicode(env,eb,uchr);
    }
    else {
      // ignore command
      term_beep(env->term);
      goto again;
    }
    // search for the new input
    if (history_search( env->history, hidx, sbuf_string(eb->input), true, &hidx, &match_pos )) {
      match_len = sbuf_len(eb->input);
    }
    else {
      term_beep(env->term);
    };
    goto again;
  }

  // done
  eb->disable_undo = false;
  hsearch_done(env->mem,hs);
  eb->prompt_text = prompt_text;
  ic_enable_hint(old_hint);
  edit_refresh(env,eb);
  if (c != 0) tty_code_pushback(env->tty, c);
}

// Start an incremental search with the current word 
static void edit_history_search_with_current_word(ic_env_t* env, editor_t* eb) {
  char* initial = NULL;
  ssize_t start = sbuf_find_word_start( eb->input, eb->pos );
  if (start >= 0) {
    const ssize_t next = sbuf_next(eb->input, start, NULL);
    if (!ic_char_is_idletter(sbuf_string(eb->input) + start, (long)(next - start))) { 
      start = next; 
    }
    if (start >= 0 && start < eb->pos) {
      initial = mem_strndup(eb->mem, sbuf_string(eb->input) + start, eb->pos - start);
    }
  }
  edit_history_search( env, eb, initial);
  mem_free(env->mem, initial);
}

//-------------------------------------------------------------
// Completion
//-------------------------------------------------------------

// #include "editline_completion.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

//-------------------------------------------------------------
// Completion menu: this file is included in editline.c
//-------------------------------------------------------------

// return true if anything changed
static bool edit_complete(ic_env_t* env, editor_t* eb, ssize_t idx) {
  editor_start_modify(eb);
  ssize_t newpos = completions_apply(env->completions, idx, eb->input, eb->pos);
  if (newpos < 0) {
    editor_undo_restore(eb,false);
    return false;
  }
  eb->pos = newpos;
  edit_refresh(env,eb);  
  return true;
}

static bool edit_complete_longest_prefix(ic_env_t* env, editor_t* eb ) {
  editor_start_modify(eb);
  ssize_t newpos = completions_apply_longest_prefix( env->completions, eb->input, eb->pos );
  if (newpos < 0) {
    editor_undo_restore(eb,false);
    return false;
  }
  eb->pos = newpos;
  edit_refresh(env,eb);
  return true;
}

ic_private void sbuf_append_tagged( stringbuf_t* sb, const char* tag, const char* content ) {
  sbuf_appendf(sb, "[%s]", tag);  
  sbuf_append(sb,content);
  sbuf_append(sb,"[/]");
}

static void editor_append_completion(ic_env_t* env, editor_t* eb, ssize_t idx, ssize_t width, bool numbered, bool selected ) {
  const char* help = NULL;
  const char* display = completions_get_display(env->completions, idx, &help);
  if (display == NULL) return;
  if (numbered) {
    sbuf_appendf(eb->extra, "[ic-info]%s%zd [/]", (selected ? (tty_is_utf8(env->tty) ? "\xE2\x86\x92" : "*") : " "), 1 + idx);
    width -= 3;
  }

  if (width > 0) {
    sbuf_appendf(eb->extra, "[width=\"%zd;left; ;on\"]", width );
  }
  if (selected) {
    sbuf_append(eb->extra, "[ic-emphasis]");
  }
  sbuf_append(eb->extra, display);
  if (selected) { sbuf_append(eb->extra,"[/ic-emphasis]"); }
  if (help != NULL) {
    sbuf_append(eb->extra, "  ");
    sbuf_append_tagged(eb->extra, "ic-info", help );      
  }
  if (width > 0) { sbuf_append(eb->extra,"[/width]"); }  
}

// 2 and 3 column output up to 80 wide
#define IC_DISPLAY2_MAX    34
#define IC_DISPLAY2_COL    (3+IC_DISPLAY2_MAX)
#define IC_DISPLAY2_WIDTH  (2*IC_DISPLAY2_COL + 2)    // 75

#define IC_DISPLAY3_MAX    21
#define IC_DISPLAY3_COL    (3+IC_DISPLAY3_MAX)
#define IC_DISPLAY3_WIDTH  (3*IC_DISPLAY3_COL + 2*2)  // 76

static void editor_append_completion2(ic_env_t* env, editor_t* eb, ssize_t col_width, ssize_t idx1, ssize_t idx2, ssize_t selected ) {  
  editor_append_completion(env, eb, idx1, col_width, true, (idx1 == selected) );
  sbuf_append( eb->extra, "  ");
  editor_append_completion(env, eb, idx2, col_width, true, (idx2 == selected) );
}

static void editor_append_completion3(ic_env_t* env, editor_t* eb, ssize_t col_width, ssize_t idx1, ssize_t idx2, ssize_t idx3, ssize_t selected ) {  
  editor_append_completion(env, eb, idx1, col_width, true, (idx1 == selected) );
  sbuf_append( eb->extra, "  ");
  editor_append_completion(env, eb, idx2, col_width, true, (idx2 == selected));
  sbuf_append( eb->extra, "  ");
  editor_append_completion(env, eb, idx3, col_width, true, (idx3 == selected) );
}

static ssize_t edit_completions_max_width( ic_env_t* env, ssize_t count ) {
  ssize_t max_width = 0;
  for( ssize_t i = 0; i < count; i++) {
    const char* help = NULL;
    ssize_t w = bbcode_column_width(env->bbcode, completions_get_display(env->completions, i, &help));
    if (help != NULL) {
      w += 2 + bbcode_column_width(env->bbcode, help);
    }
    if (w > max_width) {
      max_width = w;
    }
  }
  return max_width;
}

static void edit_completion_menu(ic_env_t* env, editor_t* eb, bool more_available) {
  ssize_t count = completions_count(env->completions);
  ssize_t count_displayed = count;
  assert(count > 1);
  ssize_t selected = (env->complete_nopreview ? 0 : -1); // select first or none
  ssize_t percolumn = count;

again:
  // show first 9 (or 8) completions
  sbuf_clear(eb->extra);
  ssize_t twidth = term_get_width(env->term) - 1;
  ssize_t colwidth;
  if (count > 3 && ((colwidth = 3 + edit_completions_max_width(env, 9))*3 + 2*2) < twidth) {
    // display as a 3 column block
    count_displayed = (count > 9 ? 9 : count);
    percolumn = 3;
    for (ssize_t rw = 0; rw < percolumn; rw++) {
      if (rw > 0) sbuf_append(eb->extra, "\n");
      editor_append_completion3(env, eb, colwidth, rw, percolumn+rw, (2*percolumn)+rw, selected);
    }
  }
  else if (count > 4 && ((colwidth = 3 + edit_completions_max_width(env, 8))*2 + 2) < twidth) {
    // display as a 2 column block if some entries are too wide for three columns
    count_displayed = (count > 8 ? 8 : count);
    percolumn = (count_displayed <= 6 ? 3 : 4);
    for (ssize_t rw = 0; rw < percolumn; rw++) {
      if (rw > 0) sbuf_append(eb->extra, "\n");
      editor_append_completion2(env, eb, colwidth, rw, percolumn+rw, selected);
    }
  }
  else {
    // display as a list
    count_displayed = (count > 9 ? 9 : count);
    percolumn = count_displayed;
    for (ssize_t i = 0; i < count_displayed; i++) {
      if (i > 0) sbuf_append(eb->extra, "\n");
      editor_append_completion(env, eb, i, -1, true /* numbered */, selected == i);
    }
  }
  if (count > count_displayed) {
    if (more_available) {
      sbuf_append(eb->extra, "\n[ic-info](press page-down (or ctrl-j) to see all further completions)[/]");
    }
    else {
      sbuf_appendf(eb->extra, "\n[ic-info](press page-down (or ctrl-j) to see all %zd completions)[/]", count );
    }
  }
  if (!env->complete_nopreview && selected >= 0 && selected <= count_displayed) {
    edit_complete(env,eb,selected);
    editor_undo_restore(eb,false);
  }
  else {
    edit_refresh(env, eb);
  }

  // read here; if not a valid key, push it back and return to main event loop
  code_t c = tty_read(env->tty);
  if (tty_term_resize_event(env->tty)) {
    edit_resize(env, eb);
  }
  sbuf_clear(eb->extra);
  
  // direct selection?
  if (c >= '1' && c <= '9') {
    ssize_t i = (c - '1');
    if (i < count) {
      selected = i;
      c = KEY_ENTER;
    }
  }

  // process commands
  if (c == KEY_DOWN || c == KEY_TAB) {
    selected++;
    if (selected >= count_displayed) {
      //term_beep(env->term);
      selected = 0;
    }
    goto again;
  }
  else if (c == KEY_UP || c == KEY_SHIFT_TAB) {
    selected--;
    if (selected < 0) {
      selected = count_displayed - 1;
      //term_beep(env->term);
    }
    goto again;
  }
  else if (c == KEY_F1) {
    edit_show_help(env, eb);
    goto again;
  }
  else if (c == KEY_ESC) {
    completions_clear(env->completions);
    edit_refresh(env,eb);
    c = 0; // ignore and return
  }
  else if (selected >= 0 && (c == KEY_ENTER || c == KEY_RIGHT || c == KEY_END)) /* || c == KEY_TAB*/ {  
    // select the current entry
    assert(selected < count);
    c = 0;      
    edit_complete(env, eb, selected);    
    if (env->complete_autotab) {
      tty_code_pushback(env->tty,KEY_EVENT_AUTOTAB); // immediately try to complete again        
    }
  }
  else if (!env->complete_nopreview && !code_is_virt_key(c)) {
    // if in preview mode, select the current entry and exit the menu
    assert(selected < count);
    edit_complete(env, eb, selected); 
  }
  else if ((c == KEY_PAGEDOWN || c == KEY_LINEFEED) && count > 9) {
    // show all completions
    c = 0;
    if (more_available) {
      // generate all entries (up to the max (= 1000))
      count = completions_generate(env, env->completions, sbuf_string(eb->input), eb->pos, IC_MAX_COMPLETIONS_TO_SHOW);
    }
    rowcol_t rc;
    edit_get_rowcol(env,eb,&rc);
    edit_clear(env,eb);
    edit_write_prompt(env,eb,0,false);
    term_writeln(env->term, "");
    for(ssize_t i = 0; i < count; i++) {
      const char* display = completions_get_display(env->completions, i, NULL);
      if (display != NULL) {
        bbcode_println(env->bbcode, display);
      }
    }
    if (count >= IC_MAX_COMPLETIONS_TO_SHOW) {
      bbcode_println(env->bbcode, "[ic-info]... and more.[/]");
    }
    else {
      bbcode_printf(env->bbcode, "[ic-info](%zd possible completions)[/]\n", count );
    }
    for(ssize_t i = 0; i < rc.row+1; i++) {
      term_write(env->term, " \n");
    }
    eb->cur_rows = 0;
    edit_refresh(env,eb);      
  }
  else {
    edit_refresh(env,eb);
  }
  // done
  completions_clear(env->completions);      
  if (c != 0) tty_code_pushback(env->tty,c);
}

static void edit_generate_completions(ic_env_t* env, editor_t* eb, bool autotab) {
  debug_msg( "edit: complete: %zd: %s\n", eb->pos, sbuf_string(eb->input) );
  if (eb->pos < 0) return;
  ssize_t count = completions_generate(env, env->completions, sbuf_string(eb->input), eb->pos, IC_MAX_COMPLETIONS_TO_TRY);
  bool more_available = (count >= IC_MAX_COMPLETIONS_TO_TRY);
  if (count <= 0) {
    // no completions
    if (!autotab) { term_beep(env->term); }
  }
  else if (count == 1) {
    // complete if only one match    
    if (edit_complete(env,eb,0 /*idx*/) && env->complete_autotab) {
      tty_code_pushback(env->tty,KEY_EVENT_AUTOTAB);
    }    
  }
  else {
    //term_beep(env->term); 
    if (!more_available) { 
      edit_complete_longest_prefix(env,eb);
    }    
    completions_sort(env->completions);
    edit_completion_menu( env, eb, more_available);    
  }
}


//-------------------------------------------------------------
// Edit line: main edit loop
//-------------------------------------------------------------

static char* edit_line( ic_env_t* env, const char* prompt_text )
{
  // set up an edit buffer
  editor_t eb;
  memset(&eb, 0, sizeof(eb));
  eb.mem      = env->mem;
  eb.input    = sbuf_new(env->mem);
  eb.extra    = sbuf_new(env->mem);
  eb.hint     = sbuf_new(env->mem);
  eb.hint_help= sbuf_new(env->mem);
  eb.termw    = term_get_width(env->term);  
  eb.pos      = 0;
  eb.cur_rows = 1; 
  eb.cur_row  = 0; 
  eb.modified = false;  
  eb.prompt_text   = (prompt_text != NULL ? prompt_text : "");
  eb.history_idx   = 0;  
  editstate_init(&eb.undo);
  editstate_init(&eb.redo);
  if (eb.input==NULL || eb.extra==NULL || eb.hint==NULL || eb.hint_help==NULL) {
    return NULL;
  }

  // caching
  if (!(env->no_highlight && env->no_bracematch)) {
    eb.attrs = attrbuf_new(env->mem);
    eb.attrs_extra = attrbuf_new(env->mem);
  }
  
  // show prompt
  edit_write_prompt(env, &eb, 0, false);   

  // always a history entry for the current input
  history_push(env->history, "");

  // process keys
  code_t c;          // current key code
  while(true) {    
    // read a character
    term_flush(env->term);
    if (env->hint_delay <= 0 || sbuf_len(eb.hint) == 0) {
      // blocking read
      c = tty_read(env->tty);
    }
    else {
      // timeout to display hint
      if (!tty_read_timeout(env->tty, env->hint_delay, &c)) {
        // timed-out
        if (sbuf_len(eb.hint) > 0) {
          // display hint
          edit_refresh(env, &eb);
        }
        c = tty_read(env->tty);
      }
      else {
        // clear the pending hint if we got input before the delay expired
        sbuf_clear(eb.hint);
        sbuf_clear(eb.hint_help);
      }
    }
    
    // update terminal in case of a resize
    if (tty_term_resize_event(env->tty)) {
      edit_resize(env,&eb);            
    }

    // clear hint only after a potential resize (so resize row calculations are correct)
    const bool had_hint = (sbuf_len(eb.hint) > 0);
    sbuf_clear(eb.hint);
    sbuf_clear(eb.hint_help);

    // if the user tries to move into a hint with left-cursor or end, we complete it first
    if ((c == KEY_RIGHT || c == KEY_END) && had_hint) {
      edit_generate_completions(env, &eb, true);
      c = KEY_NONE;      
    }

    // Operations that may return
    if (c == KEY_ENTER) {
      if (!env->singleline_only && eb.pos > 0 && 
           sbuf_string(eb.input)[eb.pos-1] == env->multiline_eol && 
            edit_pos_is_at_row_end(env,&eb)) 
      {
        // replace line-continuation with newline
        edit_multiline_eol(env,&eb);        
      }
      else {
        // otherwise done
        break;
      }
    } 
    else if (c == KEY_CTRL_D) {
      if (eb.pos == 0 && editor_pos_is_at_end(&eb)) break; // ctrl+D on empty quits with NULL
      edit_delete_char(env,&eb);     // otherwise it is like delete
    } 
    else if (c == KEY_CTRL_C || c == KEY_EVENT_STOP) {
      break; // ctrl+C or STOP event quits with NULL
    }
    else if (c == KEY_ESC) {
      if (eb.pos == 0 && editor_pos_is_at_end(&eb)) break;  // ESC on empty input returns with empty input
      edit_delete_all(env,&eb);      // otherwise delete the current input
      // edit_delete_line(env,&eb);  // otherwise delete the current line
    }
    else if (c == KEY_BELL /* ^G */) {
      edit_delete_all(env,&eb);
      break; // ctrl+G cancels (and returns empty input)
    }

    // Editing Operations
    else switch(c) {
      // events
      case KEY_EVENT_RESIZE:  // not used
        edit_resize(env,&eb);
        break;
      case KEY_EVENT_AUTOTAB:
        edit_generate_completions(env, &eb, true);
        break;

      // completion, history, help, undo
      case KEY_TAB:
      case WITH_ALT('?'):
        edit_generate_completions(env,&eb,false);
        break;
      case KEY_CTRL_R:
      case KEY_CTRL_S:
        edit_history_search_with_current_word(env,&eb);
        break;
      case KEY_CTRL_P:
        edit_history_prev(env, &eb);
        break;
      case KEY_CTRL_N:
        edit_history_next(env, &eb);
        break;
      case KEY_CTRL_L:
        edit_clear_screen(env, &eb);
        break;
      case KEY_CTRL_Z:
      case WITH_CTRL('_'):
        edit_undo_restore(env, &eb);
        break;
      case KEY_CTRL_Y:
        edit_redo_restore(env, &eb);
        break;
      case KEY_F1:
        edit_show_help(env, &eb);
        break;

      // navigation
      case KEY_LEFT:
      case KEY_CTRL_B:
        edit_cursor_left(env,&eb);
        break;
      case KEY_RIGHT:
      case KEY_CTRL_F:
        if (eb.pos == sbuf_len(eb.input)) { 
          edit_generate_completions( env, &eb, false );
        }
        else {
          edit_cursor_right(env,&eb);
        }
        break;
      case KEY_UP:
        edit_cursor_row_up(env,&eb);
        break;
      case KEY_DOWN:
        edit_cursor_row_down(env,&eb);
        break;                 
      case KEY_HOME:
      case KEY_CTRL_A:
        edit_cursor_line_start(env,&eb);
        break;
      case KEY_END:
      case KEY_CTRL_E:
        edit_cursor_line_end(env,&eb);
        break;
      case KEY_CTRL_LEFT:
      case WITH_SHIFT(KEY_LEFT):    
      case WITH_ALT('b'):
        edit_cursor_prev_word(env,&eb);
        break;
      case KEY_CTRL_RIGHT:
      case WITH_SHIFT(KEY_RIGHT):      
      case WITH_ALT('f'):
        if (eb.pos == sbuf_len(eb.input)) { 
          edit_generate_completions( env, &eb, false );
        }
        else {
          edit_cursor_next_word(env,&eb);
        }
        break;      
      case KEY_CTRL_HOME:
      case WITH_SHIFT(KEY_HOME):      
      case KEY_PAGEUP:
      case WITH_ALT('<'):
        edit_cursor_to_start(env,&eb);
        break;
      case KEY_CTRL_END:
      case WITH_SHIFT(KEY_END):      
      case KEY_PAGEDOWN:
      case WITH_ALT('>'):
        edit_cursor_to_end(env,&eb);
        break;
      case WITH_ALT('m'):
        edit_cursor_match_brace(env,&eb);
        break;

      // deletion
      case KEY_BACKSP:
        edit_backspace(env,&eb);
        break;
      case KEY_DEL:
        edit_delete_char(env,&eb);
        break;
      case WITH_ALT('d'):
        edit_delete_to_end_of_word(env,&eb);
        break;
      case KEY_CTRL_W:
        edit_delete_to_start_of_ws_word(env, &eb);
        break;
      case WITH_ALT(KEY_DEL):
      case WITH_ALT(KEY_BACKSP):
        edit_delete_to_start_of_word(env,&eb);
        break;      
      case KEY_CTRL_U:
        edit_delete_to_start_of_line(env,&eb);
        break;
      case KEY_CTRL_K:
        edit_delete_to_end_of_line(env,&eb);
        break;
      case KEY_CTRL_T:
        edit_swap_char(env,&eb);
        break;

      // Editing
      case KEY_SHIFT_TAB:
      case KEY_LINEFEED: // '\n' (ctrl+J, shift+enter)
        if (!env->singleline_only) { 
          edit_insert_char(env, &eb, '\n'); 
        }
        break;
      default: {
        char chr;
        unicode_t uchr;
        if (code_is_ascii_char(c,&chr)) {
          edit_insert_char(env,&eb,chr);
        }
        else if (code_is_unicode(c, &uchr)) {
          edit_insert_unicode(env,&eb, uchr);
        }
        else {
          debug_msg( "edit: ignore code: 0x%04x\n", c);
        }
        break;
      }
    }

  }

  // goto end
  eb.pos = sbuf_len(eb.input);

  // refresh once more but without brace matching
  bool bm = env->no_bracematch;
  env->no_bracematch = true;
  edit_refresh(env,&eb);
  env->no_bracematch = bm;
  
  // save result
  char* res; 
  if ((c == KEY_CTRL_D && sbuf_len(eb.input) == 0) || c == KEY_CTRL_C || c == KEY_EVENT_STOP) {
    res = NULL;
  }
  else if (!tty_is_utf8(env->tty)) {
    res = sbuf_strdup_from_utf8(eb.input);
  }
  else {
    res = sbuf_strdup(eb.input);
  }

  // update history
  history_update(env->history, sbuf_string(eb.input));
  if (res == NULL || sbuf_len(eb.input) <= 1) { ic_history_remove_last(); } // no empty or single-char entries
  history_save(env->history);

  // free resources 
  editstate_done(env->mem, &eb.undo);
  editstate_done(env->mem, &eb.redo);
  attrbuf_free(eb.attrs);
  attrbuf_free(eb.attrs_extra);
  sbuf_free(eb.input);
  sbuf_free(eb.extra);
  sbuf_free(eb.hint);
  sbuf_free(eb.hint_help);

  return res;
}

// # include "highlight.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#include <string.h>
// skipping dup #include "common.h"
// skipping dup #include "term.h"
// skipping dup #include "stringbuf.h"
// skipping dup #include "attr.h"
// skipping dup #include "bbcode.h"

//-------------------------------------------------------------
// Syntax highlighting
//-------------------------------------------------------------

struct ic_highlight_env_s {
  attrbuf_t*    attrs;
  const char*   input;   
  ssize_t       input_len;     
  bbcode_t*     bbcode;
  alloc_t*      mem;
  ssize_t       cached_upos;  // cached unicode position
  ssize_t       cached_cpos;  // corresponding utf-8 byte position
};


ic_private void highlight( alloc_t* mem, bbcode_t* bb, const char* s, attrbuf_t* attrs, ic_highlight_fun_t* highlighter, void* arg ) {
  const ssize_t len = ic_strlen(s);
  if (len <= 0) return;
  attrbuf_set_at(attrs,0,len,attr_none()); // fill to length of s
  if (highlighter != NULL) {
    ic_highlight_env_t henv;
    henv.attrs = attrs;
    henv.input = s;     
    henv.input_len = len;
    henv.bbcode = bb;
    henv.mem = mem;
    henv.cached_cpos = 0;
    henv.cached_upos = 0;
    (*highlighter)( &henv, s, arg );    
  }
}


//-------------------------------------------------------------
// Client interface
//-------------------------------------------------------------

static void pos_adjust( ic_highlight_env_t* henv, ssize_t* ppos, ssize_t* plen ) {
  ssize_t pos = *ppos;
  ssize_t len = *plen;
  if (pos >= henv->input_len) return;
  if (pos >= 0 && len >= 0) return;   // already character positions
  if (henv->input == NULL) return;

  if (pos < 0) {
    // negative `pos` is used as the unicode character position (for easy interfacing with Haskell)
    ssize_t upos = -pos;
    ssize_t cpos = 0;
    ssize_t ucount = 0;
    if (henv->cached_upos <= upos) {  // if we have a cached position, start from there
      ucount = henv->cached_upos;
      cpos = henv->cached_cpos;
    }
    while ( ucount < upos ) {
      ssize_t next = str_next_ofs(henv->input, henv->input_len, cpos, NULL);
      if (next <= 0) return;
      ucount++;
      cpos += next;
    }
    *ppos = pos = cpos;
    // and cache it to avoid quadratic behavior
    henv->cached_upos = upos;
    henv->cached_cpos = cpos;
  }
  if (len < 0) {
    // negative `len` is used as a unicode character length
    len = -len;
    ssize_t ucount = 0;
    ssize_t clen   = 0;    
    while (ucount < len) {
      ssize_t next = str_next_ofs(henv->input, henv->input_len, pos + clen, NULL);
      if (next <= 0) return;
      ucount++;
      clen += next;
    }
    *plen = len = clen;
    // and update cache if possible
    if (henv->cached_cpos == pos) {
      henv->cached_upos += ucount;
      henv->cached_cpos += clen;
    }
  } 
}

static void highlight_attr(ic_highlight_env_t* henv, ssize_t pos, ssize_t count, attr_t attr ) {
  if (henv==NULL) return;
  pos_adjust(henv,&pos,&count);
  if (pos < 0 || count <= 0) return;
  attrbuf_update_at(henv->attrs, pos, count, attr);
}

ic_public void ic_highlight(ic_highlight_env_t* henv, long pos, long count, const char* style ) {
  if (henv == NULL || style==NULL || style[0]==0 || pos < 0) return;  
  highlight_attr(henv,pos,count,bbcode_style( henv->bbcode, style ));
}

ic_public void ic_highlight_formatted(ic_highlight_env_t* henv, const char* s, const char* fmt) {
  if (s==NULL || s[0] == 0 || fmt==NULL) return;
  attrbuf_t* attrs = attrbuf_new(henv->mem);
  stringbuf_t* out = sbuf_new(henv->mem);  // todo: avoid allocating out?
  if (attrs!=NULL && out != NULL) {
    bbcode_append( henv->bbcode, fmt, out, attrs);
    const ssize_t len = ic_strlen(s);
    if (sbuf_len(out) != len) {
      debug_msg("highlight: formatted string content differs from the original input:\n  original: %s\n  formatted: %s\n", s, fmt);
    }
    for( ssize_t i = 0; i < len; i++) {
      attrbuf_update_at(henv->attrs, i, 1, attrbuf_attr_at(attrs,i));
    }
  }
  sbuf_free(out);
  attrbuf_free(attrs);
}

//-------------------------------------------------------------
// Brace matching
//-------------------------------------------------------------
#define MAX_NESTING (64)

typedef struct brace_s {
  char    close;
  bool    at_cursor;
  ssize_t pos;
} brace_t;

ic_private void highlight_match_braces(const char* s, attrbuf_t* attrs, ssize_t cursor_pos, const char* braces, attr_t match_attr, attr_t error_attr) 
{
  brace_t open[MAX_NESTING+1];
  ssize_t nesting = 0;
  const ssize_t brace_len = ic_strlen(braces);
  for (long i = 0; i < ic_strlen(s); i++) {
    const char c = s[i];
    // push open brace
    bool found_open = false;
    for (ssize_t b = 0; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // open brace
        if (nesting >= MAX_NESTING) return; // give up
        open[nesting].close = braces[b+1];
        open[nesting].pos = i;
        open[nesting].at_cursor = (i == cursor_pos - 1);
        nesting++;
        found_open = true;
        break;
      }
    }
    if (found_open) continue;

    // pop to closing brace and potentially highlight
    for (ssize_t b = 1; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // close brace
        if (nesting <= 0) {
          // unmatched close brace
          attrbuf_update_at( attrs, i, 1, error_attr);
        }
        else {
          // can we fix an unmatched brace where we can match by popping just one?
          if (open[nesting-1].close != c && nesting > 1 && open[nesting-2].close == c) {
            // assume previous open brace was wrong
            attrbuf_update_at(attrs, open[nesting-1].pos, 1, error_attr);
            nesting--;
          }
          if (open[nesting-1].close != c) {
            // unmatched open brace
            attrbuf_update_at( attrs, i, 1, error_attr);
          }
          else {
            // matching brace
            nesting--;
            if (i == cursor_pos - 1 || (open[nesting].at_cursor && open[nesting].pos != i - 1)) {
              // highlight matching brace
              attrbuf_update_at(attrs, open[nesting].pos, 1, match_attr);
              attrbuf_update_at(attrs, i, 1, match_attr);              
            }
          }
        }
        break;
      }
    }
  }
  // note: don't mark further unmatched open braces as in error
}


ic_private ssize_t find_matching_brace(const char* s, ssize_t cursor_pos, const char* braces, bool* is_balanced) 
{
  if (is_balanced != NULL) { *is_balanced = false; }
  bool balanced = true;
  ssize_t match = -1;
  brace_t open[MAX_NESTING+1];
  ssize_t nesting = 0;
  const ssize_t brace_len = ic_strlen(braces);
  for (long i = 0; i < ic_strlen(s); i++) {
    const char c = s[i];
    // push open brace
    bool found_open = false;
    for (ssize_t b = 0; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // open brace
        if (nesting >= MAX_NESTING) return -1; // give up
        open[nesting].close = braces[b+1];
        open[nesting].pos = i;
        open[nesting].at_cursor = (i == cursor_pos - 1);
        nesting++;
        found_open = true;
        break;
      }
    }
    if (found_open) continue;

    // pop to closing brace 
    for (ssize_t b = 1; b < brace_len; b += 2) {
      if (c == braces[b]) {
        // close brace
        if (nesting <= 0) {
          // unmatched close brace
          balanced = false;          
        }
        else {
          if (open[nesting-1].close != c) {
            // unmatched open brace
            balanced = false;
          }
          else {
            // matching brace
            nesting--;
            if (i == cursor_pos - 1) {
              // found matching open brace
              match = open[nesting].pos + 1;
            }
            else if (open[nesting].at_cursor) {
              // found matching close brace
              match = i + 1;
            }
          }
        }
        break; 
      }
    }
  }
  if (nesting != 0) { balanced = false; }
  if (is_balanced != NULL) { *is_balanced = balanced; }
  return match;
}
// # include "undo.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>

// skipping dup #include "../include/isocline.h"
// skipping dup #include "common.h"
// skipping dup #include "env.h"
// skipping dup #include "stringbuf.h"
// skipping dup #include "completions.h"
// skipping dup #include "undo.h"



//-------------------------------------------------------------
// edit state
//-------------------------------------------------------------
struct editstate_s {
  struct editstate_s* next;
  const char* input;          // input
  ssize_t     pos;            // cursor position
};

ic_private void editstate_init( editstate_t** es ) {
  *es = NULL;
}

ic_private void editstate_done( alloc_t* mem, editstate_t** es ) {
  while (*es != NULL) {
    editstate_t* next = (*es)->next;
    mem_free(mem, (*es)->input);
    mem_free(mem, *es );
    *es = next;
  }
  *es = NULL;
}

ic_private void editstate_capture( alloc_t* mem, editstate_t** es, const char* input, ssize_t pos) {
  if (input==NULL) input = "";
  // alloc
  editstate_t* entry = mem_zalloc_tp(mem, editstate_t);
  if (entry == NULL) return;
  // initialize
  entry->input = mem_strdup( mem, input);
  entry->pos   = pos;
  if (entry->input == NULL) { mem_free(mem, entry); return; }
  // and push
  entry->next = *es;
  *es = entry;
}

// caller should free *input
ic_private bool editstate_restore( alloc_t* mem, editstate_t** es, const char** input, ssize_t* pos ) {
  if (*es == NULL) return false;
  // pop 
  editstate_t* entry = *es;
  *es = entry->next;
  *input = entry->input;
  *pos = entry->pos;
  mem_free(mem, entry);
  return true;
}

// # include "history.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <string.h>  
#include <sys/stat.h>

// skipping dup #include "../include/isocline.h"
// skipping dup #include "common.h"
// skipping dup #include "history.h"
// skipping dup #include "stringbuf.h"

#define IC_MAX_HISTORY (200)

struct history_s {
  ssize_t  count;              // current number of entries in use
  ssize_t  len;                // size of elems 
  const char** elems;         // history items (up to count)
  const char*  fname;         // history file
  alloc_t* mem;
  bool     allow_duplicates;   // allow duplicate entries?
};

ic_private history_t* history_new(alloc_t* mem) {
  history_t* h = mem_zalloc_tp(mem,history_t);
  h->mem = mem;
  return h;
}

ic_private void history_free(history_t* h) {
  if (h == NULL) return;
  history_clear(h);
  if (h->len > 0) {
    mem_free( h->mem, h->elems );
    h->elems = NULL;
    h->len = 0;
  }
  mem_free(h->mem, h->fname);
  h->fname = NULL;
  mem_free(h->mem, h); // free ourselves
}

ic_private bool history_enable_duplicates( history_t* h, bool enable ) {
  bool prev = h->allow_duplicates;
  h->allow_duplicates = enable;
  return prev;
}

ic_private ssize_t  history_count(const history_t* h) {
  return h->count;
}

//-------------------------------------------------------------
// push/clear
//-------------------------------------------------------------

ic_private bool history_update( history_t* h, const char* entry ) {
  if (entry==NULL) return false;
  history_remove_last(h);
  history_push(h,entry);
  //debug_msg("history: update: with %s; now at %s\n", entry, history_get(h,0));
  return true;
}

static void history_delete_at( history_t* h, ssize_t idx ) {
  if (idx < 0 || idx >= h->count) return;
  mem_free(h->mem, h->elems[idx]);
  for(ssize_t i = idx+1; i < h->count; i++) {
    h->elems[i-1] = h->elems[i];
  }
  h->count--;
}

ic_private bool history_push( history_t* h, const char* entry ) {
  if (h->len <= 0 || entry==NULL)  return false;
  // remove any older duplicate
  if (!h->allow_duplicates) {
    for( int i = 0; i < h->count; i++) {
      if (strcmp(h->elems[i],entry) == 0) {
        history_delete_at(h,i);
      }
    }
  }
  // insert at front
  if (h->count == h->len) {
    // delete oldest entry
    history_delete_at(h,0);    
  }
  assert(h->count < h->len);
  h->elems[h->count] = mem_strdup(h->mem,entry);
  h->count++;
  return true;
}


static void history_remove_last_n( history_t* h, ssize_t n ) {
  if (n <= 0) return;
  if (n > h->count) n = h->count;
  for( ssize_t i = h->count - n; i < h->count; i++) {
    mem_free( h->mem, h->elems[i] );
  }
  h->count -= n;
  assert(h->count >= 0);    
}

ic_private void history_remove_last(history_t* h) {
  history_remove_last_n(h,1);
}

ic_private void history_clear(history_t* h) {
  history_remove_last_n( h, h->count );
}

ic_private const char* history_get( const history_t* h, ssize_t n ) {
  if (n < 0 || n >= h->count) return NULL;
  return h->elems[h->count - n - 1];
}

ic_private bool history_search( const history_t* h, ssize_t from /*including*/, const char* search, bool backward, ssize_t* hidx, ssize_t* hpos ) {
  const char* p = NULL;
  ssize_t i;
  if (backward) {
    for( i = from; i < h->count; i++ ) {
      p = strstr( history_get(h,i), search);
      if (p != NULL) break;
    }
  }
  else {
    for( i = from; i >= 0; i-- ) {
      p = strstr( history_get(h,i), search);
      if (p != NULL) break;
    }
  }
  if (p == NULL) return false;
  if (hidx != NULL) *hidx = i;
  if (hpos != NULL) *hpos = (p - history_get(h,i));
  return true;
}

//-------------------------------------------------------------
// 
//-------------------------------------------------------------

ic_private void history_load_from(history_t* h, const char* fname, long max_entries ) {
  history_clear(h);
  h->fname = mem_strdup(h->mem,fname);
  if (max_entries == 0) {
    assert(h->elems == NULL);
    return;
  }
  if (max_entries < 0 || max_entries > IC_MAX_HISTORY) max_entries = IC_MAX_HISTORY;
  h->elems = (const char**)mem_zalloc_tp_n(h->mem, char*, max_entries );
  if (h->elems == NULL) return;
  h->len = max_entries;
  history_load(h);
}




//-------------------------------------------------------------
// save/load history to file
//-------------------------------------------------------------

static char from_xdigit( int c ) {
  if (c >= '0' && c <= '9') return (char)(c - '0');
  if (c >= 'A' && c <= 'F') return (char)(10 + (c - 'A'));
  if (c >= 'a' && c <= 'f') return (char)(10 + (c - 'a'));
  return 0;
}

static char to_xdigit( uint8_t c ) {
  if (c <= 9) return ((char)c + '0');
  if (c >= 10 && c <= 15) return ((char)c - 10 + 'A');
  return '0';
}

static bool ic_isxdigit( int c ) {
  return ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || (c >= '0' && c <= '9'));
}

static bool history_read_entry( history_t* h, FILE* f, stringbuf_t* sbuf ) {
  sbuf_clear(sbuf);
  while( !feof(f)) {
    int c = fgetc(f);
    if (c == EOF || c == '\n') break;
    if (c == '\\') {
      c = fgetc(f);
      if (c == 'n')       { sbuf_append(sbuf,"\n"); }
      else if (c == 'r')  { /* ignore */ }  // sbuf_append(sbuf,"\r");
      else if (c == 't')  { sbuf_append(sbuf,"\t"); }
      else if (c == '\\') { sbuf_append(sbuf,"\\"); }
      else if (c == 'x') {
        int c1 = fgetc(f);         
        int c2 = fgetc(f);
        if (ic_isxdigit(c1) && ic_isxdigit(c2)) {
          char chr = from_xdigit(c1)*16 + from_xdigit(c2);
          sbuf_append_char(sbuf,chr);
        }
        else return false;
      }
      else return false;
    }
    else sbuf_append_char(sbuf,(char)c);
  }
  if (sbuf_len(sbuf)==0 || sbuf_string(sbuf)[0] == '#') return true;
  return history_push(h, sbuf_string(sbuf));
}

static bool history_write_entry( const char* entry, FILE* f, stringbuf_t* sbuf ) {
  sbuf_clear(sbuf);
  //debug_msg("history: write: %s\n", entry);
  while( entry != NULL && *entry != 0 ) {
    char c = *entry++;
    if (c == '\\')      { sbuf_append(sbuf,"\\\\"); }
    else if (c == '\n') { sbuf_append(sbuf,"\\n"); }
    else if (c == '\r') { /* ignore */ } // sbuf_append(sbuf,"\\r"); }
    else if (c == '\t') { sbuf_append(sbuf,"\\t"); }
    else if (c < ' ' || c > '~' || c == '#') {
      char c1 = to_xdigit( (uint8_t)c / 16 );
      char c2 = to_xdigit( (uint8_t)c % 16 );
      sbuf_append(sbuf,"\\x"); 
      sbuf_append_char(sbuf,c1); 
      sbuf_append_char(sbuf,c2);            
    }
    else sbuf_append_char(sbuf,c);
  }
  //debug_msg("history: write buf: %s\n", sbuf_string(sbuf));
  
  if (sbuf_len(sbuf) > 0) {
    sbuf_append(sbuf,"\n");
    fputs(sbuf_string(sbuf),f);
  }
  return true;
}

ic_private void history_load( history_t* h ) {
  if (h->fname == NULL) return;
  FILE* f = fopen(h->fname, "r");
  if (f == NULL) return;
  stringbuf_t* sbuf = sbuf_new(h->mem);
  if (sbuf != NULL) {
    while (!feof(f)) {
      if (!history_read_entry(h,f,sbuf)) break; // error
    }
    sbuf_free(sbuf);
  }
  fclose(f);
}

ic_private void history_save( const history_t* h ) {
  if (h->fname == NULL) return;
  FILE* f = fopen(h->fname, "w");
  if (f == NULL) return;
  #ifndef _WIN32
  chmod(h->fname,S_IRUSR|S_IWUSR);
  #endif
  stringbuf_t* sbuf = sbuf_new(h->mem);
  if (sbuf != NULL) {
    for( int i = 0; i < h->count; i++ )  {
      if (!history_write_entry(h->elems[i],f,sbuf)) break;  // error
    }
    sbuf_free(sbuf);
  }
  fclose(f);  
}
// # include "completers.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>

// skipping dup #include "../include/isocline.h"
// skipping dup #include "common.h"
// skipping dup #include "env.h"
// skipping dup #include "stringbuf.h"
// skipping dup #include "completions.h"



//-------------------------------------------------------------
// Word completion 
//-------------------------------------------------------------

// free variables for word completion
typedef struct word_closure_s {
  long                  delete_before_adjust;
  void*                 prev_env;
  ic_completion_fun_t*  prev_complete;
} word_closure_t;


// word completion callback
static bool token_add_completion_ex(ic_env_t* env, void* closure, const char* replacement, const char* display, const char* help, long delete_before, long delete_after) {
  word_closure_t* wenv = (word_closure_t*)(closure);
  // call the previous completer with an adjusted delete-before
  return (*wenv->prev_complete)(env, wenv->prev_env, replacement, display, help, wenv->delete_before_adjust + delete_before, delete_after);
}


ic_public void ic_complete_word(ic_completion_env_t* cenv, const char* prefix, ic_completer_fun_t* fun,
                                    ic_is_char_class_fun_t* is_word_char) 
{
  if (is_word_char == NULL) is_word_char = &ic_char_is_nonseparator;
  
  ssize_t len = ic_strlen(prefix);
  ssize_t pos = len; // will be start of the 'word' (excluding a potential start quote)
  while (pos > 0) {
    // go back one code point
    ssize_t ofs = str_prev_ofs(prefix, pos, NULL);
    if (ofs <= 0) break;
    if (!(*is_word_char)(prefix + (pos - ofs), (long)ofs)) { 
      break;
    }
    pos -= ofs;
  }
  if (pos < 0) { pos = 0; }
  
  // stop if empty word
  // if (len == pos) return;
  
  // set up the closure
  word_closure_t wenv;
  wenv.delete_before_adjust = (long)(len - pos);
  wenv.prev_complete = cenv->complete;
  wenv.prev_env = cenv->env;
  cenv->complete = &token_add_completion_ex;
  cenv->closure = &wenv;

  // and call the user completion routine
  (*fun)(cenv, prefix + pos);

  // restore the original environment
  cenv->complete = wenv.prev_complete;
  cenv->closure = wenv.prev_env;
}


//-------------------------------------------------------------
// Quoted word completion (with escape characters)
//-------------------------------------------------------------

// free variables for word completion
typedef struct qword_closure_s {
  char         escape_char;
  char         quote;
  long         delete_before_adjust;
  stringbuf_t* sbuf;
  void*        prev_env;
  ic_is_char_class_fun_t* is_word_char;
  ic_completion_fun_t*    prev_complete;
} qword_closure_t;


// word completion callback
static bool qword_add_completion_ex(ic_env_t* env, void* closure, const char* replacement, const char* display, const char* help, 
                                       long delete_before, long delete_after) {
  qword_closure_t* wenv = (qword_closure_t*)(closure);
  sbuf_replace( wenv->sbuf, replacement );   
  if (wenv->quote != 0) {
    // add end quote
    sbuf_append_char( wenv->sbuf, wenv->quote);
  }
  else {
    // escape non-word characters if it was not quoted
    ssize_t pos = 0;
    ssize_t next;
    while ( (next = sbuf_next_ofs(wenv->sbuf, pos, NULL)) > 0 ) 
    {
      if (!(*wenv->is_word_char)(sbuf_string(wenv->sbuf) + pos, (long)next)) { // strchr(wenv->non_word_char, sbuf_char_at( wenv->sbuf, pos )) != NULL) {
        sbuf_insert_char_at( wenv->sbuf, wenv->escape_char, pos);
        pos++;
      }
      pos += next;
    }
  }
  // and call the previous completion function
  return (*wenv->prev_complete)( env, wenv->prev_env, sbuf_string(wenv->sbuf), display, help, wenv->delete_before_adjust + delete_before, delete_after );  
}


ic_public void ic_complete_qword( ic_completion_env_t* cenv, const char* prefix, ic_completer_fun_t* fun, ic_is_char_class_fun_t* is_word_char ) {
  ic_complete_qword_ex( cenv, prefix, fun, is_word_char, '\\', NULL);
}


ic_public void ic_complete_qword_ex( ic_completion_env_t* cenv, const char* prefix, ic_completer_fun_t* fun, 
                                        ic_is_char_class_fun_t* is_word_char, char escape_char, const char* quote_chars ) {
  if (is_word_char == NULL) is_word_char = &ic_char_is_nonseparator ;  
  if (quote_chars == NULL) quote_chars = "'\"";

  ssize_t len = ic_strlen(prefix);
  ssize_t pos; // will be start of the 'word' (excluding a potential start quote)
  char quote = 0;
  ssize_t quote_len = 0;
  
  // 1. look for a starting quote
  if (quote_chars[0] != 0) {
    // we go forward and count all quotes; if it is uneven, we need to complete quoted.
    ssize_t qpos_open = -1;
    ssize_t qpos_close = -1;
    ssize_t qcount = 0;
    pos = 0; 
    while(pos < len) {
      if (prefix[pos] == escape_char && prefix[pos+1] != 0 && 
           !(*is_word_char)(prefix + pos + 1, 1)) // strchr(non_word_char, prefix[pos+1]) != NULL
      {       
        pos++; // skip escape and next char
      }
      else if (qcount % 2 == 0 && strchr(quote_chars, prefix[pos]) != NULL) {
        // open quote 
        qpos_open = pos;
        quote = prefix[pos];
        qcount++;
      }
      else if (qcount % 2 == 1 && prefix[pos] == quote) {
        // close quote
        qpos_close = pos;
        qcount++;
      }
      else if (!(*is_word_char)(prefix + pos, 1)) { //  strchr(non_word_char, prefix[pos]) != NULL) {
        qpos_close = -1;
      }
      ssize_t ofs = str_next_ofs( prefix, len, pos, NULL );
      if (ofs <= 0) break;
      pos += ofs;
    }    
    if ((qcount % 2 == 0 && qpos_close >= 0) || // if the last quote is only followed by word chars, we still complete it
        (qcount % 2 == 1))                     // opening quote found
    {
      quote_len = (len - qpos_open - 1);
      pos = qpos_open + 1;  // pos points to the word start just after the quote.
    }
    else {
      quote = 0;
    }
  }

  // 2. if we did not find a quoted word, look for non-word-chars
  if (quote == 0) {
    pos = len;
    while(pos > 0) {
      // go back one code point
      ssize_t ofs = str_prev_ofs(prefix, pos, NULL );
      if (ofs <= 0) break;
      if (!(*is_word_char)(prefix + (pos - ofs), (long)ofs)) { // strchr(non_word_char, prefix[pos - ofs]) != NULL) {
        // non word char, break if it is not escaped
        if (pos <= ofs || prefix[pos - ofs - 1] != escape_char) break; 
        // otherwise go on
        pos--; // skip escaped char
      }
      pos -= ofs;
    }
  }

  // stop if empty word
  // if (len == pos) return;

  // allocate new unescaped word prefix
  char* word = mem_strndup( cenv->env->mem, prefix + pos, (quote==0 ? len - pos : quote_len));
  if (word == NULL) return;

  if (quote == 0) {
    // unescape prefix
    ssize_t wlen = len - pos;
    ssize_t wpos = 0;
    while (wpos < wlen) {
      ssize_t ofs = str_next_ofs(word, wlen, wpos, NULL);
      if (ofs <= 0) break;
      if (word[wpos] == escape_char && word[wpos+1] != 0 &&
           !(*is_word_char)(word + wpos + 1, (long)ofs)) // strchr(non_word_char, word[wpos+1]) != NULL) {
      {
        ic_memmove(word + wpos, word + wpos + 1, wlen - wpos /* including 0 */);
      }
      wpos += ofs;
    }
  }
  #ifdef _WIN32
  else {
    // remove inner quote: "c:\Program Files\"Win
    ssize_t wlen = len - pos;
    ssize_t wpos = 0;
    while (wpos < wlen) {
      ssize_t ofs = str_next_ofs(word, wlen, wpos, NULL);
      if (ofs <= 0) break;
      if (word[wpos] == escape_char && word[wpos+1] == quote) {
        word[wpos+1] = escape_char;
        ic_memmove(word + wpos, word + wpos + 1, wlen - wpos /* including 0 */);
      }
      wpos += ofs;
    }
  }
  #endif

  // set up the closure
  qword_closure_t wenv;
  wenv.quote          = quote;
  wenv.is_word_char   = is_word_char;
  wenv.escape_char    = escape_char;
  wenv.delete_before_adjust = (long)(len - pos);
  wenv.prev_complete  = cenv->complete;
  wenv.prev_env       =  cenv->env;
  wenv.sbuf = sbuf_new(cenv->env->mem);
  if (wenv.sbuf == NULL) { mem_free(cenv->env->mem, word); return; }
  cenv->complete = &qword_add_completion_ex;
  cenv->closure = &wenv;

  // and call the user completion routine
  (*fun)( cenv, word );

  // restore the original environment
  cenv->complete = wenv.prev_complete;
  cenv->closure = wenv.prev_env;

  sbuf_free(wenv.sbuf);
  mem_free(cenv->env->mem, word);  
}




//-------------------------------------------------------------
// Complete file names
// Listing files
//-------------------------------------------------------------
#include <stdlib.h>

typedef enum file_type_e {
  // must follow BSD style LSCOLORS order
  FT_DEFAULT = 0,
  FT_DIR,
  FT_SYM,
  FT_SOCK,
  FT_PIPE,
  FT_BLOCK,
  FT_CHAR,
  FT_SETUID,
  FT_SETGID,
  FT_DIR_OW_STICKY,
  FT_DIR_OW,
  FT_DIR_STICKY,
  FT_EXE,
  FT_LAST
} file_type_t;

static int         cli_color; // 1 enabled, 0 not initialized, -1 disabled
static const char* lscolors  = "exfxcxdxbxegedabagacad";  // default BSD setting
static const char* ls_colors;
static const char* ls_colors_names[] = { "no=","di=","ln=","so=","pi=","bd=","cd=","su=","sg=","tw=","ow=","st=","ex=", NULL };

static bool ls_colors_init(void) {
  if (cli_color != 0) return (cli_color >= 1);
  // colors enabled?
  const char* s = getenv("CLICOLOR");
  if (s==NULL || (strcmp(s, "1")!=0 && strcmp(s, "") != 0)) {
    cli_color = -1;
    return false;
  }
  cli_color = 1;
  s = getenv("LS_COLORS");
  if (s != NULL) { ls_colors = s;  }
  s = getenv("LSCOLORS");
  if (s != NULL) { lscolors = s; }  
  return true;
}

static bool ls_valid_esc(ssize_t c) {
  return ((c==0 || c==1 || c==4 || c==7 || c==22 || c==24  || c==27) ||
    (c >= 30 && c <= 37) || (c >= 40 && c <= 47) ||
    (c >= 90 && c <= 97) || (c >= 100 && c <= 107));
}

static bool ls_colors_from_key(stringbuf_t* sb, const char* key) {
  // find key
  ssize_t keylen = ic_strlen(key);
  if (keylen <= 0) return false;
  const char* p = strstr(ls_colors, key);
  if (p == NULL) return false;
  p += keylen;
  if (key[keylen-1] != '=') {
    if (*p != '=') return false;
    p++;
  }
  ssize_t len = 0;
  while (p[len] != 0 && p[len] != ':') {
    len++;
  }
  if (len <= 0) return false;
  sbuf_append(sb, "[ansi-sgr=\"" );
  sbuf_append_n(sb, p, len );
  sbuf_append(sb, "\"]");
  return true;
}

static int ls_colors_from_char(char c) {
  if (c >= 'a' && c <= 'h')      { return (c - 'a'); }
  else if (c >= 'A' && c <= 'H') { return (c - 'A') + 8; }
  else if (c == 'x')             { return 256; }
  else return 256; // default
}

static bool ls_colors_append(stringbuf_t* sb, file_type_t ft, const char* ext) {
  if (!ls_colors_init()) return false;
  if (ls_colors != NULL) {
    // GNU style
    if (ft == FT_DEFAULT && ext != NULL) {
      // first try extension match
      if (ls_colors_from_key(sb, ext)) return true;
    }
    if (ft >= FT_DEFAULT && ft < FT_LAST) {
      // then a filetype match
      const char* key = ls_colors_names[ft];
      if (ls_colors_from_key(sb, key)) return true;
    }    
  }
  else if (lscolors != NULL) {
    // BSD style
    char fg = 'x';
    char bg = 'x';
    if (ic_strlen(lscolors) > (2*(ssize_t)ft)+1) {
      fg = lscolors[2*ft];
      bg = lscolors[2*ft + 1];
    }
    sbuf_appendf(sb, "[ansi-color=%d ansi-bgcolor=%d]", ls_colors_from_char(fg), ls_colors_from_char(bg) );
    return true;
  }
  return false;
}

static void ls_colorize(bool no_lscolor, stringbuf_t* sb, file_type_t ft, const char* name, const char* ext, char dirsep) {
  bool close = (no_lscolor ? false : ls_colors_append( sb, ft, ext));
  sbuf_append(sb, "[!pre]" );
  sbuf_append(sb, name);
  if (dirsep != 0) sbuf_append_char(sb, dirsep);
  sbuf_append(sb,"[/pre]" );
  if (close) { sbuf_append(sb, "[/]"); }
}

#if defined(_WIN32)
#include <io.h>
#include <sys/stat.h>

static bool os_is_dir(const char* cpath) {
  struct _stat64 st = { 0 };
  _stat64(cpath, &st);
  return ((st.st_mode & _S_IFDIR) != 0);
}

static file_type_t os_get_filetype(const char* cpath) {
  struct _stat64 st = { 0 };
  _stat64(cpath, &st);
  if (((st.st_mode) & _S_IFDIR) != 0) return FT_DIR;
  if (((st.st_mode) & _S_IFCHR) != 0) return FT_CHAR;
  if (((st.st_mode) & _S_IFIFO) != 0) return FT_PIPE;
  if (((st.st_mode) & _S_IEXEC) != 0) return FT_EXE;
  return FT_DEFAULT;
}


#define dir_cursor intptr_t
#define dir_entry  struct __finddata64_t

static bool os_findfirst(alloc_t* mem, const char* path, dir_cursor* d, dir_entry* entry) {
  stringbuf_t* spath = sbuf_new(mem);
  if (spath == NULL) return false;
  sbuf_append(spath, path);
  sbuf_append(spath, "\\*");
  *d = _findfirsti64(sbuf_string(spath), entry);
  mem_free(mem,spath);
  return (*d != -1);
}

static bool os_findnext(dir_cursor d, dir_entry* entry) {
  return (_findnexti64(d, entry) == 0);  
}

static void os_findclose(dir_cursor d) {
  _findclose(d);
}

static const char* os_direntry_name(dir_entry* entry) {
  return entry->name;  
}

static bool os_path_is_absolute( const char* path ) {
  if (path != NULL && path[0] != 0 && path[1] == ':' && (path[2] == '\\' || path[2] == '/' || path[2] == 0)) {
    char drive = path[0];
    return ((drive >= 'A' && drive <= 'Z') || (drive >= 'a' && drive <= 'z'));
  }
  else return false;
}

ic_private char ic_dirsep(void) {
  return '\\';
}
#else

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>

static bool os_is_dir(const char* cpath) {
  struct stat st;
  memset(&st, 0, sizeof(st));
  stat(cpath, &st);
  return (S_ISDIR(st.st_mode));
}

static file_type_t os_get_filetype(const char* cpath) {
  struct stat st;
  memset(&st, 0, sizeof(st));
  lstat(cpath, &st);
  switch ((st.st_mode)&S_IFMT) {
    case S_IFSOCK: return FT_SOCK;
    case S_IFLNK: {
      return FT_SYM;
    }
    case S_IFIFO:  return FT_PIPE;
    case S_IFCHR:  return FT_CHAR;
    case S_IFBLK:  return FT_BLOCK;
    case S_IFDIR: {
      if ((st.st_mode & S_ISUID) != 0) return FT_SETUID;
      if ((st.st_mode & S_ISGID) != 0) return FT_SETGID;
      if ((st.st_mode & S_IWGRP) != 0 && (st.st_mode & S_ISVTX) != 0) return FT_DIR_OW_STICKY;
      if ((st.st_mode & S_IWGRP)) return FT_DIR_OW;
      if ((st.st_mode & S_ISVTX)) return FT_DIR_STICKY;
      return FT_DIR;
    }
    case S_IFREG:
    default: {
      if ((st.st_mode & S_IXUSR) != 0) return FT_EXE;
      return FT_DEFAULT;
    }
  }  
}


#define dir_cursor DIR*
#define dir_entry  struct dirent*

static bool os_findnext(dir_cursor d, dir_entry* entry) {
  *entry = readdir(d);
  return (*entry != NULL);
}

static bool os_findfirst(alloc_t* mem, const char* cpath, dir_cursor* d, dir_entry* entry) {
  ic_unused(mem);
  *d = opendir(cpath);
  if (*d == NULL) {
    return false;
  }
  else {
    return os_findnext(*d, entry);
  }
}

static void os_findclose(dir_cursor d) {
  closedir(d);
}

static const char* os_direntry_name(dir_entry* entry) {
  return (*entry)->d_name;  
}

static bool os_path_is_absolute( const char* path ) {
  return (path != NULL && path[0] == '/');
}

ic_private char ic_dirsep(void) {
  return '/';
}
#endif



//-------------------------------------------------------------
// File completion 
//-------------------------------------------------------------

static bool ends_with_n(const char* name, ssize_t name_len, const char* ending, ssize_t len) {
  if (name_len < len) return false;
  if (ending == NULL || len <= 0) return true;
  for (ssize_t i = 1; i <= len; i++) {
    char c1 = name[name_len - i];
    char c2 = ending[len - i];
    #ifdef _WIN32
    if (ic_tolower(c1) != ic_tolower(c2)) return false;
    #else
    if (c1 != c2) return false;
    #endif
  }
  return true;
}

static bool match_extension(const char* name, const char* extensions) {
  if (extensions == NULL || extensions[0] == 0) return true;
  if (name == NULL) return false;
  ssize_t name_len = ic_strlen(name);
  ssize_t len = ic_strlen(extensions);
  ssize_t cur = 0;  
  //debug_msg("match extensions: %s ~ %s", name, extensions);
  for (ssize_t end = 0; end <= len; end++) {
    if (extensions[end] == ';' || extensions[end] == 0) {
      if (ends_with_n(name, name_len, extensions+cur, (end - cur))) {
        return true;
      }
      cur = end+1;
    }
  }
  return false;
}

static bool filename_complete_indir( ic_completion_env_t* cenv, stringbuf_t* dir, 
                                      stringbuf_t* dir_prefix, stringbuf_t* display,
                                       const char* base_prefix, 
                                        char dir_sep, const char* extensions ) 
{
  dir_cursor d = 0;
  dir_entry entry;
  bool cont = true;
  if (os_findfirst(cenv->env->mem, sbuf_string(dir), &d, &entry)) {
    do {
      const char* name = os_direntry_name(&entry);
      if (name != NULL && strcmp(name, ".") != 0 && strcmp(name, "..") != 0 && 
          ic_istarts_with(name, base_prefix))
      {
        // possible match, first check if it is a directory
        file_type_t ft;
        bool isdir;
        const ssize_t plen = sbuf_len(dir_prefix);
        sbuf_append(dir_prefix, name);
        { // check directory and potentially add a dirsep to the dir_prefix
          const ssize_t dlen = sbuf_len(dir);
          sbuf_append_char(dir,ic_dirsep());
          sbuf_append(dir,name);
          ft = os_get_filetype(sbuf_string(dir));
          isdir = os_is_dir(sbuf_string(dir));
          if (isdir && dir_sep != 0) {
            sbuf_append_char(dir_prefix,dir_sep); 
          }
          sbuf_delete_from(dir,dlen);  // restore dir
        }
        if (isdir || match_extension(name, extensions)) {
          // add completion
          sbuf_clear(display);
          ls_colorize(cenv->env->no_lscolors, display, ft, name, NULL, (isdir ? dir_sep : 0));
          cont = ic_add_completion_ex(cenv, sbuf_string(dir_prefix), sbuf_string(display), NULL);
        }
        sbuf_delete_from( dir_prefix, plen ); // restore dir_prefix
      }
    } while (cont && os_findnext(d, &entry));
    os_findclose(d);
  }
  return cont;
}

typedef struct filename_closure_s {
  const char* roots;
  const char* extensions;
  char        dir_sep;
} filename_closure_t;

static void filename_completer( ic_completion_env_t* cenv, const char* prefix ) {
  if (prefix == NULL) return;
  filename_closure_t* fclosure = (filename_closure_t*)cenv->arg;  
  stringbuf_t* root_dir   = sbuf_new(cenv->env->mem);
  stringbuf_t* dir_prefix = sbuf_new(cenv->env->mem);
  stringbuf_t* display    = sbuf_new(cenv->env->mem);  
  if (root_dir!=NULL && dir_prefix != NULL && display != NULL) 
  {
    // split prefix in dir_prefix / base.
    const char* base = strrchr(prefix,'/');
    #ifdef _WIN32
    const char* base2 = strrchr(prefix,'\\');
    if (base == NULL || base2 > base) base = base2;
    #endif
    if (base != NULL) {
      base++; 
      sbuf_append_n(dir_prefix, prefix, base - prefix ); // includes dir separator
    }

    // absolute path
    if (os_path_is_absolute(prefix)) {
      // do not use roots but try to complete directly
      if (base != NULL) {
        sbuf_append_n( root_dir, prefix, (base - prefix));  // include dir separator
      }
      filename_complete_indir( cenv, root_dir, dir_prefix, display,  
                                (base != NULL ? base : prefix), 
                                 fclosure->dir_sep, fclosure->extensions );   
    }
    else {
      // relative path, complete with respect to every root.
      const char* next;
      const char* root = fclosure->roots;
      while ( root != NULL ) {
        // create full root in `root_dir`
        sbuf_clear(root_dir);
        next = strchr(root,';');
        if (next == NULL) {
          sbuf_append( root_dir, root );
          root = NULL;
        }
        else {
          sbuf_append_n( root_dir, root, next - root );
          root = next + 1;
        }      
        sbuf_append_char( root_dir, ic_dirsep());
          
        // add the dir_prefix to the root
        if (base != NULL) {
          sbuf_append_n( root_dir, prefix, (base - prefix) - 1);
        }

        // and complete in this directory    
        filename_complete_indir( cenv, root_dir, dir_prefix, display,
                                  (base != NULL ? base : prefix), 
                                   fclosure->dir_sep, fclosure->extensions);
      }
    }
  }
  sbuf_free(display);
  sbuf_free(root_dir);
  sbuf_free(dir_prefix);
}

ic_public void ic_complete_filename( ic_completion_env_t* cenv, const char* prefix, char dir_sep, const char* roots, const char* extensions ) {
  if (roots == NULL) roots = ".";
  if (extensions == NULL) extensions = "";
  if (dir_sep == 0) dir_sep = ic_dirsep();
  filename_closure_t fclosure;
  fclosure.dir_sep = dir_sep;
  fclosure.roots = roots; 
  fclosure.extensions = extensions;
  cenv->arg = &fclosure;
  ic_complete_qword_ex( cenv, prefix, &filename_completer, &ic_char_is_filename_letter, '\\', "'\"");  
}
// # include "completions.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// skipping dup #include "../include/isocline.h"
// skipping dup #include "common.h"
// skipping dup #include "env.h"
// skipping dup #include "stringbuf.h"
// skipping dup #include "completions.h"


//-------------------------------------------------------------
// Completions
//-------------------------------------------------------------

typedef struct completion_s {
  const char* replacement;
  const char* display;
  const char* help;
  ssize_t     delete_before;
  ssize_t     delete_after;
} completion_t;

struct completions_s {
  ic_completer_fun_t* completer;
  void* completer_arg;
  ssize_t completer_max;
  ssize_t count;
  ssize_t len;
  completion_t* elems;
  alloc_t* mem;
};

static void default_filename_completer( ic_completion_env_t* cenv, const char* prefix );

ic_private completions_t* completions_new(alloc_t* mem) {
  completions_t* cms = mem_zalloc_tp(mem, completions_t);
  if (cms == NULL) return NULL;
  cms->mem = mem;
  cms->completer = &default_filename_completer;
  return cms;
}

ic_private void completions_free(completions_t* cms) {
  if (cms == NULL) return;
  completions_clear(cms);  
  if (cms->elems != NULL) {
    mem_free(cms->mem, cms->elems);
    cms->elems = NULL;
    cms->count = 0;
    cms->len = 0;
  }
  mem_free(cms->mem, cms); // free ourselves
}


ic_private void completions_clear(completions_t* cms) {  
  while (cms->count > 0) {
    completion_t* cm = cms->elems + cms->count - 1;
    mem_free( cms->mem, cm->display);
    mem_free( cms->mem, cm->replacement);
    mem_free( cms->mem, cm->help);
    memset(cm,0,sizeof(*cm));
    cms->count--;    
  }
}

static void completions_push(completions_t* cms, const char* replacement, const char* display, const char* help, ssize_t delete_before, ssize_t delete_after) 
{
  if (cms->count >= cms->len) {
    ssize_t newlen = (cms->len <= 0 ? 32 : cms->len*2);
    completion_t* newelems = mem_realloc_tp(cms->mem, completion_t, cms->elems, newlen );
    if (newelems == NULL) return;
    cms->elems = newelems;
    cms->len   = newlen;
  }
  assert(cms->count < cms->len);
  completion_t* cm  = cms->elems + cms->count;
  cm->replacement   = mem_strdup(cms->mem,replacement);
  cm->display       = mem_strdup(cms->mem,display);
  cm->help          = mem_strdup(cms->mem,help);
  cm->delete_before = delete_before;
  cm->delete_after  = delete_after;
  cms->count++;
}

ic_private ssize_t completions_count(completions_t* cms) {
  return cms->count;
}

static bool completions_contains(completions_t* cms, const char* replacement) {
  for( ssize_t i = 0; i < cms->count; i++ ) {
    const completion_t* c = cms->elems + i;
    if (strcmp(replacement,c->replacement) == 0) { return true; }
  }
  return false;
} 

ic_private bool completions_add(completions_t* cms, const char* replacement, const char* display, const char* help, ssize_t delete_before, ssize_t delete_after) {
  if (cms->completer_max <= 0) return false;
  cms->completer_max--;
  //debug_msg("completion: add: %d,%d, %s\n", delete_before, delete_after, replacement);
  if (!completions_contains(cms,replacement)) {
    completions_push(cms, replacement, display, help, delete_before, delete_after);
  }
  return true;
}

static completion_t* completions_get(completions_t* cms, ssize_t index) {
  if (index < 0 || cms->count <= 0 || index >= cms->count) return NULL;
  return &cms->elems[index];
}

ic_private const char* completions_get_display( completions_t* cms, ssize_t index, const char** help ) {
  if (help != NULL) { *help = NULL;  }
  completion_t* cm = completions_get(cms, index);
  if (cm == NULL) return NULL;
  if (help != NULL) { *help = cm->help; }
  return (cm->display != NULL ? cm->display : cm->replacement);
}

ic_private const char* completions_get_help( completions_t* cms, ssize_t index ) {
  completion_t* cm = completions_get(cms, index);
  if (cm == NULL) return NULL;
  return cm->help;
}

ic_private const char* completions_get_hint(completions_t* cms, ssize_t index, const char** help) {
  if (help != NULL) { *help = NULL; }
  completion_t* cm = completions_get(cms, index);
  if (cm == NULL) return NULL;
  ssize_t len = ic_strlen(cm->replacement);
  if (len < cm->delete_before) return NULL;
  const char* hint = (cm->replacement + cm->delete_before);
  if (*hint == 0 || utf8_is_cont((uint8_t)(*hint))) return NULL;  // utf8 boundary?
  if (help != NULL) { *help = cm->help; }
  return hint;
}

ic_private void completions_set_completer(completions_t* cms, ic_completer_fun_t* completer, void* arg) {
  cms->completer = completer;
  cms->completer_arg = arg;
}

ic_private void completions_get_completer(completions_t* cms, ic_completer_fun_t** completer, void** arg) {
  *completer = cms->completer;
  *arg = cms->completer_arg;
}


ic_public void* ic_completion_arg( const ic_completion_env_t* cenv ) {
  return (cenv == NULL ? NULL : cenv->env->completions->completer_arg);
}

ic_public bool ic_has_completions( const ic_completion_env_t* cenv ) {
  return (cenv == NULL ? false : cenv->env->completions->count > 0);
}

ic_public bool ic_stop_completing( const ic_completion_env_t* cenv) {
  return (cenv == NULL ? true : cenv->env->completions->completer_max <= 0);
}


static ssize_t completion_apply( completion_t* cm, stringbuf_t* sbuf, ssize_t pos ) {
  if (cm == NULL) return -1;  
  debug_msg( "completion: apply: %s at %zd\n", cm->replacement, pos);
  ssize_t start = pos - cm->delete_before;
  if (start < 0) start = 0;
  ssize_t n = cm->delete_before + cm->delete_after;
  if (ic_strlen(cm->replacement) == n && strncmp(sbuf_string_at(sbuf,start), cm->replacement, to_size_t(n)) == 0) {
    // no changes
    return -1;
  }
  else {
    sbuf_delete_from_to( sbuf, start, pos + cm->delete_after );
    return sbuf_insert_at(sbuf, cm->replacement, start); 
  }
}

ic_private ssize_t completions_apply( completions_t* cms, ssize_t index, stringbuf_t* sbuf, ssize_t pos ) {
  completion_t* cm = completions_get(cms, index);
  return completion_apply( cm, sbuf, pos );
}


static int completion_compare(const void* p1, const void* p2) {
  if (p1 == NULL || p2 == NULL) return 0;
  const completion_t* cm1 = (const completion_t*)p1;
  const completion_t* cm2 = (const completion_t*)p2;  
  return ic_stricmp(cm1->replacement, cm2->replacement);
}

ic_private void completions_sort(completions_t* cms) {
  if (cms->count <= 0) return;
  qsort(cms->elems, to_size_t(cms->count), sizeof(cms->elems[0]), &completion_compare);
}

#define IC_MAX_PREFIX  (256)

// find longest common prefix and complete with that.
ic_private ssize_t completions_apply_longest_prefix(completions_t* cms, stringbuf_t* sbuf, ssize_t pos) {
  if (cms->count <= 1) {
    return completions_apply(cms,0,sbuf,pos);
  }

  // set initial prefix to the first entry
  completion_t* cm = completions_get(cms, 0);
  if (cm == NULL) return -1;

  char prefix[IC_MAX_PREFIX+1];
  ssize_t delete_before = cm->delete_before;
  ic_strncpy( prefix, IC_MAX_PREFIX+1, cm->replacement, IC_MAX_PREFIX );
  prefix[IC_MAX_PREFIX] = 0;
  
  // and visit all others to find the longest common prefix
  for(ssize_t i = 1; i < cms->count; i++) {
    cm = completions_get(cms,i);
    if (cm->delete_before != delete_before) {  // deletions must match delete_before
      prefix[0] = 0;
      break;
    }
    // check if it is still a prefix
    const char* r = cm->replacement;    
    ssize_t j;
    for(j = 0; prefix[j] != 0 && r[j] != 0; j++) {
      if (prefix[j] != r[j]) break;
    }
    prefix[j] = 0;
    if (j <= 0) break;
  }

  // check the length
  ssize_t len = ic_strlen(prefix);
  if (len <= 0 || len < delete_before) return -1;

  // we found a prefix :-)
  completion_t cprefix;
  memset(&cprefix,0,sizeof(cprefix));
  cprefix.delete_before = delete_before;
  cprefix.replacement   = prefix;
  ssize_t newpos = completion_apply( &cprefix, sbuf, pos);
  if (newpos < 0) return newpos;  

  // adjust all delete_before for the new replacement
  for( ssize_t i = 0; i < cms->count; i++) {
    cm = completions_get(cms,i);
    cm->delete_before = len;
  }

  return newpos;
}


//-------------------------------------------------------------
// Completer functions
//-------------------------------------------------------------

ic_public bool ic_add_completions(ic_completion_env_t* cenv, const char* prefix, const char** completions) {
  for (const char** pc = completions; *pc != NULL; pc++) {
    if (ic_istarts_with(*pc, prefix)) {
      if (!ic_add_completion_ex(cenv, *pc, NULL, NULL)) return false;
    }
  }
  return true;
}

ic_public bool ic_add_completion(ic_completion_env_t* cenv, const char* replacement) {
  return ic_add_completion_ex(cenv, replacement, NULL, NULL);
}

ic_public bool ic_add_completion_ex( ic_completion_env_t* cenv, const char* replacement, const char* display, const char* help ) {
  return ic_add_completion_prim(cenv,replacement,display,help,0,0);
}

ic_public bool ic_add_completion_prim(ic_completion_env_t* cenv, const char* replacement, const char* display, const char* help, long delete_before, long delete_after) {
  return (*cenv->complete)(cenv->env, cenv->closure, replacement, display, help, delete_before, delete_after );
}

static bool prim_add_completion(ic_env_t* env, void* funenv, const char* replacement, const char* display, const char* help, long delete_before, long delete_after) {
  ic_unused(funenv);
  return completions_add(env->completions, replacement, display, help, delete_before, delete_after);
}

ic_public void ic_set_default_completer(ic_completer_fun_t* completer, void* arg) {
  ic_env_t* env = ic_get_env(); if (env == NULL) return;
  completions_set_completer(env->completions, completer, arg);
}

ic_private ssize_t completions_generate(struct ic_env_s* env, completions_t* cms, const char* input, ssize_t pos, ssize_t max) {
  completions_clear(cms);
  if (cms->completer == NULL || input == NULL || ic_strlen(input) < pos) return 0;

  // set up env
  ic_completion_env_t cenv;
  cenv.env = env;
  cenv.input = input,
  cenv.cursor = (long)pos;
  cenv.arg = cms->completer_arg;
  cenv.complete = &prim_add_completion;
  cenv.closure  = NULL;
  const char* prefix = mem_strndup(cms->mem, input, pos);
  cms->completer_max = max;
  
  // and complete
  cms->completer(&cenv,prefix);

  // restore
  mem_free(cms->mem,prefix);
  return completions_count(cms);
}

// The default completer is no completion is set
static void default_filename_completer( ic_completion_env_t* cenv, const char* prefix ) {
  #ifdef _WIN32
  const char sep = '\\';
  #else
  const char sep = '/';
  #endif
  ic_complete_filename( cenv, prefix, sep, ".", NULL);
}
// # include "term.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>  // getenv
#include <inttypes.h>

// skipping dup #include "common.h"
// skipping dup #include "tty.h"
// skipping dup #include "term.h"
// skipping dup #include "stringbuf.h" // str_next_ofs

#if defined(_WIN32)
#include <windows.h>
#define STDOUT_FILENO 1
#else
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>
#if defined(__linux__)
#include <linux/kd.h>
#endif
#endif

#define IC_CSI      "\x1B["

// color support; colors are auto mapped smaller palettes if needed. (see `term_color.c`)
typedef enum palette_e {
  MONOCHROME,  // no color
  ANSI8,       // only basic 8 ANSI color     (ESC[<idx>m, idx: 30-37, +10 for background)
  ANSI16,      // basic + bright ANSI colors  (ESC[<idx>m, idx: 30-37, 90-97, +10 for background)
  ANSI256,     // ANSI 256 color palette      (ESC[38;5;<idx>m, idx: 0-15 standard color, 16-231 6x6x6 rbg colors, 232-255 gray shades)
  ANSIRGB      // direct rgb colors supported (ESC[38;2;<r>;<g>;<b>m)
} palette_t;

// The terminal screen
struct term_s {
  int           fd_out;             // output handle
  ssize_t       width;              // screen column width
  ssize_t       height;             // screen row height
  ssize_t       raw_enabled;        // is raw mode active? counted by start/end pairs
  bool          nocolor;            // show colors?
  bool          silent;             // enable beep?
  bool          is_utf8;            // utf-8 output? determined by the tty
  attr_t   attr;               // current text attributes
  palette_t     palette;            // color support
  buffer_mode_t bufmode;            // buffer mode
  stringbuf_t*  buf;                // buffer for buffered output
  tty_t*        tty;                // used on posix to get the cursor position
  alloc_t*      mem;                // allocator
  #ifdef _WIN32
  HANDLE        hcon;               // output console handler
  WORD          hcon_default_attr;  // default text attributes
  WORD          hcon_orig_attr;     // original text attributes
  DWORD         hcon_orig_mode;     // original console mode
  DWORD         hcon_mode;          // used console mode
  UINT          hcon_orig_cp;       // original console code-page (locale)
  COORD         hcon_save_cursor;   // saved cursor position (for escape sequence emulation)
  #endif
};

static bool term_write_direct(term_t* term, const char* s, ssize_t n );
static void term_append_buf(term_t* term, const char* s, ssize_t n);

//-------------------------------------------------------------
// Colors
//-------------------------------------------------------------

// #include "term_color.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

// This file is included in "term.c"

//-------------------------------------------------------------
// Standard ANSI palette for 256 colors
//-------------------------------------------------------------

static uint32_t ansi256[256] = {   
  // not const as on some platforms (e.g. Windows, xterm) we update the first 16 entries with the actual used colors.
  // 0, standard ANSI
  0x000000, 0x800000, 0x008000, 0x808000, 0x000080, 0x800080, 
  0x008080, 0xc0c0c0,
  // 8, bright ANSI
  0x808080, 0xff0000, 0x00ff00, 0xffff00, 0x0000ff, 0xff00ff, 
  0x00ffff, 0xffffff,
  // 6x6x6 RGB colors
  // 16
  0x000000, 0x00005f, 0x000087, 0x0000af, 0x0000d7, 0x0000ff,
  0x005f00, 0x005f5f, 0x005f87, 0x005faf, 0x005fd7, 0x005fff,
  0x008700, 0x00875f, 0x008787, 0x0087af, 0x0087d7, 0x0087ff,
  0x00af00, 0x00af5f, 0x00af87, 0x00afaf, 0x00afd7, 0x00afff,
  0x00d700, 0x00d75f, 0x00d787, 0x00d7af, 0x00d7d7, 0x00d7ff,
  0x00ff00, 0x00ff5f, 0x00ff87, 0x00ffaf, 0x00ffd7, 0x00ffff,
  // 52
  0x5f0000, 0x5f005f, 0x5f0087, 0x5f00af, 0x5f00d7, 0x5f00ff,
  0x5f5f00, 0x5f5f5f, 0x5f5f87, 0x5f5faf, 0x5f5fd7, 0x5f5fff,
  0x5f8700, 0x5f875f, 0x5f8787, 0x5f87af, 0x5f87d7, 0x5f87ff,
  0x5faf00, 0x5faf5f, 0x5faf87, 0x5fafaf, 0x5fafd7, 0x5fafff,
  0x5fd700, 0x5fd75f, 0x5fd787, 0x5fd7af, 0x5fd7d7, 0x5fd7ff,
  0x5fff00, 0x5fff5f, 0x5fff87, 0x5fffaf, 0x5fffd7, 0x5fffff,
  // 88
  0x870000, 0x87005f, 0x870087, 0x8700af, 0x8700d7, 0x8700ff,
  0x875f00, 0x875f5f, 0x875f87, 0x875faf, 0x875fd7, 0x875fff,
  0x878700, 0x87875f, 0x878787, 0x8787af, 0x8787d7, 0x8787ff,
  0x87af00, 0x87af5f, 0x87af87, 0x87afaf, 0x87afd7, 0x87afff,
  0x87d700, 0x87d75f, 0x87d787, 0x87d7af, 0x87d7d7, 0x87d7ff,
  0x87ff00, 0x87ff5f, 0x87ff87, 0x87ffaf, 0x87ffd7, 0x87ffff,
  // 124
  0xaf0000, 0xaf005f, 0xaf0087, 0xaf00af, 0xaf00d7, 0xaf00ff,
  0xaf5f00, 0xaf5f5f, 0xaf5f87, 0xaf5faf, 0xaf5fd7, 0xaf5fff,
  0xaf8700, 0xaf875f, 0xaf8787, 0xaf87af, 0xaf87d7, 0xaf87ff,
  0xafaf00, 0xafaf5f, 0xafaf87, 0xafafaf, 0xafafd7, 0xafafff,
  0xafd700, 0xafd75f, 0xafd787, 0xafd7af, 0xafd7d7, 0xafd7ff,
  0xafff00, 0xafff5f, 0xafff87, 0xafffaf, 0xafffd7, 0xafffff,
  // 160
  0xd70000, 0xd7005f, 0xd70087, 0xd700af, 0xd700d7, 0xd700ff,
  0xd75f00, 0xd75f5f, 0xd75f87, 0xd75faf, 0xd75fd7, 0xd75fff,
  0xd78700, 0xd7875f, 0xd78787, 0xd787af, 0xd787d7, 0xd787ff,
  0xd7af00, 0xd7af5f, 0xd7af87, 0xd7afaf, 0xd7afd7, 0xd7afff,
  0xd7d700, 0xd7d75f, 0xd7d787, 0xd7d7af, 0xd7d7d7, 0xd7d7ff,
  0xd7ff00, 0xd7ff5f, 0xd7ff87, 0xd7ffaf, 0xd7ffd7, 0xd7ffff,
  // 196
  0xff0000, 0xff005f, 0xff0087, 0xff00af, 0xff00d7, 0xff00ff,
  0xff5f00, 0xff5f5f, 0xff5f87, 0xff5faf, 0xff5fd7, 0xff5fff,
  0xff8700, 0xff875f, 0xff8787, 0xff87af, 0xff87d7, 0xff87ff,
  0xffaf00, 0xffaf5f, 0xffaf87, 0xffafaf, 0xffafd7, 0xffafff,
  0xffd700, 0xffd75f, 0xffd787, 0xffd7af, 0xffd7d7, 0xffd7ff,
  0xffff00, 0xffff5f, 0xffff87, 0xffffaf, 0xffffd7, 0xffffff,
  // 232, gray scale
  0x080808, 0x121212, 0x1c1c1c, 0x262626, 0x303030, 0x3a3a3a, 
  0x444444, 0x4e4e4e, 0x585858, 0x626262, 0x6c6c6c, 0x767676, 
  0x808080, 0x8a8a8a, 0x949494, 0x9e9e9e, 0xa8a8a8, 0xb2b2b2, 
  0xbcbcbc, 0xc6c6c6, 0xd0d0d0, 0xdadada, 0xe4e4e4, 0xeeeeee   
};


//-------------------------------------------------------------
// Create colors
//-------------------------------------------------------------

// Create a color from a 24-bit color value.
ic_private ic_color_t ic_rgb(uint32_t hex) {
  return (ic_color_t)(0x1000000 | (hex & 0xFFFFFF));
}

// Limit an int to values between 0 and 255.
static uint32_t ic_cap8(ssize_t i) {
  return (i < 0 ? 0 : (i > 255 ? 255 : (uint32_t)i));
}

// Create a color from a 24-bit color value.
ic_private ic_color_t ic_rgbx(ssize_t r, ssize_t g, ssize_t b) {
  return ic_rgb( (ic_cap8(r)<<16) | (ic_cap8(g)<<8) | ic_cap8(b) );
}


//-------------------------------------------------------------
// Match an rgb color to a ansi8, ansi16, or ansi256
//-------------------------------------------------------------

static bool color_is_rgb( ic_color_t color ) {
  return (color >= IC_RGB(0));  // bit 24 is set for rgb colors
}

static void color_to_rgb(ic_color_t color, int* r, int* g, int* b) {
  assert(color_is_rgb(color));
  *r = ((color >> 16) & 0xFF);
  *g = ((color >> 8) & 0xFF);
  *b = (color & 0xFF);
}

ic_private ic_color_t color_from_ansi256(ssize_t i) {
  if (i >= 0 && i < 8) {
    return (IC_ANSI_BLACK + (uint32_t)i);
  }
  else if (i >= 8 && i < 16) {
    return (IC_ANSI_DARKGRAY + (uint32_t)(i - 8));
  }
  else if (i >= 16 && i <= 255) {
    return ic_rgb( ansi256[i] );
  }
  else if (i == 256) {
    return IC_ANSI_DEFAULT;
  }
  else {
    return IC_ANSI_DEFAULT;
  }
}

static bool is_grayish(int r, int g, int b) {
  return (abs(r-g) <= 4) && (abs((r+g)/2 - b) <= 4);
}

static bool is_grayish_color( uint32_t rgb ) {
  int r, g, b;
  color_to_rgb(IC_RGB(rgb),&r,&g,&b);
  return is_grayish(r,g,b);
}

static int_least32_t sqr(int_least32_t x) {
  return x*x;
}

// Approximation to delta-E CIE color distance using much 
// simpler calculations. See <https://www.compuphase.com/cmetric.htm>.
// This is essentialy weighted euclidean distance but the weight distribution
// depends on how big the "red" component of the color is.
// We do not take the square root as we only need to find 
// the minimal distance (and multiply by 256 to increase precision).
// Needs at least 28-bit signed integers to avoid overflow. 
static int_least32_t rgb_distance_rmean( uint32_t color, int r2, int g2, int b2 ) {
  int r1, g1, b1;
  color_to_rgb(IC_RGB(color),&r1,&g1,&b1);
  int_least32_t rmean = (r1 + r2) / 2;
  int_least32_t dr2 = sqr(r1 - r2);
  int_least32_t dg2 = sqr(g1 - g2);
  int_least32_t db2 = sqr(b1 - b2);
  int_least32_t dist = ((512+rmean)*dr2) + 1024*dg2 + ((767-rmean)*db2);    
  return dist;
}

// Another approximation to delta-E CIE color distance using
// simpler calculations. Similar to `rmean` but adds an adjustment factor
// based on the "red/blue" difference.
static int_least32_t rgb_distance_rbmean( uint32_t color, int r2, int g2, int b2 ) {
  int r1, g1, b1;
  color_to_rgb(IC_RGB(color),&r1,&g1,&b1);
  int_least32_t rmean = (r1 + r2) / 2;
  int_least32_t dr2 = sqr(r1 - r2);
  int_least32_t dg2 = sqr(g1 - g2);
  int_least32_t db2 = sqr(b1 - b2);
  int_least32_t dist = 2*dr2 + 4*dg2 + 3*db2 + ((rmean*(dr2 - db2))/256);  
  return dist;
}


// Maintain a small cache of recently used colors. Should be short enough to be effectively constant time.
// If we ever use a more expensive color distance method, we may increase the size a bit (64?) 
// (Initial zero initialized cache is valid.)
#define RGB_CACHE_LEN (16)
typedef struct rgb_cache_s {
  int        last;
  int        indices[RGB_CACHE_LEN];
  ic_color_t colors[RGB_CACHE_LEN];
} rgb_cache_t;

// remember a color in the LRU cache
//uh: make it private
ic_private void rgb_remember( rgb_cache_t* cache, ic_color_t color, int idx ) {
  if (cache == NULL) return;
  cache->colors[cache->last] = color;
  cache->indices[cache->last] = idx;
  cache->last++;
  if (cache->last >= RGB_CACHE_LEN) { cache->last = 0; }
}

// quick lookup in cache; -1 on failure
//uh: make it private
ic_private int rgb_lookup( const rgb_cache_t* cache, ic_color_t color ) {
  if (cache != NULL) {
    for(int i = 0; i < RGB_CACHE_LEN; i++) {
      if (cache->colors[i] == color) return cache->indices[i];
    }
  }
  return -1;
}

// return the index of the closest matching color
static int rgb_match( uint32_t* palette, int start, int len, rgb_cache_t* cache, ic_color_t color ) {
  assert(color_is_rgb(color));
  // in cache?
  int min = rgb_lookup(cache,color);
  if (min >= 0) {
    return min;
  }
  // otherwise find closest color match in the palette
  int r, g, b;
  color_to_rgb(color,&r,&g,&b);
  min = start;
  int_least32_t mindist = (INT_LEAST32_MAX)/4;
  for(int i = start; i < len; i++) {
    //int_least32_t dist = rgb_distance_rbmean(palette[i],r,g,b);
    int_least32_t dist = rgb_distance_rmean(palette[i],r,g,b);
    if (is_grayish_color(palette[i]) != is_grayish(r, g, b)) { 
      // with few colors, make it less eager to substitute a gray for a non-gray (or the other way around)
      if (len <= 16) {
        dist *= 4;
      } 
      else {
        dist = (dist/4)*5;
      }
    } 
    if (dist < mindist) {
      min = i;
      mindist = dist;
    }
  }
  rgb_remember(cache,color,min);
  return min;
}


// Match RGB to an index in the ANSI 256 color table
static int rgb_to_ansi256(ic_color_t color) {
  static rgb_cache_t ansi256_cache;
  int c = rgb_match(ansi256, 16, 256, &ansi256_cache, color); // not the first 16 ANSI colors as those may be different 
  //debug_msg("term: rgb %x -> ansi 256: %d\n", color, c );
  return c;
}

// Match RGB to an ANSI 16 color code (30-37, 90-97)
static int color_to_ansi16(ic_color_t color) {
  if (!color_is_rgb(color)) {
    return (int)color;
  }
  else {
    static rgb_cache_t ansi16_cache;
    int c = rgb_match(ansi256, 0, 16, &ansi16_cache, color);
    //debug_msg("term: rgb %x -> ansi 16: %d\n", color, c );
    return (c < 8 ? 30 + c : 90 + c - 8); 
  }
}

// Match RGB to an ANSI 16 color code (30-37, 90-97)
// but assuming the bright colors are simulated using 'bold'.
static int color_to_ansi8(ic_color_t color) {
  if (!color_is_rgb(color)) {
    return (int)color;
  }
  else {
    // match to basic 8 colors first
    static rgb_cache_t ansi8_cache;
    int c = 30 + rgb_match(ansi256, 0, 8, &ansi8_cache, color);
    // and then adjust for brightness
    int r, g, b;
    color_to_rgb(color,&r,&g,&b);
    if (r>=196 || g>=196 || b>=196) c += 60;
    //debug_msg("term: rgb %x -> ansi 8: %d\n", color, c );
    return c;
  }
}


//-------------------------------------------------------------
// Emit color escape codes based on the terminal capability
//-------------------------------------------------------------

static void fmt_color_ansi8( char* buf, ssize_t len, ic_color_t color, bool bg ) {
  int c = color_to_ansi8(color) + (bg ? 10 : 0);
  if (c >= 90) {
    snprintf(buf, to_size_t(len), IC_CSI "1;%dm", c - 60);    
  }
  else {
    snprintf(buf, to_size_t(len), IC_CSI "22;%dm", c );  
  }
}

static void fmt_color_ansi16( char* buf, ssize_t len, ic_color_t color, bool bg ) {
  snprintf( buf, to_size_t(len), IC_CSI "%dm", color_to_ansi16(color) + (bg ? 10 : 0) );  
}

static void fmt_color_ansi256( char* buf, ssize_t len,  ic_color_t color, bool bg ) {
  if (!color_is_rgb(color)) {
    fmt_color_ansi16(buf,len,color,bg);
  }
  else {
    snprintf( buf, to_size_t(len), IC_CSI "%d;5;%dm", (bg ? 48 : 38), rgb_to_ansi256(color) );  
  }
}

static void fmt_color_rgb( char* buf, ssize_t len, ic_color_t color, bool bg ) {
  if (!color_is_rgb(color)) {
    fmt_color_ansi16(buf,len,color,bg);
  }
  else {
    int r,g,b;
    color_to_rgb(color, &r,&g,&b);
    snprintf( buf, to_size_t(len), IC_CSI "%d;2;%d;%d;%dm", (bg ? 48 : 38), r, g, b );  
  }
}

static void fmt_color_ex(char* buf, ssize_t len, palette_t palette, ic_color_t color, bool bg) {
  if (color == IC_COLOR_NONE || palette == MONOCHROME) return;
  if (palette == ANSI8) {
    fmt_color_ansi8(buf,len,color,bg);
  }
  else if (!color_is_rgb(color) || palette == ANSI16) {
    fmt_color_ansi16(buf,len,color,bg);
  }
  else if (palette == ANSI256) {
    fmt_color_ansi256(buf,len,color,bg);
  }
  else {
    fmt_color_rgb(buf,len,color,bg);
  }
}

static void term_color_ex(term_t* term, ic_color_t color, bool bg) {
  char buf[128+1];
  fmt_color_ex(buf,128,term->palette,color,bg);
  term_write(term,buf);
}

//-------------------------------------------------------------
// Main API functions
//-------------------------------------------------------------

ic_private void term_color(term_t* term, ic_color_t color) {
  term_color_ex(term,color,false);
}

ic_private void term_bgcolor(term_t* term, ic_color_t color) {
  term_color_ex(term,color,true);
}

ic_private void term_append_color(term_t* term, stringbuf_t* sbuf, ic_color_t color) {
  char buf[128+1];
  fmt_color_ex(buf,128,term->palette,color,false);
  sbuf_append(sbuf,buf);
}

ic_private void term_append_bgcolor(term_t* term, stringbuf_t* sbuf, ic_color_t color) {
  char buf[128+1];
  fmt_color_ex(buf, 128, term->palette, color, true);
  sbuf_append(sbuf, buf);
}

ic_private int term_get_color_bits(term_t* term) {
  switch (term->palette) {
  case MONOCHROME: return 1;
  case ANSI8:      return 3;
  case ANSI16:     return 4;
  case ANSI256:    return 8;
  case ANSIRGB:    return 24;
  default:         return 4;
  }
}

//-------------------------------------------------------------
// Helpers
//-------------------------------------------------------------

ic_private void term_left(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, IC_CSI "%zdD", n );
}

ic_private void term_right(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, IC_CSI "%zdC", n );
}

ic_private void term_up(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, IC_CSI "%zdA", n );
}

ic_private void term_down(term_t* term, ssize_t n) {
  if (n <= 0) return;
  term_writef( term, IC_CSI "%zdB", n );
}

ic_private void term_clear_line(term_t* term) {
  term_write( term, "\r" IC_CSI "K");
}

ic_private void term_clear_to_end_of_line(term_t* term) {
  term_write(term, IC_CSI "K");
}

ic_private void term_start_of_line(term_t* term) {
  term_write( term, "\r" );
}

ic_private ssize_t term_get_width(term_t* term) {
  return term->width;
}

ic_private ssize_t term_get_height(term_t* term) {
  return term->height;
}

ic_private void term_attr_reset(term_t* term) {
  term_write(term, IC_CSI "m" );
}

ic_private void term_underline(term_t* term, bool on) {
  term_write(term, on ? IC_CSI "4m" : IC_CSI "24m" );
}

ic_private void term_reverse(term_t* term, bool on) {
  term_write(term, on ? IC_CSI "7m" : IC_CSI "27m");
}

ic_private void term_bold(term_t* term, bool on) {
  term_write(term, on ? IC_CSI "1m" : IC_CSI "22m" );
}

ic_private void term_italic(term_t* term, bool on) {
  term_write(term, on ? IC_CSI "3m" : IC_CSI "23m" );
}

ic_private void term_writeln(term_t* term, const char* s) {
  term_write(term,s);
  term_write(term,"\n");
}

ic_private void term_write_char(term_t* term, char c) {
  char buf[2];
  buf[0] = c;
  buf[1] = 0;
  term_write_n(term, buf, 1 );
}

ic_private attr_t term_get_attr( const term_t* term ) {
  return term->attr;
}

ic_private void term_set_attr( term_t* term, attr_t attr ) {
  if (term->nocolor) return;
  if (attr.x.color != term->attr.x.color && attr.x.color != IC_COLOR_NONE) {
    term_color(term,attr.x.color);
    if (term->palette < ANSIRGB && color_is_rgb(attr.x.color)) {
      term->attr.x.color = attr.x.color; // actual color may have been approximated but we keep the actual color to avoid updating every time
    }
  }
  if (attr.x.bgcolor != term->attr.x.bgcolor && attr.x.bgcolor != IC_COLOR_NONE) {
    term_bgcolor(term,attr.x.bgcolor);
    if (term->palette < ANSIRGB && color_is_rgb(attr.x.bgcolor)) {
      term->attr.x.bgcolor = attr.x.bgcolor; 
    }
  }
  if (attr.x.bold != term->attr.x.bold && attr.x.bold != IC_NONE) {
    term_bold(term,attr.x.bold == IC_ON);
  }
  if (attr.x.underline != term->attr.x.underline && attr.x.underline != IC_NONE) {
    term_underline(term,attr.x.underline == IC_ON);
  }
  if (attr.x.reverse != term->attr.x.reverse && attr.x.reverse != IC_NONE) {
    term_reverse(term,attr.x.reverse == IC_ON);
  }
  if (attr.x.italic != term->attr.x.italic && attr.x.italic != IC_NONE) {
    term_italic(term,attr.x.italic == IC_ON);
  }  
  assert(attr.x.color == term->attr.x.color || attr.x.color == IC_COLOR_NONE);
  assert(attr.x.bgcolor == term->attr.x.bgcolor || attr.x.bgcolor == IC_COLOR_NONE);
  assert(attr.x.bold == term->attr.x.bold || attr.x.bold == IC_NONE);
  assert(attr.x.reverse == term->attr.x.reverse || attr.x.reverse == IC_NONE);
  assert(attr.x.underline == term->attr.x.underline || attr.x.underline == IC_NONE);
  assert(attr.x.italic == term->attr.x.italic || attr.x.italic == IC_NONE);
}


/*
ic_private void term_clear_lines_to_end(term_t* term) {
  term_write(term, "\r" IC_CSI "J");
}

ic_private void term_show_cursor(term_t* term, bool on) {
  term_write(term, on ? IC_CSI "?25h" : IC_CSI "?25l");
}
*/

//-------------------------------------------------------------
// Formatted output
//-------------------------------------------------------------

ic_private void term_writef(term_t* term, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  term_vwritef(term,fmt,ap);
  va_end(ap);  
}

ic_private void term_vwritef(term_t* term, const char* fmt, va_list args ) {
  sbuf_append_vprintf(term->buf, fmt, args);
}

ic_private void term_write_formatted( term_t* term, const char* s, const attr_t* attrs ) {
  term_write_formatted_n( term, s, attrs, ic_strlen(s));
}

ic_private void term_write_formatted_n( term_t* term, const char* s, const attr_t* attrs, ssize_t len ) {
  if (attrs == NULL) {
    // write directly
    term_write(term,s);
  }
  else {
    // ensure raw mode from now on
    if (term->raw_enabled <= 0) {
      term_start_raw(term);
    }
    // and output with text attributes
    const attr_t default_attr = term_get_attr(term);
    attr_t attr = attr_none();
    ssize_t i = 0;
    ssize_t n = 0;
    while( i+n < len && s[i+n] != 0 ) {
      if (!attr_is_eq(attr,attrs[i+n])) {
        if (n > 0) { 
          term_write_n( term, s+i, n );
          i += n;
          n = 0;
        }
        attr = attrs[i];
        term_set_attr( term, attr_update_with(default_attr,attr) );
      }  
      n++;    
    }
    if (n > 0) {
      term_write_n( term, s+i, n );
      i += n;
      n = 0;    
    }
    assert(s[i] != 0 || i == len);
    term_set_attr(term, default_attr);
  }
}

//-------------------------------------------------------------
// Write to the terminal
// The buffered functions are used to reduce cursor flicker
// during refresh
//-------------------------------------------------------------

ic_private void term_beep(term_t* term) {
  if (term->silent) return;
  fprintf(stderr,"\x7");
  fflush(stderr);
}

ic_private void term_write_repeat(term_t* term, const char* s, ssize_t count) {
  for (; count > 0; count--) {
    term_write(term, s);
  }
}

ic_private void term_write(term_t* term, const char* s) {
  if (s == NULL || s[0] == 0) return;
  ssize_t n = ic_strlen(s);
  term_write_n(term,s,n);
}

// Primitive terminal write; all writes go through here
ic_private void term_write_n(term_t* term, const char* s, ssize_t n) {
  if (s == NULL || n <= 0) return;
  // write to buffer to reduce flicker and to process escape sequences (this may flush too)
  term_append_buf(term, s, n);  
}


//-------------------------------------------------------------
// Buffering
//-------------------------------------------------------------


ic_private void term_flush(term_t* term) {
  if (sbuf_len(term->buf) > 0) {
    //term_show_cursor(term,false);
    term_write_direct(term, sbuf_string(term->buf), sbuf_len(term->buf));
    //term_show_cursor(term,true);
    sbuf_clear(term->buf);
  }  
}

ic_private buffer_mode_t term_set_buffer_mode(term_t* term, buffer_mode_t mode) {
  buffer_mode_t oldmode = term->bufmode;
  if (oldmode != mode) {
    if (mode == UNBUFFERED) {
      term_flush(term);
    }
    term->bufmode = mode;
  }
  return oldmode;
}

static void term_check_flush(term_t* term, bool contains_nl) {
  if (term->bufmode == UNBUFFERED || 
      sbuf_len(term->buf) > 4000 ||
      (term->bufmode == LINEBUFFERED && contains_nl)) 
  {
    term_flush(term);
  }  
}

//-------------------------------------------------------------
// Init
//-------------------------------------------------------------

static void term_init_raw(term_t* term);

ic_private term_t* term_new(alloc_t* mem, tty_t* tty, bool nocolor, bool silent, int fd_out ) 
{
  term_t* term = mem_zalloc_tp(mem, term_t);
  if (term == NULL) return NULL;

  term->fd_out  = (fd_out < 0 ? STDOUT_FILENO : fd_out);
  term->nocolor = nocolor || (isatty(term->fd_out) == 0);
  term->silent  = silent;  
  term->mem     = mem;
  term->tty     = tty;     // can be NULL
  term->width   = 80;
  term->height  = 25;
  term->is_utf8 = tty_is_utf8(tty);
  term->palette = ANSI16; // almost universally supported
  term->buf     = sbuf_new(mem);  
  term->bufmode = LINEBUFFERED;
  term->attr    = attr_default();

  // respect NO_COLOR
  if (getenv("NO_COLOR") != NULL) {
    term->nocolor = true;
  }
  if (!term->nocolor) {
    // detect color palette
    // COLORTERM takes precedence
    const char* colorterm = getenv("COLORTERM");  
    const char* eterm = getenv("TERM");    
    if (ic_contains(colorterm,"24bit") || ic_contains(colorterm,"truecolor") || ic_contains(colorterm,"direct")) { 
      term->palette = ANSIRGB; 
    }
    else if (ic_contains(colorterm,"8bit") || ic_contains(colorterm,"256color")) { term->palette = ANSI256; } 
    else if (ic_contains(colorterm,"4bit") || ic_contains(colorterm,"16color"))  { term->palette = ANSI16; }
    else if (ic_contains(colorterm,"3bit") || ic_contains(colorterm,"8color"))   { term->palette = ANSI8; }
    else if (ic_contains(colorterm,"1bit") || ic_contains(colorterm,"nocolor") || ic_contains(colorterm,"monochrome")) { 
      term->palette = MONOCHROME; 
    }
    // otherwise check for some specific terminals
    else if (getenv("WT_SESSION") != NULL) { term->palette = ANSIRGB; } // Windows terminal
    else if (getenv("ITERM_SESSION_ID") != NULL) { term->palette = ANSIRGB; } // iTerm2 terminal
    else if (getenv("VSCODE_PID") != NULL) { term->palette = ANSIRGB; } // vscode terminal
    else {
      // and otherwise fall back to checking TERM
      if (ic_contains(eterm,"truecolor") || ic_contains(eterm,"direct") || ic_contains(colorterm,"24bit")) {
        term->palette = ANSIRGB;
      }
      else if (ic_contains(eterm,"alacritty") || ic_contains(eterm,"kitty")) {
        term->palette = ANSIRGB;
      }
      else if (ic_contains(eterm,"256color") || ic_contains(eterm,"gnome")) { 
        term->palette = ANSI256;
      }  
      else if (ic_contains(eterm,"16color")){ term->palette = ANSI16; }
      else if (ic_contains(eterm,"8color")) { term->palette = ANSI8; }
      else if (ic_contains(eterm,"monochrome") || ic_contains(eterm,"nocolor") || ic_contains(eterm,"dumb")) { 
        term->palette = MONOCHROME; 
      }
    }
    debug_msg("term: color-bits: %d (COLORTERM=%s, TERM=%s)\n", term_get_color_bits(term), colorterm, eterm);
  }
  
  // read COLUMS/LINES from the environment for a better initial guess.
  const char* env_columns = getenv("COLUMNS");
  if (env_columns != NULL) { ic_atoz(env_columns, &term->width); }
  const char* env_lines = getenv("LINES");
  if (env_lines != NULL)   { ic_atoz(env_lines, &term->height); }
  
  // initialize raw terminal output and terminal dimensions
  term_init_raw(term);
  term_update_dim(term);
  term_attr_reset(term);  // ensure we are at default settings

  return term;
}

ic_private bool term_is_interactive(const term_t* term) {
  ic_unused(term);
  // check dimensions (0 is used for debuggers)
  // if (term->width <= 0) return false; 
  
  // check editing support
  const char* eterm = getenv("TERM");
  debug_msg("term: TERM=%s\n", eterm);
  if (eterm != NULL &&
      (strstr("dumb|DUMB|cons25|CONS25|emacs|EMACS",eterm) != NULL)) {
    return false;
  }

  return true;
}

ic_private bool term_enable_beep(term_t* term, bool enable) {
  bool prev = term->silent;
  term->silent = !enable;
  return prev;
}

ic_private bool term_enable_color(term_t* term, bool enable) {
  bool prev = !term->nocolor;
  term->nocolor = !enable;
  return prev;
}

ic_private void term_free(term_t* term) {
  if (term == NULL) return;
  term_flush(term);
  term_end_raw(term, true);
  sbuf_free(term->buf); term->buf = NULL;
  mem_free(term->mem, term);
}

//-------------------------------------------------------------
// For best portability and applications inserting CSI SGR (ESC[ .. m)
// codes themselves in strings, we interpret these at the 
// lowest level so we can have a `term_get_attr` function which
// is needed for bracketed styles etc.
//-------------------------------------------------------------

static void term_append_esc(term_t* term, const char* const s, ssize_t len) {
  if (s[1]=='[' && s[len-1] == 'm') {    
    // it is a CSI SGR sequence: ESC[ ... m
    if (term->nocolor) return;       // ignore escape sequences if nocolor is set
    term->attr = attr_update_with(term->attr, attr_from_esc_sgr(s,len));
  }
  // and write out the escape sequence as-is
  sbuf_append_n(term->buf, s, len);
}


static void term_append_utf8(term_t* term, const char* s, ssize_t len) {
  ssize_t nread;
  unicode_t uchr = unicode_from_qutf8((const uint8_t*)s, len, &nread);
  uint8_t c;
  if (unicode_is_raw(uchr, &c)) {
    // write bytes as is; this also ensure that on non-utf8 terminals characters between 0x80-0xFF
    // go through _as is_ due to the qutf8 encoding.
    sbuf_append_char(term->buf,(char)c);
  }
  else if (!term->is_utf8) {
    // on non-utf8 terminals still send utf-8 and hope for the best
    // todo: we could try to convert to the locale first?
    sbuf_append_n(term->buf, s, len);
    // sbuf_appendf(term->buf, "\x1B[%" PRIu32 "u", uchr); // unicode escape code
  }
  else {
    // write utf-8 as is
    sbuf_append_n(term->buf, s, len);
  }
}

static void term_append_buf( term_t* term, const char* s, ssize_t len ) {
  ssize_t pos = 0;
  bool newline = false;
  while (pos < len) {
    // handle ascii sequences in bulk
    ssize_t ascii = 0;
    ssize_t next;
    while ((next = str_next_ofs(s, len, pos+ascii, NULL)) > 0 && 
            (uint8_t)s[pos + ascii] > '\x1B' && (uint8_t)s[pos + ascii] <= 0x7F ) 
    {
      ascii += next;      
    }
    if (ascii > 0) {
      sbuf_append_n(term->buf, s+pos, ascii);
      pos += ascii;
    }
    if (next <= 0) break;

    const uint8_t c = (uint8_t)s[pos];
    // handle utf8 sequences (for non-utf8 terminals)
    if (c >= 0x80) {
      term_append_utf8(term, s+pos, next);
    }
    // handle escape sequence (note: str_next_ofs considers whole CSI escape sequences at a time)
    else if (next > 1 && c == '\x1B') {
      term_append_esc(term, s+pos, next);
    }
    else if (c < ' ' && c != 0 && (c < '\x07' || c > '\x0D')) {
      // ignore control characters except \a, \b, \t, \n, \r, and form-feed and vertical tab.
    }
    else {
      if (c == '\n') { newline = true; }
      sbuf_append_n(term->buf, s+pos, next);
    }
    pos += next;
  }  
  // possibly flush
  term_check_flush(term, newline);  
}

//-------------------------------------------------------------
// Platform dependent: Write directly to the terminal
//-------------------------------------------------------------

#if !defined(_WIN32)

// write to the console without further processing
static bool term_write_direct(term_t* term, const char* s, ssize_t n) {
  ssize_t count = 0; 
  while( count < n ) {
    ssize_t nwritten = write(term->fd_out, s + count, to_size_t(n - count));
    if (nwritten > 0) {
      count += nwritten;
    }
    else if (errno != EINTR && errno != EAGAIN) {
      debug_msg("term: write failed: length %i, errno %i: \"%s\"\n", n, errno, s);
      return false;
    }
  }
  return true;
}

#else

//----------------------------------------------------------------------------------
// On windows we use the new virtual terminal processing if it is available (Windows Terminal)
// but fall back to  ansi escape emulation on older systems but also for example
// the PS terminal
//
// note: we use row/col as 1-based ANSI escape while windows X/Y coords are 0-based.
//-----------------------------------------------------------------------------------

#if !defined(ENABLE_VIRTUAL_TERMINAL_PROCESSING)
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING (0)
#endif
#if !defined(ENABLE_LVB_GRID_WORLDWIDE)
#define ENABLE_LVB_GRID_WORLDWIDE (0)
#endif

// direct write to the console without further processing
static bool term_write_console(term_t* term, const char* s, ssize_t n ) {
  DWORD written;
  // WriteConsoleA(term->hcon, s, (DWORD)(to_size_t(n)), &written, NULL);
  WriteFile(term->hcon, s, (DWORD)(to_size_t(n)), &written, NULL); // so it can be redirected
  return (written == (DWORD)(to_size_t(n)));
}

static bool term_get_cursor_pos( term_t* term, ssize_t* row, ssize_t* col) {
  *row = 0;
  *col = 0;
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo(term->hcon, &info)) return false;
  *row = (ssize_t)info.dwCursorPosition.Y + 1;
  *col = (ssize_t)info.dwCursorPosition.X + 1;
  return true;
}

static void term_move_cursor_to( term_t* term, ssize_t row, ssize_t col ) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo( term->hcon, &info )) return;
  if (col > info.dwSize.X) col = info.dwSize.X;
  if (row > info.dwSize.Y) row = info.dwSize.Y;
  if (col <= 0) col = 1;
  if (row <= 0) row = 1;
  COORD coord;
  coord.X = (SHORT)col - 1;
  coord.Y = (SHORT)row - 1;
  SetConsoleCursorPosition( term->hcon, coord);
}

static void term_cursor_save(term_t* term) {
  memset(&term->hcon_save_cursor, 0, sizeof(term->hcon_save_cursor));
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo(term->hcon, &info)) return;
  term->hcon_save_cursor = info.dwCursorPosition;
}

static void term_cursor_restore(term_t* term) {
  if (term->hcon_save_cursor.X == 0) return;
  SetConsoleCursorPosition(term->hcon, term->hcon_save_cursor);
}

static void term_move_cursor( term_t* term, ssize_t drow, ssize_t dcol, ssize_t n ) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo( term->hcon, &info )) return;
  COORD cur = info.dwCursorPosition;
  ssize_t col = (ssize_t)cur.X + 1 + n*dcol;
  ssize_t row = (ssize_t)cur.Y + 1 + n*drow;
  term_move_cursor_to( term, row, col );
}

static void term_cursor_visible( term_t* term, bool visible ) {
  CONSOLE_CURSOR_INFO info;
  if (!GetConsoleCursorInfo(term->hcon,&info)) return;
  info.bVisible = visible;
  SetConsoleCursorInfo(term->hcon,&info);
}

static void term_erase_line( term_t* term, ssize_t mode ) {  
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo( term->hcon, &info )) return;
  DWORD written;
  COORD start;
  ssize_t length;
  if (mode == 2) {
    // entire line
    start.X = 0;
    start.Y = info.dwCursorPosition.Y;
    length = (ssize_t)info.srWindow.Right + 1;
  }
  else if (mode == 1) {
    // to start of line
    start.X = 0;
    start.Y = info.dwCursorPosition.Y;
    length  = info.dwCursorPosition.X;
  }
  else {
    // to end of line    
    length = (ssize_t)info.srWindow.Right - info.dwCursorPosition.X + 1;
    start = info.dwCursorPosition;
  }
  FillConsoleOutputAttribute( term->hcon, term->hcon_default_attr, (DWORD)length, start, &written );
  FillConsoleOutputCharacterA( term->hcon, ' ', (DWORD)length, start, &written );
}

static void term_clear_screen(term_t* term, ssize_t mode) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo(term->hcon, &info)) return;
  COORD start;
  start.X = 0;
  start.Y = 0;
  ssize_t length;
  ssize_t width = (ssize_t)info.dwSize.X;
  if (mode == 2) {
    // entire screen
    length = width * info.dwSize.Y;    
  }
  else if (mode == 1) {
    // to cursor
    length = (width * ((ssize_t)info.dwCursorPosition.Y - 1)) + info.dwCursorPosition.X;
  }
  else {
    // from cursor
    start  = info.dwCursorPosition;
    length = (width * ((ssize_t)info.dwSize.Y - info.dwCursorPosition.Y)) + (width - info.dwCursorPosition.X + 1);
  }
  DWORD written;
  FillConsoleOutputAttribute(term->hcon, term->hcon_default_attr, (DWORD)length, start, &written);
  FillConsoleOutputCharacterA(term->hcon, ' ', (DWORD)length, start, &written);
}

static WORD attr_color[8] = {
  0,                                  // black
  FOREGROUND_RED,                     // maroon
  FOREGROUND_GREEN,                   // green
  FOREGROUND_RED | FOREGROUND_GREEN,  // orange
  FOREGROUND_BLUE,                    // navy
  FOREGROUND_RED | FOREGROUND_BLUE,   // purple
  FOREGROUND_GREEN | FOREGROUND_BLUE, // teal
  FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE, // light gray
};

static void term_set_win_attr( term_t* term, attr_t ta ) {
  WORD def_attr = term->hcon_default_attr;
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo( term->hcon, &info )) return;  
  WORD cur_attr = info.wAttributes;
  WORD attr = cur_attr; 
  if (ta.x.color != IC_COLOR_NONE) {
    if (ta.x.color >= IC_ANSI_BLACK && ta.x.color <= IC_ANSI_SILVER) {
      attr = (attr & 0xFFF0) | attr_color[ta.x.color - IC_ANSI_BLACK];
    }
    else if (ta.x.color >= IC_ANSI_GRAY && ta.x.color <= IC_ANSI_WHITE) {
      attr = (attr & 0xFFF0) | attr_color[ta.x.color - IC_ANSI_GRAY] | FOREGROUND_INTENSITY;
    }
    else if (ta.x.color == IC_ANSI_DEFAULT) {
      attr = (attr & 0xFFF0) | (def_attr & 0x000F);
    }
  }
  if (ta.x.bgcolor != IC_COLOR_NONE) {
    if (ta.x.bgcolor >= IC_ANSI_BLACK && ta.x.bgcolor <= IC_ANSI_SILVER) {
      attr = (attr & 0xFF0F) | (WORD)(attr_color[ta.x.bgcolor - IC_ANSI_BLACK] << 4);
    }
    else if (ta.x.bgcolor >= IC_ANSI_GRAY && ta.x.bgcolor <= IC_ANSI_WHITE) {
      attr = (attr & 0xFF0F) | (WORD)(attr_color[ta.x.bgcolor - IC_ANSI_GRAY] << 4) | BACKGROUND_INTENSITY;
    } 
    else if (ta.x.bgcolor == IC_ANSI_DEFAULT) {
      attr = (attr & 0xFF0F) | (def_attr & 0x00F0);
    }
  }
  if (ta.x.underline != IC_NONE) {
    attr = (attr & ~COMMON_LVB_UNDERSCORE) | (ta.x.underline == IC_ON ? COMMON_LVB_UNDERSCORE : 0);
  }
  if (ta.x.reverse != IC_NONE) {
    attr = (attr & ~COMMON_LVB_REVERSE_VIDEO) | (ta.x.reverse == IC_ON ? COMMON_LVB_REVERSE_VIDEO : 0);
  }  
  if (attr != cur_attr) {
    SetConsoleTextAttribute(term->hcon, attr);
  }
}

static ssize_t esc_param( const char* s, ssize_t def ) {
  if (*s == '?') s++;
  ssize_t n = def;
  ic_atoz(s, &n);
  return n;
}

static void esc_param2( const char* s, ssize_t* p1, ssize_t* p2, ssize_t def ) {
  if (*s == '?') s++; 
  *p1 = def;
  *p2 = def;
  ic_atoz2(s, p1, p2);  
}

// Emulate escape sequences on older windows.
static void term_write_esc( term_t* term, const char* s, ssize_t len ) {
  ssize_t row;
  ssize_t col;

  if (s[1] == '[') {
    switch (s[len-1]) {
    case 'A':
      term_move_cursor(term, -1, 0, esc_param(s+2, 1));
      break;
    case 'B':
      term_move_cursor(term, 1, 0, esc_param(s+2, 1));
      break;
    case 'C':
      term_move_cursor(term, 0, 1, esc_param(s+2, 1));
      break;
    case 'D':
      term_move_cursor(term, 0, -1, esc_param(s+2, 1));
      break;
    case 'H': 
      esc_param2(s+2, &row, &col, 1);
      term_move_cursor_to(term, row, col);
      break;
    case 'K':
      term_erase_line(term, esc_param(s+2, 0));
      break;
    case 'm': 
      term_set_win_attr( term, attr_from_esc_sgr(s,len) ); 
      break;

    // support some less standard escape codes (currently not used by isocline)
    case 'E':  // line down
      term_get_cursor_pos(term, &row, &col);
      row += esc_param(s+2, 1);
      term_move_cursor_to(term, row, 1);
      break;
    case 'F':  // line up
      term_get_cursor_pos(term, &row, &col);
      row -= esc_param(s+2, 1);
      term_move_cursor_to(term, row, 1);
      break;
    case 'G':  // absolute column
      term_get_cursor_pos(term, &row, &col);
      col = esc_param(s+2, 1);
      term_move_cursor_to(term, row, col);
      break;
    case 'J': 
      term_clear_screen(term, esc_param(s+2, 0));
      break;
    case 'h':
      if (strncmp(s+2, "?25h", 4) == 0) {
        term_cursor_visible(term, true);
      }
      break;
    case 'l': 
      if (strncmp(s+2, "?25l", 4) == 0) {
        term_cursor_visible(term, false);
      }
      break;
    case 's': 
      term_cursor_save(term);
      break;    
    case 'u':
      term_cursor_restore(term);
      break;
    // otherwise ignore
    }
  }
  else if (s[1] == '7') {
    term_cursor_save(term);
  }
  else if (s[1] == '8') {
    term_cursor_restore(term);
  }
  else {
    // otherwise ignore
  }
}

static bool term_write_direct(term_t* term, const char* s, ssize_t len ) {
  term_cursor_visible(term,false); // reduce flicker
  ssize_t pos = 0;    
  if ((term->hcon_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0) {
    // use the builtin virtual terminal processing. (enables truecolor for example)
    term_write_console(term, s, len);   
    pos = len;
  }
  else {
    // emulate escape sequences
    while( pos < len ) {
      // handle non-control in bulk (including utf-8 sequences)
      // (We don't need to handle utf-8 separately as we set the codepage to always be in utf-8 mode)
      ssize_t nonctrl = 0;
      ssize_t next;
      while( (next = str_next_ofs( s, len, pos+nonctrl, NULL )) > 0 && 
              (uint8_t)s[pos + nonctrl] >= ' ' && (uint8_t)s[pos + nonctrl] <= 0x7F) {
        nonctrl += next;
      }
      if (nonctrl > 0) {
        term_write_console(term, s+pos, nonctrl);
        pos += nonctrl;
      }    
      if (next <= 0) break;

      if ((uint8_t)s[pos] >= 0x80) {
        // utf8 is already processed
        term_write_console(term, s+pos, next);
      }
      else if (next > 1 && s[pos] == '\x1B') {                                
        // handle control (note: str_next_ofs considers whole CSI escape sequences at a time)
        term_write_esc(term, s+pos, next);
      }
      else if (next == 1 && (s[pos] == '\r' || s[pos] == '\n' || s[pos] == '\t' || s[pos] == '\b')) {
        term_write_console( term, s+pos, next);
      }
      else {
        // ignore
      }
      pos += next;
    }
  }
  term_cursor_visible(term,true);
  assert(pos == len);
  return (pos == len); 

}
#endif



//-------------------------------------------------------------
// Update terminal dimensions
//-------------------------------------------------------------

#if !defined(_WIN32)

// send escape query that may return a response on the tty
static bool term_esc_query_raw( term_t* term, const char* query, char* buf, ssize_t buflen ) 
{
  if (buf==NULL || buflen <= 0 || query[0] == 0) return false;
  bool osc = (query[1] == ']');
  if (!term_write_direct(term, query, ic_strlen(query))) return false;
  debug_msg("term: read tty query response to: ESC %s\n", query + 1);  
  return tty_read_esc_response( term->tty, query[1], osc, buf, buflen );
}

static bool term_esc_query( term_t* term, const char* query, char* buf, ssize_t buflen ) 
{
  if (!tty_start_raw(term->tty)) return false;  
  bool ok = term_esc_query_raw(term,query,buf,buflen);  
  tty_end_raw(term->tty);
  return ok;
}

// get the cursor position via an ESC[6n
static bool term_get_cursor_pos( term_t* term, ssize_t* row, ssize_t* col) 
{
  // send escape query
  char buf[128];
  if (!term_esc_query(term,"\x1B[6n",buf,128)) return false; 
  if (!ic_atoz2(buf,row,col)) return false;
  return true;
}

static void term_set_cursor_pos( term_t* term, ssize_t row, ssize_t col ) {
  term_writef( term, IC_CSI "%zd;%zdH", row, col );
}

ic_private bool term_update_dim(term_t* term) {  
  ssize_t cols = 0;
  ssize_t rows = 0;
  struct winsize ws;
  if (ioctl(term->fd_out, TIOCGWINSZ, &ws) >= 0) {
    // ioctl succeeded
    cols = ws.ws_col;  // debuggers return 0 for the column
    rows = ws.ws_row;
  }
  else {
    // determine width by querying the cursor position
    debug_msg("term: ioctl term-size failed: %d,%d\n", ws.ws_row, ws.ws_col);
    ssize_t col0 = 0;
    ssize_t row0 = 0;
    if (term_get_cursor_pos(term,&row0,&col0)) {
      term_set_cursor_pos(term,999,999);
      ssize_t col1 = 0;
      ssize_t row1 = 0;
      if (term_get_cursor_pos(term,&row1,&col1)) {
        cols = col1;
        rows = row1;
      }
      term_set_cursor_pos(term,row0,col0);
    }
    else {
      // cannot query position
      // return 0 column
    }
  }

  // update width and return whether it changed.
  bool changed = (term->width != cols || term->height != rows);
  debug_msg("terminal dim: %zd,%zd: %s\n", rows, cols, changed ? "changed" : "unchanged");  
  if (cols > 0) { 
    term->width = cols;
    term->height = rows;
  }
  return changed;  
}

#else

ic_private bool term_update_dim(term_t* term) {
  if (term->hcon == 0) {
    term->hcon = GetConsoleWindow();
  }
  ssize_t rows = 0;
  ssize_t cols = 0;  
  CONSOLE_SCREEN_BUFFER_INFO sbinfo;  
  if (GetConsoleScreenBufferInfo(term->hcon, &sbinfo)) {
     cols = (ssize_t)sbinfo.srWindow.Right - (ssize_t)sbinfo.srWindow.Left + 1;
     rows = (ssize_t)sbinfo.srWindow.Bottom - (ssize_t)sbinfo.srWindow.Top + 1;
  }
  bool changed = (term->width != cols || term->height != rows);
  term->width = cols;
  term->height = rows;
  debug_msg("term: update dim: %zd, %zd\n", term->height, term->width );
  return changed;
}

#endif



//-------------------------------------------------------------
// Enable/disable terminal raw mode
//-------------------------------------------------------------

#if !defined(_WIN32)

// On non-windows, the terminal is set in raw mode by the tty.

ic_private void term_start_raw(term_t* term) {
  term->raw_enabled++;
}

ic_private void term_end_raw(term_t* term, bool force) {
  if (term->raw_enabled <= 0) return;
  if (!force) {
    term->raw_enabled--;
  }
  else {
    term->raw_enabled = 0;
  }
}

static bool term_esc_query_color_raw(term_t* term, int color_idx, uint32_t* color ) {
  char buf[128+1];
  snprintf(buf,128,"\x1B]4;%d;?\x1B\\", color_idx);
  if (!term_esc_query_raw( term, buf, buf, 128 )) {
    debug_msg("esc query response not received\n");
    return false;
  }
  if (buf[0] != '4') return false;
  const char* rgb = strchr(buf,':');
  if (rgb==NULL) return false;
  rgb++; // skip ':'
  unsigned int r,g,b;
  if (sscanf(rgb,"%x/%x/%x",&r,&g,&b) != 3) return false;
  if (rgb[2]!='/') { // 48-bit rgb, hexadecimal round to 24-bit     
    r = (r+0x7F)/0x100;   // note: can "overflow", e.g. 0xFFFF -> 0x100. (and we need `ic_cap8` to convert.)
    g = (g+0x7F)/0x100;
    b = (b+0x7F)/0x100; 
  }
  *color = (ic_cap8(r)<<16) | (ic_cap8(g)<<8) | ic_cap8(b);
  debug_msg("color query: %02x,%02x,%02x: %06x\n", r, g, b, *color);  
  return true;
}

// update ansi 16 color palette for better color approximation
static void term_update_ansi16(term_t* term) {
  debug_msg("update ansi colors\n");
  #if defined(GIO_CMAP)
  // try ioctl first (on Linux)
  uint8_t cmap[48];
  memset(cmap,0,48);
  if (ioctl(term->fd_out,GIO_CMAP,&cmap) >= 0) {
    // success
    for(ssize_t i = 0; i < 48; i+=3) {
      uint32_t color = ((uint32_t)(cmap[i]) << 16) | ((uint32_t)(cmap[i+1]) << 8) | cmap[i+2];
      debug_msg("term (ioctl) ansi color %d: 0x%06x\n", i, color);
      ansi256[i] = color;
    }
    return;
  }
  else {
    debug_msg("ioctl GIO_CMAP failed: entry 1: 0x%02x%02x%02x\n", cmap[3], cmap[4], cmap[5]);
  }
  #endif
  // this seems to be unreliable on some systems (Ubuntu+Gnome terminal) so only enable when known ok.
  #if __APPLE__
  // otherwise use OSC 4 escape sequence query
  if (tty_start_raw(term->tty)) {
    for(ssize_t i = 0; i < 16; i++) {
      uint32_t color;
      if (!term_esc_query_color_raw(term, i, &color)) break;
      debug_msg("term ansi color %d: 0x%06x\n", i, color);
      ansi256[i] = color;
    }  
    tty_end_raw(term->tty);  
  }
  #endif
}

static void term_init_raw(term_t* term) {
  if (term->palette < ANSIRGB) {
    term_update_ansi16(term);
  }
}

#else

ic_private void term_start_raw(term_t* term) {
  if (term->raw_enabled++ > 0) return;  
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (GetConsoleScreenBufferInfo(term->hcon, &info)) {
    term->hcon_orig_attr = info.wAttributes;
  }
  term->hcon_orig_cp = GetConsoleOutputCP();
  SetConsoleOutputCP(CP_UTF8);
  if (term->hcon_mode == 0) {
    // first time initialization
    DWORD mode = ENABLE_PROCESSED_OUTPUT | ENABLE_WRAP_AT_EOL_OUTPUT | ENABLE_LVB_GRID_WORLDWIDE;   // for \r \n and \b    
    // use escape sequence handling if available and the terminal supports it (so we can use rgb colors in Windows terminal)
    // Unfortunately, in plain powershell, we can successfully enable terminal processing
    // but it still fails to render correctly; so we require the palette be large enough (like in Windows Terminal)
    if (term->palette >= ANSI256 && SetConsoleMode(term->hcon, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
      term->hcon_mode = mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
      debug_msg("term: console mode: virtual terminal processing enabled\n");
    }
    // no virtual terminal processing, emulate instead
    else if (SetConsoleMode(term->hcon, mode)) {
      term->hcon_mode = mode;
      term->palette = ANSI16;
    }
    GetConsoleMode(term->hcon, &mode);
    debug_msg("term: console mode: orig: 0x%x, new: 0x%x, current 0x%x\n", term->hcon_orig_mode, term->hcon_mode, mode);
  }
  else {
    SetConsoleMode(term->hcon, term->hcon_mode);
  }  
}

ic_private void term_end_raw(term_t* term, bool force) {
  if (term->raw_enabled <= 0) return;
  if (!force && term->raw_enabled > 1) {
    term->raw_enabled--;
  }
  else {
    term->raw_enabled = 0;
    SetConsoleMode(term->hcon, term->hcon_orig_mode);
    SetConsoleOutputCP(term->hcon_orig_cp);
    SetConsoleTextAttribute(term->hcon, term->hcon_orig_attr);
  }
}

static void term_init_raw(term_t* term) {
  term->hcon = GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleMode(term->hcon, &term->hcon_orig_mode);
  CONSOLE_SCREEN_BUFFER_INFOEX info;
  memset(&info, 0, sizeof(info));
  info.cbSize = sizeof(info);
  if (GetConsoleScreenBufferInfoEx(term->hcon, &info)) {
    // store default attributes
    term->hcon_default_attr = info.wAttributes;
    // update our color table with the actual colors used.    
    for (unsigned i = 0; i < 16; i++) {
      COLORREF cr = info.ColorTable[i];
      uint32_t color = (ic_cap8(GetRValue(cr))<<16) | (ic_cap8(GetGValue(cr))<<8) | ic_cap8(GetBValue(cr)); // COLORREF = BGR
      // index is also in reverse in the bits 0 and 2 
      unsigned j = (i&0x08) | ((i&0x04)>>2) | (i&0x02) | (i&0x01)<<2;
      debug_msg("term: ansi color %d is 0x%06x\n", j, color);
      ansi256[j] = color;
    }    
  }
  else {
    DWORD err = GetLastError();
    debug_msg("term: cannot get console screen buffer: %d %x", err, err);
  }
  term_start_raw(term); // initialize the hcon_mode
  term_end_raw(term,false);
}

#endif
// # include "tty_esc.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
// skipping dup #include "tty.h"

/*-------------------------------------------------------------
Decoding escape sequences to key codes.
This is a bit tricky there are many variants to encode keys as escape sequences, see for example:
- <http://www.leonerd.org.uk/hacks/fixterms/>.
- <https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences>
- <https://www.xfree86.org/current/ctlseqs.html>
- <https://vt100.net/docs/vt220-rm/contents.html>
- <https://www.ecma-international.org/wp-content/uploads/ECMA-48_5th_edition_june_1991.pdf>

Generally, for our purposes we accept a subset of escape sequences as:

  escseq ::= ESC 
          |  ESC char
          |  ESC start special? (number (';' modifiers)?)? final

where:
  char         ::= [\x00-\xFF]               # any character
  special      ::= [:<=>?]             
  number       ::= [0-9+]
  modifiers    ::= [1-9]      
  intermediate ::= [\x20-\x2F]               # !"#$%&'()*+,-./
  final        ::= [\x40-\x7F]               # @A–Z[\]^_`a–z{|}~
  ESC          ::= '\x1B'
  CSI          ::= ESC '['
  SS3          ::= ESC 'O'

In ECMA48 `special? (number (';' modifiers)?)?` is the more liberal `[\x30-\x3F]*` 
but that seems never used for key codes. If the number (vtcode or unicode) or the 
modifiers are not given, we assume these are '1'. 
We then accept the following key sequences:

  key ::= ESC                                              # lone ESC
       |  ESC char                                         # Alt+char
       |  ESC '[' special? vtcode  ';' modifiers '~'       # vt100 codes
       |  ESC '[' special? '1'     ';' modifiers [A-Z]     # xterm codes
       |  ESC 'O' special? '1'     ';' modifiers [A-Za-z]  # SS3 codes
       |  ESC '[' special? unicode ';' modifiers 'u'       # direct unicode code

Moreover, we translate the following special cases that do not fit into the above grammar.
First we translate away special starter sequences:
---------------------------------------------------------------------
  ESC '[' '[' ..      ~>  ESC '[' ..                  # Linux sometimes uses extra '[' for CSI
  ESC '[' 'O' ..      ~>  ESC 'O' ..                  # Linux sometimes uses extra '[' for SS3
  ESC 'o' ..          ~>  ESC 'O' ..                  # Eterm: ctrl + SS3
  ESC '?' ..          ~>  ESC 'O' ..                  # vt52 treated as SS3

And then translate the following special cases into a standard form:
---------------------------------------------------------------------
  ESC '[' .. '@'      ~>  ESC '[' '3' '~'             # Del on Mach
  ESC '[' .. '9'      ~>  ESC '[' '2' '~'             # Ins on Mach
  ESC .. [^@$]        ~>  ESC .. '~'                  # ETerm,xrvt,urxt: ^ = ctrl, $ = shift, @ = alt
  ESC '[' [a-d]       ~>  ESC '[' '1' ';' '2' [A-D]   # Eterm shift+<cursor>
  ESC 'O' [1-9] final ~>  ESC 'O' '1' ';' [1-9] final # modifiers as parameter 1 (like on Haiku)
  ESC '[' [1-9] [^~u] ~>  ESC 'O' '1' ';' [1-9] final # modifiers as parameter 1

The modifier keys are encoded as "(modifiers-1) & mask" where the 
shift mask is 0x01, alt 0x02 and ctrl 0x04. Therefore:
------------------------------------------------------------
  1:  -           5: ctrl            9: alt  (for minicom)
  2:  shift       6: shift+ctrl
  3:  alt         7: alt+ctrl
  4:  shift+alt   8: shift+alt+ctrl

The different encodings fox vt100, xterm, and SS3 are:

vt100:  ESC [ vtcode ';' modifiers '~'
--------------------------------------
  1:  Home       10-15: F1-F5
  2:  Ins        16   : F5
  3:  Del        17-21: F6-F10
  4:  End        23-26: F11-F14
  5:  PageUp     28   : F15
  6:  PageDn     29   : F16
  7:  Home       31-34: F17-F20
  8:  End

xterm: ESC [ 1 ';' modifiers [A-Z]
-----------------------------------
  A: Up          N: F2        
  B: Down        O: F3       
  C: Right       P: F4       
  D: Left        Q: F5       
  E: '5'         R: F6       
  F: End         S: F7       
  G:             T: F8       
  H: Home        U: PageDn       
  I: PageUp      V: PageUp       
  J:             W: F11      
  K:             X: F12      
  L: Ins         Y: End      
  M: F1          Z: shift+Tab

SS3:   ESC 'O' 1 ';' modifiers [A-Za-z]
---------------------------------------
  (normal)                       (numpad)
  A: Up          N:              a: Up        n:           
  B: Down        O:              b: Down      o: 
  C: Right       P: F1           c: Right     p: Ins  
  D: Left        Q: F2           d: Left      q: End  
  E: '5'         R: F3           e:           r: Down 
  F: End         S: F4           f:           s: PageDn
  G:             T: F5           g:           t: Left 
  H: Home        U: F6           h:           u: '5'
  I: Tab         V: F7           i:           v: Right
  J:             W: F8           j: '*'       w: Home 
  K:             X: F9           k: '+'       x: Up 
  L:             Y: F10          l: ','       y: PageUp 
  M: \x0A '\n'   Z: shift+Tab    m: '-'       z:  
    
-------------------------------------------------------------*/

//-------------------------------------------------------------
// Decode escape sequences
//-------------------------------------------------------------

static code_t esc_decode_vt(uint32_t vt_code ) {
  switch(vt_code) {
    case 1: return KEY_HOME; 
    case 2: return KEY_INS;
    case 3: return KEY_DEL;
    case 4: return KEY_END;          
    case 5: return KEY_PAGEUP;
    case 6: return KEY_PAGEDOWN;
    case 7: return KEY_HOME;
    case 8: return KEY_END;          
    default: 
      if (vt_code >= 10 && vt_code <= 15) return KEY_F(1  + (vt_code - 10));
      if (vt_code == 16) return KEY_F5; // minicom
      if (vt_code >= 17 && vt_code <= 21) return KEY_F(6  + (vt_code - 17));
      if (vt_code >= 23 && vt_code <= 26) return KEY_F(11 + (vt_code - 23));
      if (vt_code >= 28 && vt_code <= 29) return KEY_F(15 + (vt_code - 28));
      if (vt_code >= 31 && vt_code <= 34) return KEY_F(17 + (vt_code - 31));
  }
  return KEY_NONE;
}

static code_t esc_decode_xterm( uint8_t xcode ) {
  // ESC [
  switch(xcode) {
    case 'A': return KEY_UP;
    case 'B': return KEY_DOWN;
    case 'C': return KEY_RIGHT;
    case 'D': return KEY_LEFT;
    case 'E': return '5';          // numpad 5
    case 'F': return KEY_END;
    case 'H': return KEY_HOME;
    case 'Z': return KEY_TAB | KEY_MOD_SHIFT;
    // Freebsd:
    case 'I': return KEY_PAGEUP;  
    case 'L': return KEY_INS;   
    case 'M': return KEY_F1;
    case 'N': return KEY_F2;
    case 'O': return KEY_F3;
    case 'P': return KEY_F4;       // note: differs from <https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences>
    case 'Q': return KEY_F5;
    case 'R': return KEY_F6;
    case 'S': return KEY_F7;
    case 'T': return KEY_F8;
    case 'U': return KEY_PAGEDOWN; // Mach
    case 'V': return KEY_PAGEUP;   // Mach
    case 'W': return KEY_F11;
    case 'X': return KEY_F12;    
    case 'Y': return KEY_END;      // Mach       
  }
  return KEY_NONE;
}

static code_t esc_decode_ss3( uint8_t ss3_code ) {
  // ESC O 
  switch(ss3_code) {
    case 'A': return KEY_UP;
    case 'B': return KEY_DOWN;
    case 'C': return KEY_RIGHT;
    case 'D': return KEY_LEFT;
    case 'E': return '5';           // numpad 5
    case 'F': return KEY_END;
    case 'H': return KEY_HOME;
    case 'I': return KEY_TAB;
    case 'Z': return KEY_TAB | KEY_MOD_SHIFT;
    case 'M': return KEY_LINEFEED; 
    case 'P': return KEY_F1;
    case 'Q': return KEY_F2;
    case 'R': return KEY_F3;
    case 'S': return KEY_F4;
    // on Mach
    case 'T': return KEY_F5;
    case 'U': return KEY_F6;
    case 'V': return KEY_F7;
    case 'W': return KEY_F8;
    case 'X': return KEY_F9;  // '=' on vt220
    case 'Y': return KEY_F10;
    // numpad
    case 'a': return KEY_UP;
    case 'b': return KEY_DOWN;
    case 'c': return KEY_RIGHT;
    case 'd': return KEY_LEFT;
    case 'j': return '*';
    case 'k': return '+';
    case 'l': return ',';
    case 'm': return '-'; 
    case 'n': return KEY_DEL; // '.'
    case 'o': return '/'; 
    case 'p': return KEY_INS;
    case 'q': return KEY_END;  
    case 'r': return KEY_DOWN; 
    case 's': return KEY_PAGEDOWN; 
    case 't': return KEY_LEFT; 
    case 'u': return '5';
    case 'v': return KEY_RIGHT;
    case 'w': return KEY_HOME;  
    case 'x': return KEY_UP; 
    case 'y': return KEY_PAGEUP;   
  }
  return KEY_NONE;
}

static void tty_read_csi_num(tty_t* tty, uint8_t* ppeek, uint32_t* num, long esc_timeout) {
  *num = 1; // default
  ssize_t count = 0;
  uint32_t i = 0;
  while (*ppeek >= '0' && *ppeek <= '9' && count < 16) {    
    uint8_t digit = *ppeek - '0';
    if (!tty_readc_noblock(tty,ppeek,esc_timeout)) break;  // peek is not modified in this case 
    count++;
    i = 10*i + digit; 
  }
  if (count > 0) *num = i;
}

static code_t tty_read_csi(tty_t* tty, uint8_t c1, uint8_t peek, code_t mods0, long esc_timeout) {
  // CSI starts with 0x9b (c1=='[') | ESC [ (c1=='[') | ESC [Oo?] (c1 == 'O')  /* = SS3 */
  
  // check for extra starter '[' (Linux sends ESC [ [ 15 ~  for F5 for example)
  if (c1 == '[' && strchr("[Oo", (char)peek) != NULL) {
    uint8_t cx = peek;
    if (tty_readc_noblock(tty,&peek,esc_timeout)) {
      c1 = cx;
    }
  }

  // "special" characters ('?' is used for private sequences)
  uint8_t special = 0;
  if (strchr(":<=>?",(char)peek) != NULL) { 
    special = peek;
    if (!tty_readc_noblock(tty,&peek,esc_timeout)) {  
      tty_cpush_char(tty,special); // recover
      return (key_unicode(c1) | KEY_MOD_ALT);       // Alt+<anychar>
    }
  }

  // up to 2 parameters that default to 1
  uint32_t num1 = 1;
  uint32_t num2 = 1;
  tty_read_csi_num(tty,&peek,&num1,esc_timeout);
  if (peek == ';') {
    if (!tty_readc_noblock(tty,&peek,esc_timeout)) return KEY_NONE;
    tty_read_csi_num(tty,&peek,&num2,esc_timeout);
  }

  // the final character (we do not allow 'intermediate characters')
  uint8_t final = peek;
  code_t  modifiers = mods0;

  debug_msg("tty: escape sequence: ESC %c %c %d;%d %c\n", c1, (special == 0 ? '_' : special), num1, num2, final);
  
  // Adjust special cases into standard ones.
  if ((final == '@' || final == '9') && c1 == '[' && num1 == 1) {
    // ESC [ @, ESC [ 9  : on Mach
    if (final == '@')      num1 = 3; // DEL
    else if (final == '9') num1 = 2; // INS 
    final = '~';
  }
  else if (final == '^' || final == '$' || final == '@') {  
    // Eterm/rxvt/urxt  
    if (final=='^') modifiers |= KEY_MOD_CTRL;
    if (final=='$') modifiers |= KEY_MOD_SHIFT;
    if (final=='@') modifiers |= KEY_MOD_SHIFT | KEY_MOD_CTRL;
    final = '~';
  }
  else if (c1 == '[' && final >= 'a' && final <= 'd') {  // note: do not catch ESC [ .. u  (for unicode)
    // ESC [ [a-d]  : on Eterm for shift+ cursor
    modifiers |= KEY_MOD_SHIFT;
    final = 'A' + (final - 'a');
  }
  
  if (((c1 == 'O') || (c1=='[' && final != '~' && final != 'u')) &&
      (num2 == 1 && num1 > 1 && num1 <= 8)) 
  {
    // on haiku the modifier can be parameter 1, make it parameter 2 instead
    num2 = num1;
    num1 = 1;
  }

  // parameter 2 determines the modifiers
  if (num2 > 1 && num2 <= 9) {
    if (num2 == 9) num2 = 3; // iTerm2 in xterm mode
    num2--;
    if (num2 & 0x1) modifiers |= KEY_MOD_SHIFT;
    if (num2 & 0x2) modifiers |= KEY_MOD_ALT;
    if (num2 & 0x4) modifiers |= KEY_MOD_CTRL;
  }

  // and translate
  code_t code = KEY_NONE;
  if (final == '~') {
    // vt codes
    code = esc_decode_vt(num1);
  }
  else if (c1 == '[' && final == 'u') {
    // unicode
    code = key_unicode(num1);
  }
  else if (c1 == 'O' && ((final >= 'A' && final <= 'Z') || (final >= 'a' && final <= 'z'))) {
    // ss3
    code = esc_decode_ss3(final);
  }
  else if (num1 == 1 && final >= 'A' && final <= 'Z') {
    // xterm 
    code = esc_decode_xterm(final);
  }
  else if (c1 == '[' && final == 'R') {
    // cursor position
    code = KEY_NONE;
  }  

  if (code == KEY_NONE && final != 'R') { 
    debug_msg("tty: ignore escape sequence: ESC %c %zu;%zu %c\n", c1, num1, num2, final); 
  }
  return (code != KEY_NONE ? (code | modifiers) : KEY_NONE);
}

static code_t tty_read_osc( tty_t* tty, uint8_t* ppeek, long esc_timeout ) {
  debug_msg("discard OSC response..\n");
  // keep reading until termination: OSC is terminated by BELL, or ESC \ (ST)  (and STX)
  while (true) {
    uint8_t c = *ppeek;
    if (c <= '\x07') {  // BELL and anything below (STX, ^C, ^D)
      if (c != '\x07') { tty_cpush_char( tty, c ); }
      break;
    }
    else if (c=='\x1B') {
      uint8_t c1;
      if (!tty_readc_noblock(tty, &c1, esc_timeout)) break;
      if (c1=='\\') break;
      tty_cpush_char(tty,c1);
    }
    if (!tty_readc_noblock(tty, ppeek, esc_timeout)) break;
  }
  return KEY_NONE;
}

ic_private code_t tty_read_esc(tty_t* tty, long esc_initial_timeout, long esc_timeout) {
  code_t  mods = 0;
  uint8_t peek = 0;
  
  // lone ESC?
  if (!tty_readc_noblock(tty, &peek, esc_initial_timeout)) return KEY_ESC;

  // treat ESC ESC as Alt modifier (macOS sends ESC ESC [ [A-D] for alt-<cursor>)
  if (peek == KEY_ESC) {
    if (!tty_readc_noblock(tty, &peek, esc_timeout)) goto alt;
    mods |= KEY_MOD_ALT;
  }

  // CSI ?
  if (peek == '[') {
    if (!tty_readc_noblock(tty, &peek, esc_timeout)) goto alt;
    return tty_read_csi(tty, '[', peek, mods, esc_timeout);  // ESC [ ...
  }

  // SS3?
  if (peek == 'O' || peek == 'o' || peek == '?' /*vt52*/) {
    uint8_t c1 = peek;
    if (!tty_readc_noblock(tty, &peek, esc_timeout)) goto alt;
    if (c1 == 'o') { 
      // ETerm uses this for ctrl+<cursor>
      mods |= KEY_MOD_CTRL;
    }
    // treat all as standard SS3 'O'
    return tty_read_csi(tty,'O',peek,mods, esc_timeout);  // ESC [Oo?] ...
  }

  // OSC: we may get a delayed query response; ensure it is ignored
  if (peek == ']') {
    if (!tty_readc_noblock(tty, &peek, esc_timeout)) goto alt;
    return tty_read_osc(tty, &peek, esc_timeout);  // ESC ] ...
  }

alt:  
  // Alt+<char>
  return (key_unicode(peek) | KEY_MOD_ALT);  // ESC <anychar>
}
// # include "tty.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <locale.h>

// skipping dup #include "tty.h"

#if defined(_WIN32)
#include <windows.h>
#include <io.h>
#define isatty(fd)     _isatty(fd)
#define read(fd,s,n)   _read(fd,s,n)
#define STDIN_FILENO   0
#if (_WIN32_WINNT < 0x0600)
WINBASEAPI ULONGLONG WINAPI GetTickCount64(VOID);
#endif
#else
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#if !defined(FIONREAD)
#include <fcntl.h>
#endif
#endif

#define TTY_PUSH_MAX (32)

struct tty_s {
  int       fd_in;                  // input handle
  bool      raw_enabled;            // is raw mode enabled?
  bool      is_utf8;                // is the input stream in utf-8 mode?
  bool      has_term_resize_event;  // are resize events generated?
  bool      term_resize_event;      // did a term resize happen?
  alloc_t*  mem;                    // memory allocator
  code_t    pushbuf[TTY_PUSH_MAX];  // push back buffer for full key codes  
  ssize_t   push_count;               
  uint8_t   cpushbuf[TTY_PUSH_MAX]; // low level push back buffer for bytes
  ssize_t   cpush_count;
  long      esc_initial_timeout;    // initial ms wait to see if ESC starts an escape sequence
  long      esc_timeout;            // follow up delay for characters in an escape sequence
  #if defined(_WIN32)               
  HANDLE    hcon;                   // console input handle
  DWORD     hcon_orig_mode;         // original console mode
  #else
  struct termios  orig_ios;         // original terminal settings
  struct termios  raw_ios;          // raw terminal settings
  #endif
};


//-------------------------------------------------------------
// Forward declarations of platform dependent primitives below
//-------------------------------------------------------------

ic_private bool tty_readc_noblock(tty_t* tty, uint8_t* c, long timeout_ms);  // does not modify `c` when no input (false is returned)

//-------------------------------------------------------------
// Key code helpers
//-------------------------------------------------------------

ic_private bool code_is_ascii_char(code_t c, char* chr ) {
  if (c >= ' ' && c <= 0x7F) {
    if (chr != NULL) *chr = (char)c;
    return true;
  }
  else {
    if (chr != NULL) *chr = 0;
    return false;
  }
}

ic_private bool code_is_unicode(code_t c, unicode_t* uchr) {
  if (c <= KEY_UNICODE_MAX) {
    if (uchr != NULL) *uchr = c;
    return true;
  }
  else {
    if (uchr != NULL) *uchr = 0;
    return false;
  }
}

ic_private bool code_is_virt_key(code_t c ) {
  return (KEY_NO_MODS(c) <= 0x20 || KEY_NO_MODS(c) >= KEY_VIRT);
}


//-------------------------------------------------------------
// Read a key code
//-------------------------------------------------------------
static code_t modify_code( code_t code );

static code_t tty_read_utf8( tty_t* tty, uint8_t c0 ) {
  uint8_t buf[5];
  memset(buf, 0, 5);

  // try to read as many bytes as potentially needed
  buf[0] = c0;
  ssize_t count = 1;
  if (c0 > 0x7F) {
    if (tty_readc_noblock(tty, buf+count, tty->esc_timeout)) {
      count++;
      if (c0 > 0xDF) {
        if (tty_readc_noblock(tty, buf+count, tty->esc_timeout)) {
          count++;
          if (c0 > 0xEF) {
            if (tty_readc_noblock(tty, buf+count, tty->esc_timeout)) {
              count++;
            }
          }
        }
      }
    }
  }
  
  buf[count] = 0;
  debug_msg("tty: read utf8: count: %zd: %02x,%02x,%02x,%02x", count, buf[0], buf[1], buf[2], buf[3]);

  // decode the utf8 to unicode
  ssize_t read = 0;
  code_t code = key_unicode(unicode_from_qutf8(buf, count, &read));

  // push back unused bytes (in the case of invalid utf8)
  while (count > read) {
    count--;
    if (count >= 0 && count <= 4) {  // to help the static analyzer
      tty_cpush_char(tty, buf[count]);
    }
  }
  return code;
}

// pop a code from the pushback buffer.
static bool tty_code_pop(tty_t* tty, code_t* code);


// read a single char/key 
ic_private bool tty_read_timeout(tty_t* tty, long timeout_ms, code_t* code) 
{
  // is there a push_count back code?
  if (tty_code_pop(tty,code)) {
    return code;
  }

  // read a single char/byte from a character stream
  uint8_t c;
  if (!tty_readc_noblock(tty, &c, timeout_ms)) return false;
  
  if (c == KEY_ESC) {
    // escape sequence?
    *code = tty_read_esc(tty, tty->esc_initial_timeout, tty->esc_timeout);
  }
  else if (c <= 0x7F) {
    // ascii
    *code = key_unicode(c);
  }
  else if (tty->is_utf8) {
    // utf8 sequence
    *code = tty_read_utf8(tty,c);
  }
  else {
    // c >= 0x80 but tty is not utf8; use raw plane so we can translate it back in the end
    *code = key_unicode( unicode_from_raw(c) );
  }

  *code = modify_code(*code);
  return true;
}

// Transform virtual keys to be more portable across platforms
static code_t modify_code( code_t code ) {
  code_t key  = KEY_NO_MODS(code);
  code_t mods = KEY_MODS(code);
  debug_msg( "tty: readc %s%s%s 0x%03x ('%c')\n", 
              mods&KEY_MOD_SHIFT ? "shift+" : "",  mods&KEY_MOD_CTRL  ? "ctrl+" : "", mods&KEY_MOD_ALT   ? "alt+" : "",
              key, (key >= ' ' && key <= '~' ? key : ' '));

  // treat KEY_RUBOUT (0x7F) as KEY_BACKSP
  if (key == KEY_RUBOUT) {
    code = KEY_BACKSP | mods;
  }
  // ctrl+'_' is translated to '\x1F' on Linux, translate it back 
  else if (key == key_char('\x1F') && (mods & KEY_MOD_ALT) == 0) {
    key = '_';
    code = WITH_CTRL(key_char('_'));
  }
  // treat ctrl/shift + enter always as KEY_LINEFEED for portability
  else if (key == KEY_ENTER && (mods == KEY_MOD_SHIFT || mods == KEY_MOD_ALT || mods == KEY_MOD_CTRL)) {
    code = KEY_LINEFEED;
  }
  // treat ctrl+tab always as shift+tab for portability
  else if (code == WITH_CTRL(KEY_TAB)) {
    code = KEY_SHIFT_TAB;
  }
  // treat ctrl+end/alt+>/alt-down and ctrl+home/alt+</alt-up always as pagedown/pageup for portability
  else if (code == WITH_ALT(KEY_DOWN) || code == WITH_ALT('>') || code == WITH_CTRL(KEY_END)) {
    code = KEY_PAGEDOWN;
  }
  else if (code == WITH_ALT(KEY_UP) || code == WITH_ALT('<') || code == WITH_CTRL(KEY_HOME)) {
    code = KEY_PAGEUP;
  }
  
  // treat C0 codes without KEY_MOD_CTRL
  if (key < ' ' && (mods&KEY_MOD_CTRL) != 0) {
    code &= ~KEY_MOD_CTRL; 
  }
  
  return code;
}


// read a single char/key 
ic_private code_t tty_read(tty_t* tty)
{
  code_t code;
  if (!tty_read_timeout(tty, -1, &code)) return KEY_NONE;
  return code;
}

//-------------------------------------------------------------
// Read back an ANSI query response
//-------------------------------------------------------------

ic_private bool tty_read_esc_response(tty_t* tty, char esc_start, bool final_st, char* buf, ssize_t buflen ) 
{
  buf[0] = 0;
  ssize_t len = 0;
  uint8_t c = 0;
  if (!tty_readc_noblock(tty, &c, 2*tty->esc_initial_timeout) || c != '\x1B') {
    debug_msg("initial esc response failed: 0x%02x\n", c);
    return false;
  }
  if (!tty_readc_noblock(tty, &c, tty->esc_timeout) || (c != esc_start)) return false;
  while( len < buflen ) {
    if (!tty_readc_noblock(tty, &c, tty->esc_timeout)) return false;
    if (final_st) {
      // OSC is terminated by BELL, or ESC \ (ST)  (and STX)
      if (c=='\x07' || c=='\x02') {
        break;
      }
      else if (c=='\x1B') {
        uint8_t c1;
        if (!tty_readc_noblock(tty, &c1, tty->esc_timeout)) return false;
        if (c1=='\\') break;
        tty_cpush_char(tty,c1);
      }
    }
    else {
      if (c == '\x02') { // STX
        break;
      }
      else if (!((c >= '0' && c <= '9') || strchr("<=>?;:",c) != NULL)) {
        buf[len++] = (char)c; // for non-OSC save the terminating character
        break;
      }
    }
    buf[len++] = (char)c; 
  }
  buf[len] = 0;
  debug_msg("tty: escape query response: %s\n", buf);
  return true;
}

//-------------------------------------------------------------
// High level code pushback
//-------------------------------------------------------------

static bool tty_code_pop( tty_t* tty, code_t* code ) {
  if (tty->push_count <= 0) return false;
  tty->push_count--;
  *code = tty->pushbuf[tty->push_count];
  return true;
}

ic_private void tty_code_pushback( tty_t* tty, code_t c ) {   
  // note: must be signal safe
  if (tty->push_count >= TTY_PUSH_MAX) return;
  tty->pushbuf[tty->push_count] = c;
  tty->push_count++;
}


//-------------------------------------------------------------
// low-level character pushback (for escape sequences and windows)
//-------------------------------------------------------------

ic_private bool tty_cpop(tty_t* tty, uint8_t* c) {  
  if (tty->cpush_count <= 0) {  // do not modify c on failure (see `tty_decode_unicode`)
    return false;
  }
  else {
    tty->cpush_count--;
    *c = tty->cpushbuf[tty->cpush_count];
    return true;
  }
}

static void tty_cpush(tty_t* tty, const char* s) {
  ssize_t len = ic_strlen(s);
  if (tty->push_count + len > TTY_PUSH_MAX) {
    debug_msg("tty: cpush buffer full! (pushing %s)\n", s);
    assert(false);
    return;
  }
  for (ssize_t i = 0; i < len; i++) {
    tty->cpushbuf[tty->cpush_count + i] = (uint8_t)( s[len - i - 1] );
  }
  tty->cpush_count += len;
  return;
}

// convenience function for small sequences
static void tty_cpushf(tty_t* tty, const char* fmt, ...) {
  va_list args;
  va_start(args,fmt);
  char buf[TTY_PUSH_MAX+1];
  vsnprintf(buf,TTY_PUSH_MAX,fmt,args);
  buf[TTY_PUSH_MAX] = 0;
  tty_cpush(tty,buf);
  va_end(args);
  return;
}

ic_private void tty_cpush_char(tty_t* tty, uint8_t c) {  
  uint8_t buf[2];
  buf[0] = c;
  buf[1] = 0;
  tty_cpush(tty, (const char*)buf);
}


//-------------------------------------------------------------
// Push escape codes (used on Windows to insert keys)
//-------------------------------------------------------------

static unsigned csi_mods(code_t mods) {
  unsigned m = 1;
  if (mods&KEY_MOD_SHIFT) m += 1;
  if (mods&KEY_MOD_ALT)   m += 2;
  if (mods&KEY_MOD_CTRL)  m += 4;
  return m;
}

// Push ESC [ <vtcode> ; <mods> ~
static void tty_cpush_csi_vt( tty_t* tty, code_t mods, uint32_t vtcode ) {
  tty_cpushf(tty,"\x1B[%u;%u~", vtcode, csi_mods(mods) );
}

// push ESC [ 1 ; <mods> <xcmd>
static void tty_cpush_csi_xterm( tty_t* tty, code_t mods, char xcode ) {
  tty_cpushf(tty,"\x1B[1;%u%c", csi_mods(mods), xcode );
}

// push ESC [ <unicode> ; <mods> u
static void tty_cpush_csi_unicode( tty_t* tty, code_t mods, uint32_t unicode ) {
  if ((unicode < 0x80 && mods == 0) || 
      (mods == KEY_MOD_CTRL && unicode < ' ' && unicode != KEY_TAB && unicode != KEY_ENTER 
                        && unicode != KEY_LINEFEED && unicode != KEY_BACKSP) ||
      (mods == KEY_MOD_SHIFT && unicode >= ' ' && unicode <= KEY_RUBOUT)) {
    tty_cpush_char(tty,(uint8_t)unicode);
  }
  else {
    tty_cpushf(tty,"\x1B[%u;%uu", unicode, csi_mods(mods) );
  }
}

//-------------------------------------------------------------
// Init
//-------------------------------------------------------------

static bool tty_init_raw(tty_t* tty);
static void tty_done_raw(tty_t* tty);

static bool tty_init_utf8(tty_t* tty) {
  #ifdef _WIN32
  tty->is_utf8 = true;
  #else
  const char* loc = setlocale(LC_ALL,"");
  tty->is_utf8 = (ic_icontains(loc,"UTF-8") || ic_icontains(loc,"utf8") || ic_stricmp(loc,"C") == 0);
  debug_msg("tty: utf8: %s (loc=%s)\n", tty->is_utf8 ? "true" : "false", loc);
  #endif
  return true;
}

ic_private tty_t* tty_new(alloc_t* mem, int fd_in) 
{
  tty_t* tty = mem_zalloc_tp(mem, tty_t);
  tty->mem = mem;
  tty->fd_in = (fd_in < 0 ? STDIN_FILENO : fd_in);
  #if defined(__APPLE__)
  tty->esc_initial_timeout = 200;  // apple use ESC+<key> for alt-<key>
  #else
  tty->esc_initial_timeout = 100; 
  #endif
  tty->esc_timeout = 10;
  if (!(isatty(tty->fd_in) && tty_init_raw(tty) && tty_init_utf8(tty))) {
    tty_free(tty);
    return NULL;
  }
  return tty;
}

ic_private void tty_free(tty_t* tty) {
  if (tty==NULL) return;
  tty_end_raw(tty);
  tty_done_raw(tty);
  mem_free(tty->mem,tty);
}

ic_private bool tty_is_utf8(const tty_t* tty) {
  if (tty == NULL) return true;
  return (tty->is_utf8);
}

ic_private bool tty_term_resize_event(tty_t* tty) {
  if (tty == NULL) return true;
  if (tty->has_term_resize_event) {
    if (!tty->term_resize_event) return false;
    tty->term_resize_event = false;  // reset.   
  }
  return true;  // always return true on systems without a resize event (more expensive but still ok)
}

ic_private void tty_set_esc_delay(tty_t* tty, long initial_delay_ms, long followup_delay_ms) {
  tty->esc_initial_timeout = (initial_delay_ms < 0 ? 0 : (initial_delay_ms > 1000 ? 1000 : initial_delay_ms));
  tty->esc_timeout = (followup_delay_ms < 0 ? 0 : (followup_delay_ms > 1000 ? 1000 : followup_delay_ms));
}

//-------------------------------------------------------------
// Unix
//-------------------------------------------------------------
#if !defined(_WIN32)

static bool tty_readc_blocking(tty_t* tty, uint8_t* c) {
  if (tty_cpop(tty,c)) return true;
  *c = 0;
  ssize_t nread = read(tty->fd_in, (char*)c, 1);
  if (nread < 0 && errno == EINTR) {
    // can happen on SIGWINCH signal for terminal resize
  }
  return (nread == 1);
}


// non blocking read -- with a small timeout used for reading escape sequences.
ic_private bool tty_readc_noblock(tty_t* tty, uint8_t* c, long timeout_ms) 
{
  // in our pushback buffer?
  if (tty_cpop(tty, c)) return true;

  // blocking read?
  if (timeout_ms < 0) {
    return tty_readc_blocking(tty,c);
  }

  // if supported, peek first if any char is available.
  #if defined(FIONREAD)
  { int navail = 0;
    if (ioctl(0, FIONREAD, &navail) == 0) {
      if (navail >= 1) {
        return tty_readc_blocking(tty, c);
      }
      else if (timeout_ms == 0) {
        return false;  // return early if there is no input available (with a zero timeout)
      }
    }
  }
  #endif

  // otherwise block for at most timeout milliseconds
  #if defined(FD_SET)   
    // we can use select to detect when input becomes available
    fd_set readset;
    struct timeval time;
    FD_ZERO(&readset);
    FD_SET(tty->fd_in, &readset);
    time.tv_sec  = (timeout_ms > 0 ? timeout_ms / 1000 : 0);
    time.tv_usec = (timeout_ms > 0 ? 1000*(timeout_ms % 1000) : 0);      
    if (select(tty->fd_in + 1, &readset, NULL, NULL, &time) == 1) {
      // input available
      return tty_readc_blocking(tty, c);
    }    
  #else
    // no select, we cannot timeout; use usleeps :-(
    // todo: this seems very rare nowadays; should be even support this?
    do {
      // peek ahead if possible
      #if defined(FIONREAD)
      int navail = 0;
      if (ioctl(0, FIONREAD, &navail) == 0 && navail >= 1) {
        return tty_readc_blocking(tty, c);
      }
      #elif defined(O_NONBLOCK)
      // use a temporary non-blocking read mode
      int fstatus = fcntl(tty->fd_in, F_GETFL, 0);
      if (fstatus != -1) {
        if (fcntl(tty->fd_in, F_SETFL, (fstatus | O_NONBLOCK)) != -1) {
          char buf[2] = { 0, 0 };
          ssize_t nread = read(tty->fd_in, buf, 1);
          fcntl(tty->fd_in, F_SETFL, fstatus);
          if (nread >= 1) {
            *c = (uint8_t)buf[0];
            return true;
          }
        }
      }
      #else
      #error "define an nonblocking read for this platform"
      #endif
      // and sleep a bit
      if (timeout_ms > 0) {
        usleep(50*1000L); // sleep at most 0.05s at a time
        timeout_ms -= 100;
        if (timeout_ms < 0) { timeout_ms = 0; }
      }      
    } 
    while (timeout_ms > 0);
  #endif  
  return false;
}

#if defined(TIOCSTI) 
ic_private bool tty_async_stop(const tty_t* tty) {
  // insert ^C in the input stream
  char c = KEY_CTRL_C;
  return (ioctl(tty->fd_in, TIOCSTI, &c) >= 0);
}
#else
ic_private bool tty_async_stop(const tty_t* tty) {
  return false;
}
#endif

// We install various signal handlers to restore the terminal settings
// in case of a terminating signal. This is also used to catch terminal window resizes.
// This is not strictly needed so this can be disabled on 
// (older) platforms that do not support signal handling well.
#if defined(SIGWINCH) && defined(SA_RESTART)  // ensure basic signal functionality is defined

// store the tty in a global so we access it on unexpected termination
static tty_t* sig_tty; // = NULL

// Catch all termination signals (and SIGWINCH)
typedef struct signal_handler_s {
  int signum;
  union {
    int _avoid_warning;
    struct sigaction previous;
  } action;
} signal_handler_t;

static signal_handler_t sighandlers[] = {
  { SIGWINCH, {0} },
  { SIGTERM , {0} }, 
  { SIGINT  , {0} }, 
  { SIGQUIT , {0} }, 
  { SIGHUP  , {0} },
  { SIGSEGV , {0} }, 
  { SIGTRAP , {0} }, 
  { SIGBUS  , {0} }, 
  { SIGTSTP , {0} },
  { SIGTTIN , {0} }, 
  { SIGTTOU , {0} },
  { 0       , {0} }
};

static bool sigaction_is_valid( struct sigaction* sa ) {
  return (sa->sa_sigaction != NULL && sa->sa_handler != SIG_DFL && sa->sa_handler != SIG_IGN);
}

// Generic signal handler
static void sig_handler(int signum, siginfo_t* siginfo, void* uap ) {
  if (signum == SIGWINCH) {
    if (sig_tty != NULL) {
      sig_tty->term_resize_event = true;
    }
  }
  else {
    // the rest are termination signals; restore the terminal mode. (`tcsetattr` is signal-safe)
    if (sig_tty != NULL && sig_tty->raw_enabled) {
      tcsetattr(sig_tty->fd_in, TCSAFLUSH, &sig_tty->orig_ios);
      sig_tty->raw_enabled = false;
    }
  }
  // call previous handler
  signal_handler_t* sh = sighandlers;
  while( sh->signum != 0 && sh->signum != signum) { sh++; }
  if (sh->signum == signum) {
    if (sigaction_is_valid(&sh->action.previous)) {
      (sh->action.previous.sa_sigaction)(signum, siginfo, uap);
    }
  }
}

static void signals_install(tty_t* tty) {
  sig_tty = tty;
  // generic signal handler
  struct sigaction handler;
  memset(&handler,0,sizeof(handler));
  sigemptyset(&handler.sa_mask);
  handler.sa_sigaction = &sig_handler; 
  handler.sa_flags = SA_RESTART;
  // install for all signals
  for( signal_handler_t* sh = sighandlers; sh->signum != 0; sh++ ) {
    if (sigaction( sh->signum, NULL, &sh->action.previous) == 0) {            // get previous
      if (sh->action.previous.sa_handler != SIG_IGN) {                        // if not to be ignored
        if (sigaction( sh->signum, &handler, &sh->action.previous ) < 0) {    // install our handler
          sh->action.previous.sa_sigaction = NULL;       // do not restore on error
        }
        else if (sh->signum == SIGWINCH) {
          sig_tty->has_term_resize_event = true;
        };
      }
    }    
  }
}

static void signals_restore(void) {
  // restore all signal handlers
  for( signal_handler_t* sh = sighandlers; sh->signum != 0; sh++ ) {
    if (sigaction_is_valid(&sh->action.previous)) {
      sigaction( sh->signum, &sh->action.previous, NULL );
    };
  }
  sig_tty = NULL;
}

#else
static void signals_install(tty_t* tty) {
  ic_unused(tty);
  // nothing
}
static void signals_restore(void) {
  // nothing
}

#endif

ic_private bool tty_start_raw(tty_t* tty) {
  if (tty == NULL) return false;
  if (tty->raw_enabled) return true;
  if (tcsetattr(tty->fd_in,TCSAFLUSH,&tty->raw_ios) < 0) return false;  
  tty->raw_enabled = true;
  return true;
}

ic_private void tty_end_raw(tty_t* tty) {
  if (tty == NULL) return;
  if (!tty->raw_enabled) return;
  tty->cpush_count = 0;
  if (tcsetattr(tty->fd_in,TCSAFLUSH,&tty->orig_ios) < 0) return;
  tty->raw_enabled = false;
}

static bool tty_init_raw(tty_t* tty) 
{  
  // Set input to raw mode. See <https://man7.org/linux/man-pages/man3/termios.3.html>.
  if (tcgetattr(tty->fd_in,&tty->orig_ios) == -1) return false;
  tty->raw_ios = tty->orig_ios; 
  // input: no break signal, no \r to \n, no parity check, no 8-bit to 7-bit, no flow control
  tty->raw_ios.c_iflag &= ~(unsigned long)(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  // control: allow 8-bit
  tty->raw_ios.c_cflag |= CS8;  
  // local: no echo, no line-by-line (canonical), no extended input processing, no signals for ^z,^c
  tty->raw_ios.c_lflag &= ~(unsigned long)(ECHO | ICANON | IEXTEN | ISIG);
  // 1 byte at a time, no delay
  tty->raw_ios.c_cc[VTIME] = 0;
  tty->raw_ios.c_cc[VMIN] = 1;

  // store in global so our signal handlers can restore the terminal mode
  signals_install(tty);
  
  return true;
}

static void tty_done_raw(tty_t* tty) {
  ic_unused(tty);
  signals_restore();
}


#else

//-------------------------------------------------------------
// Windows
// For best portability we push CSI escape sequences directly
// to the character stream (instead of returning key codes).
//-------------------------------------------------------------

static void tty_waitc_console(tty_t* tty, long timeout_ms);

ic_private bool tty_readc_noblock(tty_t* tty, uint8_t* c, long timeout_ms) {  // don't modify `c` if there is no input
  // in our pushback buffer?
  if (tty_cpop(tty, c)) return true;
  // any events in the input queue?
  tty_waitc_console(tty, timeout_ms);
  return tty_cpop(tty, c);
}

// Read from the console input events and push escape codes into the tty cbuffer.
static void tty_waitc_console(tty_t* tty, long timeout_ms) 
{
  //  wait for a key down event
  INPUT_RECORD inp;
	DWORD count;
  uint32_t surrogate_hi = 0;
  while (true) {
    // check if there are events if in non-blocking timeout mode
    if (timeout_ms >= 0) {
      // first peek ahead
      if (!GetNumberOfConsoleInputEvents(tty->hcon, &count)) return;  
      if (count == 0) {
        if (timeout_ms == 0) {
          // out of time
          return;
        }
        else {
          // wait for input events for at most timeout milli seconds
          ULONGLONG start_ms = GetTickCount64();
          DWORD res = WaitForSingleObject(tty->hcon, (DWORD)timeout_ms);
          switch (res) {
            case WAIT_OBJECT_0: {
              // input is available, decrease our timeout
              ULONGLONG waited_ms = (GetTickCount64() - start_ms);
              timeout_ms -= (long)waited_ms;
              if (timeout_ms < 0) {
                timeout_ms = 0;
              }
              break;
            }
            case WAIT_TIMEOUT:
            case WAIT_ABANDONED:
            case WAIT_FAILED:
            default: 
              return;
          }
        }
      }
    }

    // (blocking) Read from the input
    if (!ReadConsoleInputW(tty->hcon, &inp, 1, &count)) return;
    if (count != 1) return;

    // resize event?
    if (inp.EventType == WINDOW_BUFFER_SIZE_EVENT) {
      tty->term_resize_event = true;
      continue;
    }

    // wait for key down events 
    if (inp.EventType != KEY_EVENT) continue;

    // the modifier state
    DWORD modstate = inp.Event.KeyEvent.dwControlKeyState;
    
    // we need to handle shift up events separately
    if (!inp.Event.KeyEvent.bKeyDown && inp.Event.KeyEvent.wVirtualKeyCode == VK_SHIFT) {
      modstate &= (DWORD)~SHIFT_PRESSED;
    }

    // ignore AltGr
    DWORD altgr = LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED;
    if ((modstate & altgr) == altgr) { modstate &= ~altgr; }

    
    // get modifiers
    code_t mods = 0;
    if ((modstate & ( RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED )) != 0) mods |= KEY_MOD_CTRL;
    if ((modstate & ( RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED )) != 0)   mods |= KEY_MOD_ALT;
    if ((modstate & SHIFT_PRESSED) != 0)                              mods |= KEY_MOD_SHIFT;

    // virtual keys
    uint32_t chr = (uint32_t)inp.Event.KeyEvent.uChar.UnicodeChar;
    WORD     virt = inp.Event.KeyEvent.wVirtualKeyCode;
    debug_msg("tty: console %s: %s%s%s virt 0x%04x, chr 0x%04x ('%c')\n", inp.Event.KeyEvent.bKeyDown ? "down" : "up", mods&KEY_MOD_CTRL ? "ctrl-" : "", mods&KEY_MOD_ALT ? "alt-" : "", mods&KEY_MOD_SHIFT ? "shift-" : "", virt, chr, chr);

    // only process keydown events (except for Alt-up which is used for unicode pasting...)
    if (!inp.Event.KeyEvent.bKeyDown && virt != VK_MENU) {
			continue;
		}
    
    if (chr == 0) { 
      switch (virt) {
        case VK_UP:     tty_cpush_csi_xterm(tty, mods, 'A'); return;
        case VK_DOWN:   tty_cpush_csi_xterm(tty, mods, 'B'); return;
        case VK_RIGHT:  tty_cpush_csi_xterm(tty, mods, 'C'); return;
        case VK_LEFT:   tty_cpush_csi_xterm(tty, mods, 'D'); return;
        case VK_END:    tty_cpush_csi_xterm(tty, mods, 'F'); return; 
        case VK_HOME:   tty_cpush_csi_xterm(tty, mods, 'H'); return;
        case VK_DELETE: tty_cpush_csi_vt(tty,mods,3); return;
        case VK_PRIOR:  tty_cpush_csi_vt(tty,mods,5); return;   //page up
        case VK_NEXT:   tty_cpush_csi_vt(tty,mods,6); return;   //page down
        case VK_TAB:    tty_cpush_csi_unicode(tty,mods,9);  return; 
        case VK_RETURN: tty_cpush_csi_unicode(tty,mods,13); return;
        default: {
          uint32_t vtcode = 0;
          if (virt >= VK_F1 && virt <= VK_F5) {
            vtcode = 10 + (virt - VK_F1);
          }
          else if (virt >= VK_F6 && virt <= VK_F10) {
            vtcode = 17 + (virt - VK_F6);
          }
          else if (virt >= VK_F11 && virt <= VK_F12) {
            vtcode = 13 + (virt - VK_F11);
          }
          if (vtcode > 0) {
            tty_cpush_csi_vt(tty,mods,vtcode);
            return;
          }
        }
      }    
      // ignore other control keys (shift etc).
    }
    // high surrogate pair
    else if (chr >= 0xD800 && chr <= 0xDBFF) {
			surrogate_hi = (chr - 0xD800);			
    }
    // low surrogate pair
    else if (chr >= 0xDC00 && chr <= 0xDFFF) {
			chr = ((surrogate_hi << 10) + (chr - 0xDC00) + 0x10000);
      tty_cpush_csi_unicode(tty,mods,chr);
      surrogate_hi = 0;
      return;
		}
    // regular character
    else {
      tty_cpush_csi_unicode(tty,mods,chr);
			return;
    }
  }
}  

ic_private bool tty_async_stop(const tty_t* tty) {
  // send ^c
  INPUT_RECORD events[2];
  memset(events, 0, 2*sizeof(INPUT_RECORD));
  events[0].EventType = KEY_EVENT;
  events[0].Event.KeyEvent.bKeyDown = TRUE;
  events[0].Event.KeyEvent.uChar.AsciiChar = KEY_CTRL_C;
  events[1] = events[0];
  events[1].Event.KeyEvent.bKeyDown = FALSE;
  DWORD nwritten = 0;
  WriteConsoleInput(tty->hcon, events, 2, &nwritten);
  return (nwritten == 2);
}

ic_private bool tty_start_raw(tty_t* tty) {
  if (tty->raw_enabled) return true;
  GetConsoleMode(tty->hcon,&tty->hcon_orig_mode);
  DWORD mode = ENABLE_QUICK_EDIT_MODE   // cut&paste allowed 
             | ENABLE_WINDOW_INPUT      // to catch resize events 
             // | ENABLE_VIRTUAL_TERMINAL_INPUT 
             // | ENABLE_PROCESSED_INPUT
             ;
  SetConsoleMode(tty->hcon, mode );
  tty->raw_enabled = true; 
  return true; 
}

ic_private void tty_end_raw(tty_t* tty) {
  if (!tty->raw_enabled) return;
  SetConsoleMode(tty->hcon, tty->hcon_orig_mode );
  tty->raw_enabled = false;
}

static bool tty_init_raw(tty_t* tty) {
  tty->hcon = GetStdHandle( STD_INPUT_HANDLE );  
  tty->has_term_resize_event = true;
  return true;
}

static void tty_done_raw(tty_t* tty) {
  ic_unused(tty);
}

#endif


// # include "stringbuf.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

// get `wcwidth` for the column width of unicode characters
// note: for now the OS provided one is unused as we see quite a bit of variation 
// among platforms and including our own seems more reliable.
/* 
#if defined(__linux__) || defined(__freebsd__)
// use the system supplied one
#if !defined(_XOPEN_SOURCE)
#define  _XOPEN_SOURCE  700    // so wcwidth is visible
#endif
#include <wchar.h>
#else
*/
// use our own (also on APPLE as that fails within vscode)
#define  wcwidth(c)  mk_wcwidth(c)
// #include "wcwidth.c"
// include in "stringbuf.c"
/*
 * This is an implementation of wcwidth() and wcswidth() (defined in
 * IEEE Std 1002.1-2001) for Unicode.
 *
 * http://www.opengroup.org/onlinepubs/007904975/functions/wcwidth.html
 * http://www.opengroup.org/onlinepubs/007904975/functions/wcswidth.html
 *
 * In fixed-width output devices, Latin characters all occupy a single
 * "cell" position of equal width, whereas ideographic CJK characters
 * occupy two such cells. Interoperability between terminal-line
 * applications and (teletype-style) character terminals using the
 * UTF-8 encoding requires agreement on which character should advance
 * the cursor by how many cell positions. No established formal
 * standards exist at present on which Unicode character shall occupy
 * how many cell positions on character terminals. These routines are
 * a first attempt of defining such behavior based on simple rules
 * applied to data provided by the Unicode Consortium.
 *
 * For some graphical characters, the Unicode standard explicitly
 * defines a character-cell width via the definition of the East Asian
 * FullWidth (F), Wide (W), Half-width (H), and Narrow (Na) classes.
 * In all these cases, there is no ambiguity about which width a
 * terminal shall use. For characters in the East Asian Ambiguous (A)
 * class, the width choice depends purely on a preference of backward
 * compatibility with either historic CJK or Western practice.
 * Choosing single-width for these characters is easy to justify as
 * the appropriate long-term solution, as the CJK practice of
 * displaying these characters as double-width comes from historic
 * implementation simplicity (8-bit encoded characters were displayed
 * single-width and 16-bit ones double-width, even for Greek,
 * Cyrillic, etc.) and not any typographic considerations.
 *
 * Much less clear is the choice of width for the Not East Asian
 * (Neutral) class. Existing practice does not dictate a width for any
 * of these characters. It would nevertheless make sense
 * typographically to allocate two character cells to characters such
 * as for instance EM SPACE or VOLUME INTEGRAL, which cannot be
 * represented adequately with a single-width glyph. The following
 * routines at present merely assign a single-cell width to all
 * neutral characters, in the interest of simplicity. This is not
 * entirely satisfactory and should be reconsidered before
 * establishing a formal standard in this area. At the moment, the
 * decision which Not East Asian (Neutral) characters should be
 * represented by double-width glyphs cannot yet be answered by
 * applying a simple rule from the Unicode database content. Setting
 * up a proper standard for the behavior of UTF-8 character terminals
 * will require a careful analysis not only of each Unicode character,
 * but also of each presentation form, something the author of these
 * routines has avoided to do so far.
 *
 * http://www.unicode.org/unicode/reports/tr11/
 *
 * Markus Kuhn -- 2007-05-26 (Unicode 5.0)
 *
 * Permission to use, copy, modify, and distribute this software
 * for any purpose and without fee is hereby granted. The author
 * disclaims all warranties with regard to this software.
 *
 * Latest version: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
 */

#include <stdint.h>
#include <string.h>

struct interval {
	int32_t first;
	int32_t last;
};

/* auxiliary function for binary search in interval table */
static int bisearch(int32_t ucs, const struct interval *table, int max) {
	int min = 0;
	int mid;

	if (ucs < table[0].first || ucs > table[max].last)
		return 0;
	while (max >= min) {
		mid = (min + max) / 2;
		if (ucs > table[mid].last)
			min = mid + 1;
		else if (ucs < table[mid].first)
			max = mid - 1;
		else
			return 1;
	}

	return 0;
}


/* The following two functions define the column width of an ISO 10646
 * character as follows:
 *
 *		- The null character (U+0000) has a column width of 0.
 *
 *		- Other C0/C1 control characters and DEL will lead to a return
 *			value of -1.
 *
 *		- Non-spacing and enclosing combining characters (general
 *			category code Mn or Me in the Unicode database) have a
 *			column width of 0.
 *
 *		- SOFT HYPHEN (U+00AD) has a column width of 1.
 *
 *		- Other format characters (general category code Cf in the Unicode
 *			database) and ZERO WIDTH SPACE (U+200B) have a column width of 0.
 *
 *		- Hangul Jamo medial vowels and final consonants (U+1160-U+11FF)
 *			have a column width of 0.
 *
 *		- Spacing characters in the East Asian Wide (W) or East Asian
 *			Full-width (F) category as defined in Unicode Technical
 *			Report #11 have a column width of 2.
 *
 *		- All remaining characters (including all printable
 *			ISO 8859-1 and WGL4 characters, Unicode control characters,
 *			etc.) have a column width of 1.
 *
 * This implementation assumes that wchar_t characters are encoded
 * in ISO 10646.
 */

static int mk_is_wide_char(int32_t ucs) {
  static const struct interval wide[] = {
    {0x1100, 0x115f}, {0x231a, 0x231b}, {0x2329, 0x232a},
    {0x23e9, 0x23ec}, {0x23f0, 0x23f0}, {0x23f3, 0x23f3},
    {0x25fd, 0x25fe}, {0x2614, 0x2615}, {0x2648, 0x2653},
    {0x267f, 0x267f}, {0x2693, 0x2693}, {0x26a1, 0x26a1},
    {0x26aa, 0x26ab}, {0x26bd, 0x26be}, {0x26c4, 0x26c5},
    {0x26ce, 0x26ce}, {0x26d4, 0x26d4}, {0x26ea, 0x26ea},
    {0x26f2, 0x26f3}, {0x26f5, 0x26f5}, {0x26fa, 0x26fa},
    {0x26fd, 0x26fd}, {0x2705, 0x2705}, {0x270a, 0x270b},
    {0x2728, 0x2728}, {0x274c, 0x274c}, {0x274e, 0x274e},
    {0x2753, 0x2755}, {0x2757, 0x2757}, {0x2795, 0x2797},
    {0x27b0, 0x27b0}, {0x27bf, 0x27bf}, {0x2b1b, 0x2b1c},
    {0x2b50, 0x2b50}, {0x2b55, 0x2b55}, {0x2e80, 0x2fdf},
    {0x2ff0, 0x303e}, {0x3040, 0x3247}, {0x3250, 0x4dbf},
    {0x4e00, 0xa4cf}, {0xa960, 0xa97f}, {0xac00, 0xd7a3},
    {0xf900, 0xfaff}, {0xfe10, 0xfe19}, {0xfe30, 0xfe6f},
    {0xff01, 0xff60}, {0xffe0, 0xffe6}, {0x16fe0, 0x16fe1},
    {0x17000, 0x18aff}, {0x1b000, 0x1b12f}, {0x1b170, 0x1b2ff},
    {0x1f004, 0x1f004}, {0x1f0cf, 0x1f0cf}, {0x1f18e, 0x1f18e},
    {0x1f191, 0x1f19a}, {0x1f200, 0x1f202}, {0x1f210, 0x1f23b},
    {0x1f240, 0x1f248}, {0x1f250, 0x1f251}, {0x1f260, 0x1f265},
    {0x1f300, 0x1f320}, {0x1f32d, 0x1f335}, {0x1f337, 0x1f37c},
    {0x1f37e, 0x1f393}, {0x1f3a0, 0x1f3ca}, {0x1f3cf, 0x1f3d3},
    {0x1f3e0, 0x1f3f0}, {0x1f3f4, 0x1f3f4}, {0x1f3f8, 0x1f43e},
    {0x1f440, 0x1f440}, {0x1f442, 0x1f4fc}, {0x1f4ff, 0x1f53d},
    {0x1f54b, 0x1f54e}, {0x1f550, 0x1f567}, {0x1f57a, 0x1f57a},
    {0x1f595, 0x1f596}, {0x1f5a4, 0x1f5a4}, {0x1f5fb, 0x1f64f},
    {0x1f680, 0x1f6c5}, {0x1f6cc, 0x1f6cc}, {0x1f6d0, 0x1f6d2},
    {0x1f6eb, 0x1f6ec}, {0x1f6f4, 0x1f6f8}, {0x1f910, 0x1f93e},
    {0x1f940, 0x1f94c}, {0x1f950, 0x1f96b}, {0x1f980, 0x1f997},
    {0x1f9c0, 0x1f9c0}, {0x1f9d0, 0x1f9e6}, {0x20000, 0x2fffd},
    {0x30000, 0x3fffd},
  };

  if ( bisearch(ucs, wide, sizeof(wide) / sizeof(struct interval) - 1) ) {
    return 1;
	}

  return 0;
}

static int mk_wcwidth(int32_t ucs) {
	/* sorted list of non-overlapping intervals of non-spacing characters */
	/* generated by "uniset +cat=Me +cat=Mn +cat=Cf -00AD +1160-11FF +200B c" */
	static const struct interval combining[] = {
    {0x00ad, 0x00ad}, {0x0300, 0x036f}, {0x0483, 0x0489},
    {0x0591, 0x05bd}, {0x05bf, 0x05bf}, {0x05c1, 0x05c2},
    {0x05c4, 0x05c5}, {0x05c7, 0x05c7}, {0x0610, 0x061a},
    {0x061c, 0x061c}, {0x064b, 0x065f}, {0x0670, 0x0670},
    {0x06d6, 0x06dc}, {0x06df, 0x06e4}, {0x06e7, 0x06e8},
    {0x06ea, 0x06ed}, {0x0711, 0x0711}, {0x0730, 0x074a},
    {0x07a6, 0x07b0}, {0x07eb, 0x07f3}, {0x0816, 0x0819},
    {0x081b, 0x0823}, {0x0825, 0x0827}, {0x0829, 0x082d},
    {0x0859, 0x085b}, {0x08d4, 0x08e1}, {0x08e3, 0x0902},
    {0x093a, 0x093a}, {0x093c, 0x093c}, {0x0941, 0x0948},
    {0x094d, 0x094d}, {0x0951, 0x0957}, {0x0962, 0x0963},
    {0x0981, 0x0981}, {0x09bc, 0x09bc}, {0x09c1, 0x09c4},
    {0x09cd, 0x09cd}, {0x09e2, 0x09e3}, {0x0a01, 0x0a02},
    {0x0a3c, 0x0a3c}, {0x0a41, 0x0a42}, {0x0a47, 0x0a48},
    {0x0a4b, 0x0a4d}, {0x0a51, 0x0a51}, {0x0a70, 0x0a71},
    {0x0a75, 0x0a75}, {0x0a81, 0x0a82}, {0x0abc, 0x0abc},
    {0x0ac1, 0x0ac5}, {0x0ac7, 0x0ac8}, {0x0acd, 0x0acd},
    {0x0ae2, 0x0ae3}, {0x0afa, 0x0aff}, {0x0b01, 0x0b01},
    {0x0b3c, 0x0b3c}, {0x0b3f, 0x0b3f}, {0x0b41, 0x0b44},
    {0x0b4d, 0x0b4d}, {0x0b56, 0x0b56}, {0x0b62, 0x0b63},
    {0x0b82, 0x0b82}, {0x0bc0, 0x0bc0}, {0x0bcd, 0x0bcd},
    {0x0c00, 0x0c00}, {0x0c3e, 0x0c40}, {0x0c46, 0x0c48},
    {0x0c4a, 0x0c4d}, {0x0c55, 0x0c56}, {0x0c62, 0x0c63},
    {0x0c81, 0x0c81}, {0x0cbc, 0x0cbc}, {0x0cbf, 0x0cbf},
    {0x0cc6, 0x0cc6}, {0x0ccc, 0x0ccd}, {0x0ce2, 0x0ce3},
    {0x0d00, 0x0d01}, {0x0d3b, 0x0d3c}, {0x0d41, 0x0d44},
    {0x0d4d, 0x0d4d}, {0x0d62, 0x0d63}, {0x0dca, 0x0dca},
    {0x0dd2, 0x0dd4}, {0x0dd6, 0x0dd6}, {0x0e31, 0x0e31},
    {0x0e34, 0x0e3a}, {0x0e47, 0x0e4e}, {0x0eb1, 0x0eb1},
    {0x0eb4, 0x0eb9}, {0x0ebb, 0x0ebc}, {0x0ec8, 0x0ecd},
    {0x0f18, 0x0f19}, {0x0f35, 0x0f35}, {0x0f37, 0x0f37},
    {0x0f39, 0x0f39}, {0x0f71, 0x0f7e}, {0x0f80, 0x0f84},
    {0x0f86, 0x0f87}, {0x0f8d, 0x0f97}, {0x0f99, 0x0fbc},
    {0x0fc6, 0x0fc6}, {0x102d, 0x1030}, {0x1032, 0x1037},
    {0x1039, 0x103a}, {0x103d, 0x103e}, {0x1058, 0x1059},
    {0x105e, 0x1060}, {0x1071, 0x1074}, {0x1082, 0x1082},
    {0x1085, 0x1086}, {0x108d, 0x108d}, {0x109d, 0x109d},
    {0x1160, 0x11ff}, {0x135d, 0x135f}, {0x1712, 0x1714},
    {0x1732, 0x1734}, {0x1752, 0x1753}, {0x1772, 0x1773},
    {0x17b4, 0x17b5}, {0x17b7, 0x17bd}, {0x17c6, 0x17c6},
    {0x17c9, 0x17d3}, {0x17dd, 0x17dd}, {0x180b, 0x180e},
    {0x1885, 0x1886}, {0x18a9, 0x18a9}, {0x1920, 0x1922},
    {0x1927, 0x1928}, {0x1932, 0x1932}, {0x1939, 0x193b},
    {0x1a17, 0x1a18}, {0x1a1b, 0x1a1b}, {0x1a56, 0x1a56},
    {0x1a58, 0x1a5e}, {0x1a60, 0x1a60}, {0x1a62, 0x1a62},
    {0x1a65, 0x1a6c}, {0x1a73, 0x1a7c}, {0x1a7f, 0x1a7f},
    {0x1ab0, 0x1abe}, {0x1b00, 0x1b03}, {0x1b34, 0x1b34},
    {0x1b36, 0x1b3a}, {0x1b3c, 0x1b3c}, {0x1b42, 0x1b42},
    {0x1b6b, 0x1b73}, {0x1b80, 0x1b81}, {0x1ba2, 0x1ba5},
    {0x1ba8, 0x1ba9}, {0x1bab, 0x1bad}, {0x1be6, 0x1be6},
    {0x1be8, 0x1be9}, {0x1bed, 0x1bed}, {0x1bef, 0x1bf1},
    {0x1c2c, 0x1c33}, {0x1c36, 0x1c37}, {0x1cd0, 0x1cd2},
    {0x1cd4, 0x1ce0}, {0x1ce2, 0x1ce8}, {0x1ced, 0x1ced},
    {0x1cf4, 0x1cf4}, {0x1cf8, 0x1cf9}, {0x1dc0, 0x1df9},
    {0x1dfb, 0x1dff}, {0x200b, 0x200f}, {0x202a, 0x202e},
    {0x2060, 0x2064}, {0x2066, 0x206f}, {0x20d0, 0x20f0},
    {0x2cef, 0x2cf1}, {0x2d7f, 0x2d7f}, {0x2de0, 0x2dff},
    {0x302a, 0x302d}, {0x3099, 0x309a}, {0xa66f, 0xa672},
    {0xa674, 0xa67d}, {0xa69e, 0xa69f}, {0xa6f0, 0xa6f1},
    {0xa802, 0xa802}, {0xa806, 0xa806}, {0xa80b, 0xa80b},
    {0xa825, 0xa826}, {0xa8c4, 0xa8c5}, {0xa8e0, 0xa8f1},
    {0xa926, 0xa92d}, {0xa947, 0xa951}, {0xa980, 0xa982},
    {0xa9b3, 0xa9b3}, {0xa9b6, 0xa9b9}, {0xa9bc, 0xa9bc},
    {0xa9e5, 0xa9e5}, {0xaa29, 0xaa2e}, {0xaa31, 0xaa32},
    {0xaa35, 0xaa36}, {0xaa43, 0xaa43}, {0xaa4c, 0xaa4c},
    {0xaa7c, 0xaa7c}, {0xaab0, 0xaab0}, {0xaab2, 0xaab4},
    {0xaab7, 0xaab8}, {0xaabe, 0xaabf}, {0xaac1, 0xaac1},
    {0xaaec, 0xaaed}, {0xaaf6, 0xaaf6}, {0xabe5, 0xabe5},
    {0xabe8, 0xabe8}, {0xabed, 0xabed}, {0xfb1e, 0xfb1e},
    {0xfe00, 0xfe0f}, {0xfe20, 0xfe2f}, {0xfeff, 0xfeff},
    {0xfff9, 0xfffb}, {0x101fd, 0x101fd}, {0x102e0, 0x102e0},
    {0x10376, 0x1037a}, {0x10a01, 0x10a03}, {0x10a05, 0x10a06},
    {0x10a0c, 0x10a0f}, {0x10a38, 0x10a3a}, {0x10a3f, 0x10a3f},
    {0x10ae5, 0x10ae6}, {0x11001, 0x11001}, {0x11038, 0x11046},
    {0x1107f, 0x11081}, {0x110b3, 0x110b6}, {0x110b9, 0x110ba},
    {0x11100, 0x11102}, {0x11127, 0x1112b}, {0x1112d, 0x11134},
    {0x11173, 0x11173}, {0x11180, 0x11181}, {0x111b6, 0x111be},
    {0x111ca, 0x111cc}, {0x1122f, 0x11231}, {0x11234, 0x11234},
    {0x11236, 0x11237}, {0x1123e, 0x1123e}, {0x112df, 0x112df},
    {0x112e3, 0x112ea}, {0x11300, 0x11301}, {0x1133c, 0x1133c},
    {0x11340, 0x11340}, {0x11366, 0x1136c}, {0x11370, 0x11374},
    {0x11438, 0x1143f}, {0x11442, 0x11444}, {0x11446, 0x11446},
    {0x114b3, 0x114b8}, {0x114ba, 0x114ba}, {0x114bf, 0x114c0},
    {0x114c2, 0x114c3}, {0x115b2, 0x115b5}, {0x115bc, 0x115bd},
    {0x115bf, 0x115c0}, {0x115dc, 0x115dd}, {0x11633, 0x1163a},
    {0x1163d, 0x1163d}, {0x1163f, 0x11640}, {0x116ab, 0x116ab},
    {0x116ad, 0x116ad}, {0x116b0, 0x116b5}, {0x116b7, 0x116b7},
    {0x1171d, 0x1171f}, {0x11722, 0x11725}, {0x11727, 0x1172b},
    {0x11a01, 0x11a06}, {0x11a09, 0x11a0a}, {0x11a33, 0x11a38},
    {0x11a3b, 0x11a3e}, {0x11a47, 0x11a47}, {0x11a51, 0x11a56},
    {0x11a59, 0x11a5b}, {0x11a8a, 0x11a96}, {0x11a98, 0x11a99},
    {0x11c30, 0x11c36}, {0x11c38, 0x11c3d}, {0x11c3f, 0x11c3f},
    {0x11c92, 0x11ca7}, {0x11caa, 0x11cb0}, {0x11cb2, 0x11cb3},
    {0x11cb5, 0x11cb6}, {0x11d31, 0x11d36}, {0x11d3a, 0x11d3a},
    {0x11d3c, 0x11d3d}, {0x11d3f, 0x11d45}, {0x11d47, 0x11d47},
    {0x16af0, 0x16af4}, {0x16b30, 0x16b36}, {0x16f8f, 0x16f92},
    {0x1bc9d, 0x1bc9e}, {0x1bca0, 0x1bca3}, {0x1d167, 0x1d169},
    {0x1d173, 0x1d182}, {0x1d185, 0x1d18b}, {0x1d1aa, 0x1d1ad},
    {0x1d242, 0x1d244}, {0x1da00, 0x1da36}, {0x1da3b, 0x1da6c},
    {0x1da75, 0x1da75}, {0x1da84, 0x1da84}, {0x1da9b, 0x1da9f},
    {0x1daa1, 0x1daaf}, {0x1e000, 0x1e006}, {0x1e008, 0x1e018},
    {0x1e01b, 0x1e021}, {0x1e023, 0x1e024}, {0x1e026, 0x1e02a},
    {0x1e8d0, 0x1e8d6}, {0x1e944, 0x1e94a}, {0xe0001, 0xe0001},
    {0xe0020, 0xe007f}, {0xe0100, 0xe01ef},
	};

	/* test for 8-bit control characters */
	if ( ucs == 0 ) {
		return 0;
	}
	if ( ( ucs < 32 ) || ( ( ucs >= 0x7f ) && ( ucs < 0xa0 ) ) ) {
		return -1;
	}

	/* binary search in table of non-spacing characters */
	if ( bisearch( ucs, combining, sizeof( combining ) / sizeof( struct interval ) - 1 ) ) {
		return 0;
	}

	/* if we arrive here, ucs is not a combining or C0/C1 control character */
  return ( mk_is_wide_char( ucs ) ? 2 : 1 );
}

// #endif

#include <stdio.h>
#include <string.h>
#include <inttypes.h>

// skipping dup #include "common.h"
// skipping dup #include "stringbuf.h"

//-------------------------------------------------------------
// In place growable utf-8 strings
//-------------------------------------------------------------

struct stringbuf_s {
  char*     buf;
  ssize_t   buflen;
  ssize_t   count;  
  alloc_t*  mem;
};


//-------------------------------------------------------------
// String column width
//-------------------------------------------------------------

// column width of a utf8 single character sequence.
static ssize_t utf8_char_width( const char* s, ssize_t n ) {
  if (n <= 0) return 0;

  uint8_t b = (uint8_t)s[0];
  int32_t c;
  if (b < ' ') {
    return 0;
  }
  else if (b <= 0x7F) {
    return 1;
  }
  else if (b <= 0xC1) { // invalid continuation byte or invalid 0xC0, 0xC1 (check is strictly not necessary as we don't validate..)
    return 1;
  }
  else if (b <= 0xDF && n >= 2) { // b >= 0xC2  // 2 bytes
    c = (((b & 0x1F) << 6) | (s[1] & 0x3F));
    assert(c < 0xD800 || c > 0xDFFF);
    int w = wcwidth(c);
    return w;
  }
  else if (b <= 0xEF && n >= 3) { // b >= 0xE0  // 3 bytes 
    c = (((b & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
    return wcwidth(c);    
  }
  else if (b <= 0xF4 && n >= 4) { // b >= 0xF0  // 4 bytes 
    c = (((b & 0x07) << 18) | ((s[1] & 0x3F) << 12) | ((s[2] & 0x3F) << 6) | (s[3] & 0x3F));
    return wcwidth(c);
  }
  else {
    // failed
    return 1;
  }
}


// The column width of a codepoint (0, 1, or 2)
static ssize_t char_column_width( const char* s, ssize_t n ) {
  if (s == NULL || n <= 0) return 0;
  else if ((uint8_t)(*s) < ' ') return 0;   // also for CSI escape sequences
  else {
    ssize_t w = utf8_char_width(s, n);
    #ifdef _WIN32
    return (w <= 0 ? 1 : w); // windows console seems to use at least one column
    #else
    return w;
    #endif
  }
}

static ssize_t str_column_width_n( const char* s, ssize_t len ) {
  if (s == NULL || len <= 0) return 0;
  ssize_t pos = 0;
  ssize_t cwidth = 0;
  ssize_t cw;
  ssize_t ofs;
  while (s[pos] != 0 && (ofs = str_next_ofs(s, len, pos, &cw)) > 0) {
    cwidth += cw;
    pos += ofs;
  }  
  return cwidth;
}

ic_private ssize_t str_column_width( const char* s ) {
  return str_column_width_n( s, ic_strlen(s) );
}

ic_private ssize_t str_skip_until_fit( const char* s, ssize_t max_width ) {
  if (s == NULL) return 0;
  ssize_t cwidth = str_column_width(s);
  ssize_t len    = ic_strlen(s);
  ssize_t pos = 0;
  ssize_t next;
  ssize_t cw;
  while (cwidth > max_width && (next = str_next_ofs(s, len, pos, &cw)) > 0) {
    cwidth -= cw;
    pos += next;
  }
  return pos;
}

ic_private ssize_t str_take_while_fit( const char* s, ssize_t max_width) {
  if (s == NULL) return 0;
  const ssize_t len = ic_strlen(s);
  ssize_t pos = 0;
  ssize_t next;
  ssize_t cw;
  ssize_t cwidth = 0;
  while ((next = str_next_ofs(s, len, pos, &cw)) > 0) {
    if (cwidth + cw > max_width) break;
    cwidth += cw;
    pos += next;
  }
  return pos;
}


//-------------------------------------------------------------
// String navigation 
//-------------------------------------------------------------

// get offset of the previous codepoint. does not skip back over CSI sequences.
ic_private ssize_t str_prev_ofs( const char* s, ssize_t pos, ssize_t* width ) {
  ssize_t ofs = 0;
  if (s != NULL && pos > 0) {
    ofs = 1;
    while (pos > ofs) {
      uint8_t u = (uint8_t)s[pos - ofs];
      if (u < 0x80 || u > 0xBF) break;  // continue while follower
      ofs++;
    }
  }
  if (width != NULL) *width = char_column_width( s+(pos-ofs), ofs );
  return ofs;
}

// skip an escape sequence
// <https://www.xfree86.org/current/ctlseqs.html>
ic_private bool skip_esc( const char* s, ssize_t len, ssize_t* esclen ) {  
  if (s == NULL || len <= 1 || s[0] != '\x1B') return false;
  if (esclen != NULL) *esclen = 0;
  if (strchr("[PX^_]",s[1]) != NULL) {
    // CSI (ESC [), DCS (ESC P), SOS (ESC X), PM (ESC ^), APC (ESC _), and OSC (ESC ]): terminated with a special sequence
    bool finalCSI = (s[1] == '[');  // CSI terminates with 0x40-0x7F; otherwise ST (bell or ESC \)
    ssize_t n = 2;
    while (len > n) {
      char c = s[n++];
      if ((finalCSI && (uint8_t)c >= 0x40 && (uint8_t)c <= 0x7F) ||  // terminating byte: @A–Z[\]^_`a–z{|}~
          (!finalCSI && c == '\x07') ||   // bell
          (c == '\x02'))                  // STX terminates as well
      {
        if (esclen != NULL) *esclen = n;
        return true;
      }
      else if (!finalCSI && c == '\x1B' && len > n && s[n] == '\\') {  // ST (ESC \)
        n++;
        if (esclen != NULL) *esclen = n;
        return true;
      }
    }
  }
  if (strchr(" #%()*+",s[1]) != NULL) {
    // assume escape sequence of length 3 (like ESC % G)
    if (esclen != NULL) *esclen = 2;
    return true;
  }
  else {
    // assume single character escape code (like ESC 7)
    if (esclen != NULL) *esclen = 2;
    return true;
  }
  return false;
}

// Offset to the next codepoint, treats CSI escape sequences as a single code point.
ic_private ssize_t str_next_ofs( const char* s, ssize_t len, ssize_t pos, ssize_t* cwidth ) {
  ssize_t ofs = 0;
  if (s != NULL && len > pos) {
    if (skip_esc(s+pos,len-pos,&ofs)) {
      // skip escape sequence      
    }
    else {
      ofs = 1;
      // utf8 extended character?
      while(len > pos + ofs) {
        uint8_t u = (uint8_t)s[pos + ofs];
        if (u < 0x80 || u > 0xBF) break;  // break if not a follower
        ofs++;
      }      
    } 
  }
  if (cwidth != NULL) *cwidth = char_column_width( s+pos, ofs );
  return ofs;
}

static ssize_t str_limit_to_length( const char* s, ssize_t n ) {
  ssize_t i;
  for(i = 0; i < n && s[i] != 0; i++) { /* nothing */ }
  return i;
}


//-------------------------------------------------------------
// String searching prev/next word, line, ws_word
//-------------------------------------------------------------


static ssize_t str_find_backward( const char* s, ssize_t len, ssize_t pos, ic_is_char_class_fun_t* match, bool skip_immediate_matches ) {
  if (pos > len) pos = len;
  if (pos < 0) pos = 0;
  ssize_t i = pos;
  // skip matching first (say, whitespace in case of the previous start-of-word)
  if (skip_immediate_matches) {
    do {
      ssize_t prev = str_prev_ofs(s, i, NULL); 
      if (prev <= 0) break;
      assert(i - prev >= 0);
      if (!match(s + i - prev, (long)prev)) break;
      i -= prev;
    } while (i > 0);  
  }
  // find match
  do {
    ssize_t prev = str_prev_ofs(s, i, NULL); 
    if (prev <= 0) break;
    assert(i - prev >= 0);
    if (match(s + i - prev, (long)prev)) {
      return i;  // found;
    }
    i -= prev;
  } while (i > 0);
  return -1; // not found
}

static ssize_t str_find_forward( const char* s, ssize_t len, ssize_t pos, ic_is_char_class_fun_t* match, bool skip_immediate_matches ) {
  if (s == NULL || len < 0) return -1;
  if (pos > len) pos = len;
  if (pos < 0) pos = 0;  
  ssize_t i = pos;
  ssize_t next;
  // skip matching first (say, whitespace in case of the next end-of-word)
  if (skip_immediate_matches) {
    do {
      next = str_next_ofs(s, len, i, NULL); 
      if (next <= 0) break;
      assert( i + next <= len);
      if (!match(s + i, (long)next)) break;
      i += next;
    } while (i < len);  
  }
  // and then look
  do {
    next = str_next_ofs(s, len, i, NULL); 
    if (next <= 0) break;
    assert( i + next <= len);
    if (match(s + i, (long)next)) {
      return i; // found
    }
    i += next;
  } while (i < len);
  return -1;
} 

static bool char_is_linefeed( const char* s, long n ) {  
  return (n == 1 && (*s == '\n' || *s == 0));
}

static ssize_t str_find_line_start( const char* s, ssize_t len, ssize_t pos) {
  ssize_t start = str_find_backward(s,len,pos,&char_is_linefeed,false /* don't skip immediate matches */);
  return (start < 0 ? 0 : start); 
}

static ssize_t str_find_line_end( const char* s, ssize_t len, ssize_t pos) {
  ssize_t end = str_find_forward(s,len,pos, &char_is_linefeed, false);
  return (end < 0 ? len : end);
}

static ssize_t str_find_word_start( const char* s, ssize_t len, ssize_t pos) {
  ssize_t start = str_find_backward(s,len,pos, &ic_char_is_idletter,true /* skip immediate matches */);
  return (start < 0 ? 0 : start); 
}

static ssize_t str_find_word_end( const char* s, ssize_t len, ssize_t pos) {
  ssize_t end = str_find_forward(s,len,pos,&ic_char_is_idletter,true /* skip immediate matches */);
  return (end < 0 ? len : end); 
}

static ssize_t str_find_ws_word_start( const char* s, ssize_t len, ssize_t pos) {
  ssize_t start = str_find_backward(s,len,pos,&ic_char_is_white,true /* skip immediate matches */);
  return (start < 0 ? 0 : start); 
}

static ssize_t str_find_ws_word_end( const char* s, ssize_t len, ssize_t pos) {
  ssize_t end = str_find_forward(s,len,pos,&ic_char_is_white,true /* skip immediate matches */);
  return (end < 0 ? len : end); 
}


//-------------------------------------------------------------
// String row/column iteration
//-------------------------------------------------------------

// invoke a function for each terminal row; returns total row count.
static ssize_t str_for_each_row( const char* s, ssize_t len, ssize_t termw, ssize_t promptw, ssize_t cpromptw,
                                 row_fun_t* fun, const void* arg, void* res ) 
{
  if (s == NULL) s = "";
  ssize_t i;
  ssize_t rcount = 0;
  ssize_t rcol = 0;
  ssize_t rstart = 0;  
  ssize_t startw  = promptw; 
  for(i = 0; i < len; ) {
    ssize_t w;
    ssize_t next = str_next_ofs(s, len, i, &w);    
    if (next <= 0) {
      debug_msg("str: foreach row: next<=0: len %zd, i %zd, w %zd, buf %s\n", len, i, w, s );
      assert(false);
      break;
    }
    startw = (rcount == 0 ? promptw : cpromptw);
    ssize_t termcol = rcol + w + startw + 1 /* for the cursor */;
    if (termw != 0 && i != 0 && termcol >= termw) {  
      // wrap
      if (fun != NULL) {
        if (fun(s,rcount,rstart,i - rstart,startw,true,arg,res)) return rcount;
      }
      rcount++;
      rstart = i;
      rcol   = 0;
    }
    if (s[i] == '\n') {
      // newline
      if (fun != NULL) {
        if (fun(s,rcount,rstart,i - rstart,startw,false,arg,res)) return rcount;
      }
      rcount++;
      rstart = i+1;
      rcol = 0;
    }
    assert (s[i] != 0);
    i += next;
    rcol += w;
  }
  if (fun != NULL) {
    if (fun(s,rcount,rstart,i - rstart,startw,false,arg,res)) return rcount;
  }
  return rcount+1;
}

//-------------------------------------------------------------
// String: get row/column position
//-------------------------------------------------------------


static bool str_get_current_pos_iter(
    const char* s,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    ssize_t startw, bool is_wrap, const void* arg, void* res)
{
  ic_unused(is_wrap); ic_unused(startw);
  rowcol_t* rc = (rowcol_t*)res;
  ssize_t pos = *((ssize_t*)arg);

  if (pos >= row_start && pos <= (row_start + row_len)) {
    // found the cursor row
    rc->row_start = row_start;
    rc->row_len   = row_len;
    rc->row = row;
    rc->col = str_column_width_n( s + row_start, pos - row_start );
    rc->first_on_row = (pos == row_start);
    if (is_wrap) {
      // if wrapped, we check if the next character is at row_len
      ssize_t next = str_next_ofs(s, row_start + row_len, pos, NULL);
      rc->last_on_row = (pos + next >= row_start + row_len);
    }
    else {
      // normal last position is right after the last character
      rc->last_on_row = (pos >= row_start + row_len); 
    }
    // debug_msg("edit; pos iter: pos: %zd (%c), row_start: %zd, rowlen: %zd\n", pos, s[pos], row_start, row_len);    
  }  
  return false; // always continue to count all rows
}

static ssize_t str_get_rc_at_pos(const char* s, ssize_t len, ssize_t termw, ssize_t promptw, ssize_t cpromptw, ssize_t pos, rowcol_t* rc) {
  memset(rc, 0, sizeof(*rc));
  ssize_t rows = str_for_each_row(s, len, termw, promptw, cpromptw, &str_get_current_pos_iter, &pos, rc);
  // debug_msg("edit: current pos: (%d, %d) %s %s\n", rc->row, rc->col, rc->first_on_row ? "first" : "", rc->last_on_row ? "last" : "");
  return rows;
}



//-------------------------------------------------------------
// String: get row/column position for a resized terminal
// with potentially "hard-wrapped" rows
//-------------------------------------------------------------
typedef struct wrapped_arg_s {
  ssize_t  pos;
  ssize_t  newtermw;
} wrapped_arg_t;

typedef struct wrowcol_s {
  rowcol_t rc;
  ssize_t  hrows;  // count of hard-wrapped extra rows
} wrowcol_t;

static bool str_get_current_wrapped_pos_iter(
    const char* s,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    ssize_t startw, bool is_wrap, const void* arg, void* res)
{
  ic_unused(is_wrap);
  wrowcol_t*     wrc = (wrowcol_t*)res;
  const wrapped_arg_t* warg = (const wrapped_arg_t*)arg;

  // iterate through the row and record the postion and hard-wraps
  ssize_t hwidth = startw;
  ssize_t i = 0;
  while( i <= row_len ) {  // include rowlen as the cursor position can be just after the last character
    // get next position and column width
    ssize_t cw;
    ssize_t next;
    bool is_cursor = (warg->pos == row_start+i);
    if (i < row_len) {
      next = str_next_ofs(s + row_start, row_len, i, &cw);
    }
    else {
      // end of row: take wrap or cursor into account
      // (wrap has width 2 as it displays a back-arrow but also has an invisible newline that wraps)
      cw = (is_wrap ? 2 : (is_cursor ? 1 : 0));
      next = 1;
    }

    if (next > 0) {
      if (hwidth + cw > warg->newtermw) {
        // hardwrap
        hwidth = 0;
        wrc->hrows++;
        debug_msg("str: found hardwrap: row: %zd, hrows: %zd\n", row, wrc->hrows);      
      }
    }    
    else {
      next++; // ensure we terminate (as we go up to rowlen)      
    }

    // did we find our position?
    if (is_cursor) {
      debug_msg("str: found position: row: %zd, hrows: %zd\n", row, wrc->hrows);
      wrc->rc.row_start = row_start;
      wrc->rc.row_len   = row_len;      
      wrc->rc.row       = wrc->hrows + row;
      wrc->rc.col       = hwidth;      
      wrc->rc.first_on_row = (i==0);
      wrc->rc.last_on_row  = (i+next >= row_len - (is_wrap ? 1 : 0)); 
    }

    // advance
    hwidth += cw;
    i += next;    
  }
  return false; // always continue to count all rows
}


static ssize_t str_get_wrapped_rc_at_pos(const char* s, ssize_t len, ssize_t termw, ssize_t newtermw, ssize_t promptw, ssize_t cpromptw, ssize_t pos, rowcol_t* rc) {
  wrapped_arg_t warg;
  warg.pos = pos;
  warg.newtermw = newtermw;
  wrowcol_t wrc;
  memset(&wrc,0,sizeof(wrc));
  ssize_t rows = str_for_each_row(s, len, termw, promptw, cpromptw, &str_get_current_wrapped_pos_iter, &warg, &wrc);
  debug_msg("edit: wrapped pos: (%zd,%zd) rows %zd %s %s, hrows: %zd\n", wrc.rc.row, wrc.rc.col, rows, wrc.rc.first_on_row ? "first" : "", wrc.rc.last_on_row ? "last" : "", wrc.hrows);
  *rc = wrc.rc;
  return (rows + wrc.hrows);
}


//-------------------------------------------------------------
// Set position
//-------------------------------------------------------------

static bool str_set_pos_iter(
    const char* s,
    ssize_t row, ssize_t row_start, ssize_t row_len, 
    ssize_t startw, bool is_wrap, const void* arg, void* res)
{
  ic_unused(arg); ic_unused(is_wrap); ic_unused(startw);
  rowcol_t* rc = (rowcol_t*)arg;
  if (rc->row != row) return false; // keep searching
  // we found our row
  ssize_t col = 0; 
  ssize_t i   = row_start;
  ssize_t end = row_start + row_len;
  while (col < rc->col && i < end) {
    ssize_t cw;
    ssize_t next = str_next_ofs(s, row_start + row_len, i, &cw);
    if (next <= 0) break;
    i   += next;
    col += cw;
  }
  *((ssize_t*)res) = i;
  return true; // stop iteration
}

static ssize_t str_get_pos_at_rc(const char* s, ssize_t len, ssize_t termw, ssize_t promptw, ssize_t cpromptw, ssize_t row, ssize_t col /* without prompt */) {
  rowcol_t rc;
  memset(&rc,0,ssizeof(rc));
  rc.row = row;
  rc.col = col;
  ssize_t pos = -1;
  str_for_each_row(s,len,termw,promptw,cpromptw,&str_set_pos_iter,&rc,&pos);  
  return pos;
}


//-------------------------------------------------------------
// String buffer
//-------------------------------------------------------------
static bool sbuf_ensure_extra(stringbuf_t* s, ssize_t extra) 
{
  if (s->buflen >= s->count + extra) return true;   
  // reallocate; pick good initial size and multiples to increase reuse on allocation
  ssize_t newlen = (s->buflen <= 0 ? 120 : (s->buflen > 1000 ? s->buflen + 1000 : 2*s->buflen));
  if (newlen < s->count + extra) newlen = s->count + extra;
  if (s->buflen > 0) {
    debug_msg("stringbuf: reallocate: old %zd, new %zd\n", s->buflen, newlen);
  }
  char* newbuf = mem_realloc_tp(s->mem, char, s->buf, newlen+1); // one more for terminating zero
  if (newbuf == NULL) {
    assert(false);
    return false;
  }
  s->buf = newbuf;
  s->buflen = newlen;
  s->buf[s->count] = s->buf[s->buflen] = 0;
  assert(s->buflen >= s->count + extra);
  return true;
}

static void sbuf_init( stringbuf_t* sbuf, alloc_t* mem ) {
  sbuf->mem = mem;
  sbuf->buf = NULL;
  sbuf->buflen = 0;
  sbuf->count = 0;
}

static void sbuf_done( stringbuf_t* sbuf ) {
  mem_free( sbuf->mem, sbuf->buf );
  sbuf->buf = NULL;
  sbuf->buflen = 0;
  sbuf->count = 0;
}


ic_private void sbuf_free( stringbuf_t* sbuf ) {
  if (sbuf==NULL) return;
  sbuf_done(sbuf);
  mem_free(sbuf->mem, sbuf);
}

ic_private stringbuf_t*  sbuf_new( alloc_t* mem ) {
  stringbuf_t* sbuf = mem_zalloc_tp(mem,stringbuf_t);
  if (sbuf == NULL) return NULL;
  sbuf_init(sbuf,mem);
  return sbuf;
}

// free the sbuf and return the current string buffer as the result
ic_private char* sbuf_free_dup(stringbuf_t* sbuf) {
  if (sbuf == NULL) return NULL;
  char* s = NULL;
  if (sbuf->buf != NULL) {
    s = mem_realloc_tp(sbuf->mem, char, sbuf->buf, sbuf_len(sbuf)+1);
    if (s == NULL) { s = sbuf->buf; }
    sbuf->buf = 0;
    sbuf->buflen = 0;
    sbuf->count = 0;
  }
  sbuf_free(sbuf);
  return s;
}

ic_private const char* sbuf_string_at( stringbuf_t* sbuf, ssize_t pos ) {
  if (pos < 0 || sbuf->count < pos) return NULL;
  if (sbuf->buf == NULL) return "";
  assert(sbuf->buf[sbuf->count] == 0);
  return sbuf->buf + pos;
}

ic_private const char* sbuf_string( stringbuf_t* sbuf ) {
  return sbuf_string_at( sbuf, 0 );
}

ic_private char sbuf_char_at(stringbuf_t* sbuf, ssize_t pos) {
  if (sbuf->buf == NULL || pos < 0 || sbuf->count < pos) return 0;
  return sbuf->buf[pos];
}

ic_private char* sbuf_strdup_at( stringbuf_t* sbuf, ssize_t pos ) {
  return mem_strdup(sbuf->mem, sbuf_string_at(sbuf,pos));
}

ic_private char* sbuf_strdup( stringbuf_t* sbuf ) {
  return mem_strdup(sbuf->mem, sbuf_string(sbuf));
}

ic_private ssize_t sbuf_len(const stringbuf_t* s) {
  if (s == NULL) return 0;
  return s->count;
}

ic_private ssize_t sbuf_append_vprintf(stringbuf_t* sb, const char* fmt, va_list args) {
  const ssize_t min_needed = ic_strlen(fmt);
  if (!sbuf_ensure_extra(sb,min_needed + 16)) return sb->count;
  ssize_t avail = sb->buflen - sb->count;
  va_list args0;
  va_copy(args0, args);
  ssize_t needed = vsnprintf(sb->buf + sb->count, to_size_t(avail), fmt, args0);
  if (needed > avail) {
    sb->buf[sb->count] = 0;
    if (!sbuf_ensure_extra(sb, needed)) return sb->count;
    avail = sb->buflen - sb->count;
    needed = vsnprintf(sb->buf + sb->count, to_size_t(avail), fmt, args);
  }
  assert(needed <= avail);
  sb->count += (needed > avail ? avail : (needed >= 0 ? needed : 0));
  assert(sb->count <= sb->buflen);
  sb->buf[sb->count] = 0;
  return sb->count;
}

ic_private ssize_t sbuf_appendf(stringbuf_t* sb, const char* fmt, ...) {
  va_list args;
  va_start( args, fmt);
  ssize_t res = sbuf_append_vprintf( sb, fmt, args );
  va_end(args);
  return res;
}


ic_private ssize_t sbuf_insert_at_n(stringbuf_t* sbuf, const char* s, ssize_t n, ssize_t pos ) {
  if (pos < 0 || pos > sbuf->count || s == NULL) return pos;
  n = str_limit_to_length(s,n);
  if (n <= 0 || !sbuf_ensure_extra(sbuf,n)) return pos;
  ic_memmove(sbuf->buf + pos + n, sbuf->buf + pos, sbuf->count - pos);
  ic_memcpy(sbuf->buf + pos, s, n);
  sbuf->count += n;
  sbuf->buf[sbuf->count] = 0;
  return (pos + n);
}

ic_private stringbuf_t* sbuf_split_at( stringbuf_t* sb, ssize_t pos ) {
  stringbuf_t* res = sbuf_new(sb->mem);
  if (res==NULL || pos < 0) return NULL;
  if (pos < sb->count) {
    sbuf_append_n(res, sb->buf + pos, sb->count - pos);
    sb->count = pos;
  }
  return res;
}

ic_private ssize_t sbuf_insert_at(stringbuf_t* sbuf, const char* s, ssize_t pos ) {
  return sbuf_insert_at_n( sbuf, s, ic_strlen(s), pos );
}

ic_private ssize_t sbuf_insert_char_at(stringbuf_t* sbuf, char c, ssize_t pos ) {
  char s[2];
  s[0] = c;
  s[1] = 0;
  return sbuf_insert_at_n( sbuf, s, 1, pos);
}

ic_private ssize_t sbuf_insert_unicode_at(stringbuf_t* sbuf, unicode_t u, ssize_t pos) {
  uint8_t s[5];
  unicode_to_qutf8(u, s);
  return sbuf_insert_at(sbuf, (const char*)s, pos);
}



ic_private void sbuf_delete_at( stringbuf_t* sbuf, ssize_t pos, ssize_t count ) {
  if (pos < 0 || pos >= sbuf->count) return;
  if (pos + count > sbuf->count) count = sbuf->count - pos;
  ic_memmove(sbuf->buf + pos, sbuf->buf + pos + count, sbuf->count - pos - count);
  sbuf->count -= count;
  sbuf->buf[sbuf->count] = 0;
}

ic_private void sbuf_delete_from_to( stringbuf_t* sbuf, ssize_t pos, ssize_t end ) {
  if (end <= pos) return;
  sbuf_delete_at( sbuf, pos, end - pos);
}

ic_private void  sbuf_delete_from(stringbuf_t* sbuf, ssize_t pos ) {
  sbuf_delete_at(sbuf, pos, sbuf_len(sbuf) - pos );
}


ic_private void sbuf_clear( stringbuf_t* sbuf ) {
  sbuf_delete_at(sbuf, 0, sbuf_len(sbuf));
}

ic_private ssize_t sbuf_append_n( stringbuf_t* sbuf, const char* s, ssize_t n ) {
  return sbuf_insert_at_n( sbuf, s, n, sbuf_len(sbuf));
}

ic_private ssize_t sbuf_append( stringbuf_t* sbuf, const char* s ) {
  return sbuf_insert_at( sbuf, s, sbuf_len(sbuf));
}

ic_private ssize_t sbuf_append_char( stringbuf_t* sbuf, char c ) {
  char buf[2];
  buf[0] = c;
  buf[1] = 0;
  return sbuf_append( sbuf, buf );
}

ic_private void sbuf_replace(stringbuf_t* sbuf, const char* s) {
  sbuf_clear(sbuf);
  sbuf_append(sbuf,s);
}

ic_private ssize_t sbuf_next_ofs( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth ) {
  return str_next_ofs( sbuf->buf, sbuf->count, pos, cwidth);
}

ic_private ssize_t sbuf_prev_ofs( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth ) {
  return str_prev_ofs( sbuf->buf, pos, cwidth);
}

ic_private ssize_t sbuf_next( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth) {
  ssize_t ofs = sbuf_next_ofs(sbuf,pos,cwidth);
  if (ofs <= 0) return -1;
  assert(pos + ofs <= sbuf->count);
  return pos + ofs; 
}

ic_private ssize_t sbuf_prev( stringbuf_t* sbuf, ssize_t pos, ssize_t* cwidth) {
  ssize_t ofs = sbuf_prev_ofs(sbuf,pos,cwidth);
  if (ofs <= 0) return -1;
  assert(pos - ofs >= 0);
  return pos - ofs;
}

ic_private ssize_t sbuf_delete_char_before( stringbuf_t* sbuf, ssize_t pos ) {
  ssize_t n = sbuf_prev_ofs(sbuf, pos, NULL);
  if (n <= 0) return 0;  
  assert( pos - n >= 0 );
  sbuf_delete_at(sbuf, pos - n, n);
  return pos - n;
}

ic_private void sbuf_delete_char_at( stringbuf_t* sbuf, ssize_t pos ) {
  ssize_t n = sbuf_next_ofs(sbuf, pos, NULL);
  if (n <= 0) return;  
  assert( pos + n <= sbuf->count );
  sbuf_delete_at(sbuf, pos, n);
  return;
}

ic_private ssize_t sbuf_swap_char( stringbuf_t* sbuf, ssize_t pos ) {
  ssize_t next = sbuf_next_ofs(sbuf, pos, NULL);
  if (next <= 0) return 0;  
  ssize_t prev = sbuf_prev_ofs(sbuf, pos, NULL);
  if (prev <= 0) return 0;  
  char buf[64];
  if (prev >= 63) return 0;
  ic_memcpy(buf, sbuf->buf + pos - prev, prev );
  ic_memmove(sbuf->buf + pos - prev, sbuf->buf + pos, next);
  ic_memmove(sbuf->buf + pos - prev + next, buf, prev);
  return pos - prev;
}

ic_private ssize_t sbuf_find_line_start( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_line_start( sbuf->buf, sbuf->count, pos);
}

ic_private ssize_t sbuf_find_line_end( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_line_end( sbuf->buf, sbuf->count, pos);
}

ic_private ssize_t sbuf_find_word_start( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_word_start( sbuf->buf, sbuf->count, pos);
}

ic_private ssize_t sbuf_find_word_end( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_word_end( sbuf->buf, sbuf->count, pos);
}

ic_private ssize_t sbuf_find_ws_word_start( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_ws_word_start( sbuf->buf, sbuf->count, pos);
}

ic_private ssize_t sbuf_find_ws_word_end( stringbuf_t* sbuf, ssize_t pos ) {
  return str_find_ws_word_end( sbuf->buf, sbuf->count, pos);
}

// find row/col position
ic_private ssize_t sbuf_get_pos_at_rc( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t cpromptw, ssize_t row, ssize_t col ) {
  return str_get_pos_at_rc( sbuf->buf, sbuf->count, termw, promptw, cpromptw, row, col);
}

// get row/col for a given position
ic_private ssize_t sbuf_get_rc_at_pos( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t cpromptw, ssize_t pos, rowcol_t* rc ) {
  return str_get_rc_at_pos( sbuf->buf, sbuf->count, termw, promptw, cpromptw, pos, rc);
}

ic_private ssize_t sbuf_get_wrapped_rc_at_pos( stringbuf_t* sbuf, ssize_t termw, ssize_t newtermw, ssize_t promptw, ssize_t cpromptw, ssize_t pos, rowcol_t* rc ) {
  return str_get_wrapped_rc_at_pos( sbuf->buf, sbuf->count, termw, newtermw, promptw, cpromptw, pos, rc);
}

ic_private ssize_t sbuf_for_each_row( stringbuf_t* sbuf, ssize_t termw, ssize_t promptw, ssize_t cpromptw, row_fun_t* fun, void* arg, void* res ) {
  if (sbuf == NULL) return 0;
  return str_for_each_row( sbuf->buf, sbuf->count, termw, promptw, cpromptw, fun, arg, res);
}


// Duplicate and decode from utf-8 (for non-utf8 terminals)
ic_private char* sbuf_strdup_from_utf8(stringbuf_t* sbuf) {
  ssize_t len = sbuf_len(sbuf);
  if (sbuf == NULL || len <= 0) return NULL;
  char* s = mem_zalloc_tp_n(sbuf->mem, char, len);
  if (s == NULL) return NULL;
  ssize_t dest = 0;
  for (ssize_t i = 0; i < len; ) {
    ssize_t ofs = sbuf_next_ofs(sbuf, i, NULL);
    if (ofs <= 0) {
      // invalid input
      break;
    }
    else if (ofs == 1) {
      // regular character
      s[dest++] = sbuf->buf[i];
    }
    else if (sbuf->buf[i] == '\x1B') {
      // skip escape sequences
    }
    else {
      // decode unicode
      ssize_t nread;
      unicode_t uchr = unicode_from_qutf8( (const uint8_t*)(sbuf->buf + i), ofs, &nread);
      uint8_t c;
      if (unicode_is_raw(uchr, &c)) {
        // raw byte, output as is (this will take care of locale specific input)
        s[dest++] = (char)c;
      }
      else if (uchr <= 0x7F) {
        // allow ascii
        s[dest++] = (char)uchr;
      }
      else {
        // skip unknown unicode characters..
        // todo: convert according to locale?
      }
    }
    i += ofs;
  }
  assert(dest <= len);
  s[dest] = 0;
  return s;
}

//-------------------------------------------------------------
// String helpers
//-------------------------------------------------------------

ic_public long ic_prev_char( const char* s, long pos ) {
  ssize_t len = ic_strlen(s);
  if (pos < 0 || pos > len) return -1;
  ssize_t ofs = str_prev_ofs( s, pos, NULL );
  if (ofs <= 0) return -1;
  return (long)(pos - ofs);
}

ic_public long ic_next_char( const char* s, long pos ) {
  ssize_t len = ic_strlen(s);
  if (pos < 0 || pos > len) return -1;
  ssize_t ofs = str_next_ofs( s, len, pos, NULL );
  if (ofs <= 0) return -1;
  return (long)(pos + ofs);
}


// parse a decimal (leave pi unchanged on error)
ic_private bool ic_atoz(const char* s, ssize_t* pi) {
  return (sscanf(s, "%zd", pi) == 1);
}

// parse two decimals separated by a semicolon 
ic_private bool ic_atoz2(const char* s, ssize_t* pi, ssize_t* pj) {
  return (sscanf(s, "%zd;%zd", pi, pj) == 2);
}

// parse unsigned 32-bit (leave pu unchanged on error)
ic_private bool ic_atou32(const char* s, uint32_t* pu) {
  return (sscanf(s, "%" SCNu32, pu) == 1);
}


// Convenience: character class for whitespace `[ \t\r\n]`.
ic_public bool ic_char_is_white(const char* s, long len) {
  if (s == NULL || len != 1) return false;
  const char c = *s;
  return (c==' ' || c == '\t' || c == '\n' || c == '\r');
}

// Convenience: character class for non-whitespace `[^ \t\r\n]`.
ic_public bool ic_char_is_nonwhite(const char* s, long len) {
  return !ic_char_is_white(s, len);
}

// Convenience: character class for separators `[ \t\r\n,.;:/\\\(\)\{\}\[\]]`.
ic_public bool ic_char_is_separator(const char* s, long len) {
  if (s == NULL || len != 1) return false;
  const char c = *s;
  return (strchr(" \t\r\n,.;:/\\(){}[]", c) != NULL);
}

// Convenience: character class for non-separators.
ic_public bool ic_char_is_nonseparator(const char* s, long len) {
  return !ic_char_is_separator(s, len);
}


// Convenience: character class for digits (`[0-9]`).
ic_public bool ic_char_is_digit(const char* s, long len) {
  if (s == NULL || len != 1) return false;
  const char c = *s;
  return (c >= '0' && c <= '9');
}

// Convenience: character class for hexadecimal digits (`[A-Fa-f0-9]`).
ic_public bool ic_char_is_hexdigit(const char* s, long len) {
  if (s == NULL || len != 1) return false;
  const char c = *s;
  return ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'));
}

// Convenience: character class for letters (`[A-Za-z]` and any unicode > 0x80).
ic_public bool ic_char_is_letter(const char* s, long len) {
  if (s == NULL || len <= 0) return false;
  const char c = *s;
  return ((uint8_t)c >= 0x80 || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
}

// Convenience: character class for identifier letters (`[A-Za-z0-9_-]` and any unicode > 0x80).
ic_public bool ic_char_is_idletter(const char* s, long len) {
  if (s == NULL || len <= 0) return false;
  const char c = *s;
  return ((uint8_t)c >= 0x80 || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || (c == '_') || (c == '-'));
}

// Convenience: character class for filename letters (`[^ \t\r\n`@$><=;|&{(]`).
ic_public bool ic_char_is_filename_letter(const char* s, long len) {
  if (s == NULL || len <= 0) return false;
  const char c = *s;
  return ((uint8_t)c >= 0x80 || (strchr(" \t\r\n`@$><=;|&{}()[]", c) == NULL));
}

// Convenience: If this is a token start, returns the length (or <= 0 if not found).
ic_public long ic_is_token(const char* s, long pos, ic_is_char_class_fun_t* is_token_char) {
  if (s == NULL || pos < 0 || is_token_char == NULL) return -1;
  ssize_t len = ic_strlen(s);
  if (pos >= len) return -1;
  if (pos > 0 && is_token_char(s + pos -1, 1)) return -1; // token start?
  ssize_t i = pos;
  while ( i < len ) {
    ssize_t next = str_next_ofs(s, len, i, NULL);
    if (next <= 0) return -1;
    if (!is_token_char(s + i, (long)next)) break;
    i += next;
  }
  return (long)(i - pos);
}


static int ic_strncmp(const char* s1, const char* s2, ssize_t n) {
  return strncmp(s1, s2, to_size_t(n));
}

// Convenience: Does this match the specified token? 
// Ensures not to match prefixes or suffixes, and returns the length of the match (in bytes).
// E.g. `ic_match_token("function",0,&ic_char_is_letter,"fun")` returns 0.
ic_public long ic_match_token(const char* s, long pos, ic_is_char_class_fun_t* is_token_char, const char* token) {
  long n = ic_is_token(s, pos, is_token_char);
  if (n > 0 && token != NULL && n == ic_strlen(token) && ic_strncmp(s + pos, token, n) == 0) {
    return n;
  }
  else {
    return 0;
  }
}


// Convenience: Do any of the specified tokens match? 
// Ensures not to match prefixes or suffixes, and returns the length of the match (in bytes).
// Ensures not to match prefixes or suffixes. 
// E.g. `ic_match_any_token("function",0,&ic_char_is_letter,{"fun","func",NULL})` returns 0.
ic_public long ic_match_any_token(const char* s, long pos, ic_is_char_class_fun_t* is_token_char, const char** tokens) {
  long n = ic_is_token(s, pos, is_token_char);
  if (n <= 0 || tokens == NULL) return 0;
  for (const char** token = tokens; *token != NULL; token++) {
    if (n == ic_strlen(*token) && ic_strncmp(s + pos, *token, n) == 0) {
      return n;
    }
  }
  return 0;
}

// # include "common.c"
/* ----------------------------------------------------------------------------
  Copyright (c) 2021, Daan Leijen
  This is free software; you can redistribute it and/or modify it
  under the terms of the MIT License. A copy of the license can be
  found in the "LICENSE" file at the root of this distribution.
-----------------------------------------------------------------------------*/

#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
// skipping dup #include "common.h"


//-------------------------------------------------------------
// String wrappers for ssize_t
//-------------------------------------------------------------

ic_private ssize_t ic_strlen( const char* s ) {
  if (s==NULL) return 0;
  return to_ssize_t(strlen(s));
}

ic_private void ic_memmove( void* dest, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (n <= 0) return;
  memmove(dest,src,to_size_t(n));
}


ic_private void ic_memcpy( void* dest, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (dest == NULL || src == NULL || n <= 0) return;
  memcpy(dest,src,to_size_t(n));
}

ic_private void ic_memset(void* dest, uint8_t value, ssize_t n) {
  assert(dest!=NULL);
  if (dest == NULL || n <= 0) return;
  memset(dest,(int8_t)value,to_size_t(n));
}

ic_private bool ic_memnmove( void* dest, ssize_t dest_size, const void* src, ssize_t n ) {
  assert(dest!=NULL && src != NULL);
  if (n <= 0) return true;
  if (dest_size < n) { assert(false); return false; }
  memmove(dest,src,to_size_t(n));
  return true;
}

ic_private bool ic_strcpy( char* dest, ssize_t dest_size /* including 0 */, const char* src) {
  assert(dest!=NULL && src != NULL);
  if (dest == NULL || dest_size <= 0) return false;
  ssize_t slen = ic_strlen(src);
  if (slen >= dest_size) return false;
  strcpy(dest,src);
  assert(dest[slen] == 0);
  return true;
}


ic_private bool ic_strncpy( char* dest, ssize_t dest_size /* including 0 */, const char* src, ssize_t n) {
  assert(dest!=NULL && n < dest_size);
  if (dest == NULL || dest_size <= 0) return false;
  if (n >= dest_size) return false;
  if (src==NULL || n <= 0) {
    dest[0] = 0;
  }
  else {
    strncpy(dest,src,to_size_t(n));
    dest[n] = 0;  
  }
  return true;
}

//-------------------------------------------------------------
// String matching
//-------------------------------------------------------------

ic_public bool ic_starts_with( const char* s, const char* prefix ) {
  if (s==prefix) return true;
  if (prefix==NULL) return true;
  if (s==NULL) return false;

  ssize_t i;
  for( i = 0; s[i] != 0 && prefix[i] != 0; i++) {
    if (s[i] != prefix[i]) return false;
  }
  return (prefix[i] == 0);
}

ic_private char ic_tolower( char c ) {
  return (c >= 'A' && c <= 'Z'  ?  c - 'A' + 'a' : c);
}

ic_private void ic_str_tolower(char* s) {
  while(*s != 0) {
    *s = ic_tolower(*s);
    s++;
  }
}

ic_public bool ic_istarts_with( const char* s, const char* prefix ) {
  if (s==prefix) return true;
  if (prefix==NULL) return true;
  if (s==NULL) return false;

  ssize_t i;
  for( i = 0; s[i] != 0 && prefix[i] != 0; i++) {
    if (ic_tolower(s[i]) != ic_tolower(prefix[i])) return false;
  }
  return (prefix[i] == 0);
}


ic_private int ic_strnicmp(const char* s1, const char* s2, ssize_t n) {
  if (s1 == NULL && s2 == NULL) return 0;
  if (s1 == NULL) return -1;
  if (s2 == NULL) return 1;
  ssize_t i;
  for (i = 0; s1[i] != 0 && i < n; i++) {  // note: if s2[i] == 0 the loop will stop as c1 != c2
    char c1 = ic_tolower(s1[i]);
    char c2 = ic_tolower(s2[i]);
    if (c1 < c2) return -1;
    if (c1 > c2) return 1;
  }
  return ((i >= n || s2[i] == 0) ? 0 : -1);
}

ic_private int ic_stricmp(const char* s1, const char* s2) {
  ssize_t len1 = ic_strlen(s1);
  ssize_t len2 = ic_strlen(s2);
  if (len1 < len2) return -1;
  if (len1 > len2) return 1;
  return (ic_strnicmp(s1, s2, (len1 >= len2 ? len1 : len2)));
}


static const char* ic_stristr(const char* s, const char* pat) {
  if (s==NULL) return NULL;
  if (pat==NULL || pat[0] == 0) return s;
  ssize_t patlen = ic_strlen(pat);
  for (ssize_t i = 0; s[i] != 0; i++) {
    if (ic_strnicmp(s + i, pat, patlen) == 0) return (s+i);
  }
  return NULL;
}

ic_private bool ic_contains(const char* big, const char* s) {
  if (big == NULL) return false;
  if (s == NULL) return true;
  return (strstr(big,s) != NULL);
}

ic_private bool ic_icontains(const char* big, const char* s) {
  if (big == NULL) return false;
  if (s == NULL) return true;
  return (ic_stristr(big,s) != NULL);
}


//-------------------------------------------------------------
// Unicode
// QUTF-8: See <https://github.com/koka-lang/koka/blob/master/kklib/include/kklib/string.h>
// Raw bytes are code points 0xEE000 - 0xEE0FF
//-------------------------------------------------------------
#define IC_UNICODE_RAW   ((unicode_t)(0xEE000U))

ic_private unicode_t unicode_from_raw(uint8_t c) {
  return (IC_UNICODE_RAW + c);
}

ic_private bool unicode_is_raw(unicode_t u, uint8_t* c) {
  if (u >= IC_UNICODE_RAW && u <= IC_UNICODE_RAW + 0xFF) {
    *c = (uint8_t)(u - IC_UNICODE_RAW);
    return true;
  }
  else {
    return false;
  }
}

ic_private void unicode_to_qutf8(unicode_t u, uint8_t buf[5]) {
  memset(buf, 0, 5);
  if (u <= 0x7F) {
    buf[0] = (uint8_t)u;
  }
  else if (u <= 0x07FF) {
    buf[0] = (0xC0 | ((uint8_t)(u >> 6)));
    buf[1] = (0x80 | (((uint8_t)u) & 0x3F));
  }
  else if (u <= 0xFFFF) {
    buf[0] = (0xE0 |  ((uint8_t)(u >> 12)));
    buf[1] = (0x80 | (((uint8_t)(u >>  6)) & 0x3F));
    buf[2] = (0x80 | (((uint8_t)u) & 0x3F));
  }
  else if (u <= 0x10FFFF) {
    if (unicode_is_raw(u, &buf[0])) {
      buf[1] = 0;
    }
    else {
      buf[0] = (0xF0 |  ((uint8_t)(u >> 18)));
      buf[1] = (0x80 | (((uint8_t)(u >> 12)) & 0x3F));
      buf[2] = (0x80 | (((uint8_t)(u >>  6)) & 0x3F));
      buf[3] = (0x80 | (((uint8_t)u) & 0x3F));
    }
  }
}

// is this a utf8 continuation byte?
ic_private bool utf8_is_cont(uint8_t c) {
  return ((c & 0xC0) == 0x80);
}

ic_private unicode_t unicode_from_qutf8(const uint8_t* s, ssize_t len, ssize_t* count) {
  unicode_t c0 = 0;
  if (len <= 0 || s == NULL) {
    goto fail;
  }
  // 1 byte
  c0 = s[0];
  if (c0 <= 0x7F && len >= 1) {
    if (count != NULL) *count = 1;
    return c0; 
  }
  else if (c0 <= 0xC1) { // invalid continuation byte or invalid 0xC0, 0xC1
    goto fail;
  }
  // 2 bytes
  else if (c0 <= 0xDF && len >= 2 && utf8_is_cont(s[1])) { 
    if (count != NULL) *count = 2;
    return (((c0 & 0x1F) << 6) | (s[1] & 0x3F));
  }
  // 3 bytes: reject overlong and surrogate halves
  else if (len >= 3 && 
           ((c0 == 0xE0 && s[1] >= 0xA0 && s[1] <= 0xBF && utf8_is_cont(s[2])) ||
            (c0 >= 0xE1 && c0 <= 0xEC && utf8_is_cont(s[1]) && utf8_is_cont(s[2])) 
          ))
  {
    if (count != NULL) *count = 3;
    return (((c0 & 0x0F) << 12) | ((unicode_t)(s[1] & 0x3F) << 6) | (s[2] & 0x3F));
  }
  // 4 bytes: reject overlong
  else if (len >= 4 && 
           (((c0 == 0xF0 && s[1] >= 0x90 && s[1] <= 0xBF && utf8_is_cont(s[2]) && utf8_is_cont(s[3])) ||
            (c0 >= 0xF1 && c0 <= 0xF3 && utf8_is_cont(s[1]) && utf8_is_cont(s[2]) && utf8_is_cont(s[3])) ||
            (c0 == 0xF4 && s[1] >= 0x80 && s[1] <= 0x8F && utf8_is_cont(s[2]) && utf8_is_cont(s[3]))) 
          )) 
  {
    if (count != NULL) *count = 4;
    return (((c0 & 0x07) << 18) | ((unicode_t)(s[1] & 0x3F) << 12) | ((unicode_t)(s[2] & 0x3F) << 6) | (s[3] & 0x3F));
  }  
fail:
  if (count != NULL) *count = 1;
  return unicode_from_raw(s[0]);
}


//-------------------------------------------------------------
// Debug
//-------------------------------------------------------------

#if defined(IC_NO_DEBUG_MSG) 
// nothing
#elif !defined(IC_DEBUG_TO_FILE)
ic_private void debug_msg(const char* fmt, ...) {
  if (getenv("ISOCLINE_DEBUG")) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
  }
}
#else
ic_private void debug_msg(const char* fmt, ...) {
  static int debug_init;
  static const char* debug_fname = "isocline.debug.txt";
  // initialize?
  if (debug_init==0) {
    debug_init = -1;
    const char* rdebug = getenv("ISOCLINE_DEBUG");
    if (rdebug!=NULL && strcmp(rdebug,"1") == 0) {
      FILE* fdbg = fopen(debug_fname, "w");
      if (fdbg!=NULL) {
        debug_init = 1;
        fclose(fdbg);
      }
    }
  }
  if (debug_init <= 0) return;

  // write debug messages
  FILE* fdbg = fopen(debug_fname, "a");
  if (fdbg==NULL) return;
  va_list args;
  va_start(args, fmt);
  vfprintf(fdbg, fmt, args);
  fclose(fdbg);
  va_end(args);
}
#endif


//-------------------------------------------------------------
// Allocation
//-------------------------------------------------------------

ic_private void* mem_malloc(alloc_t* mem, ssize_t sz) {
  return mem->malloc(to_size_t(sz));
}

ic_private void* mem_zalloc(alloc_t* mem, ssize_t sz) {
  void* p = mem_malloc(mem, sz);
  if (p != NULL) memset(p, 0, to_size_t(sz));
  return p;
}

ic_private void* mem_realloc(alloc_t* mem, void* p, ssize_t newsz) {
  return mem->realloc(p, to_size_t(newsz));
}

ic_private void mem_free(alloc_t* mem, const void* p) {
  mem->free((void*)p);
}

ic_private char* mem_strdup(alloc_t* mem, const char* s) {
  if (s==NULL) return NULL;
  ssize_t n = ic_strlen(s);
  char* p = mem_malloc_tp_n(mem, char, n+1);
  if (p == NULL) return NULL;
  ic_memcpy(p, s, n+1);
  return p;
}

ic_private char* mem_strndup(alloc_t* mem, const char* s, ssize_t n) {
  if (s==NULL || n < 0) return NULL;
  char* p = mem_malloc_tp_n(mem, char, n+1);
  if (p == NULL) return NULL;
  ssize_t i;
  for (i = 0; i < n && s[i] != 0; i++) {
    p[i] = s[i];
  }
  assert(i <= n);
  p[i] = 0;
  return p;
}

#endif

//-------------------------------------------------------------
// includes
//-------------------------------------------------------------
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

// skipping dup #include "../include/isocline.h"
// skipping dup #include "common.h"
// skipping dup #include "env.h"


//-------------------------------------------------------------
// Readline
//-------------------------------------------------------------

static char*  ic_getline( alloc_t* mem );

ic_public char* ic_readline(const char* prompt_text) 
{
  ic_env_t* env = ic_get_env();
  if (env == NULL) return NULL;
  if (!env->noedit) {
    // terminal editing enabled
    return ic_editline(env, prompt_text);   // in editline.c
  } 
  else {
    // no editing capability (pipe, dumb terminal, etc)
    if (env->tty != NULL && env->term != NULL) {
      // if the terminal is not interactive, but we are reading from the tty (keyboard), we display a prompt
      term_start_raw(env->term);  // set utf8 mode on windows
      if (prompt_text != NULL) {
        term_write(env->term, prompt_text);
      }
      term_write(env->term, env->prompt_marker);    
      term_end_raw(env->term, false);
    }
    // read directly from stdin
    return ic_getline(env->mem);
  }
}


//-------------------------------------------------------------
// Read a line from the stdin stream if there is no editing 
// support (like from a pipe, file, or dumb terminal).
//-------------------------------------------------------------

static char* ic_getline(alloc_t* mem)
{  
  // read until eof or newline
  stringbuf_t* sb = sbuf_new(mem);
  int c;
  while (true) {
    c = fgetc(stdin);
    if (c==EOF || c=='\n') {
      break;
    }
    else {
      sbuf_append_char(sb, (char)c);
    }
  }
  return sbuf_free_dup(sb);
}


//-------------------------------------------------------------
// Formatted output
//-------------------------------------------------------------


ic_public void ic_printf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  ic_vprintf(fmt, ap);
  va_end(ap);
}

ic_public void ic_vprintf(const char* fmt, va_list args) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->bbcode == NULL) return;
  bbcode_vprintf(env->bbcode, fmt, args);
}

ic_public void ic_print(const char* s) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->bbcode==NULL) return;
  bbcode_print(env->bbcode, s);
}

ic_public void ic_println(const char* s) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->bbcode==NULL) return;
  bbcode_println(env->bbcode, s);
}

void ic_style_def(const char* name, const char* fmt) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->bbcode==NULL) return;
  bbcode_style_def(env->bbcode, name, fmt);
}

void ic_style_open(const char* fmt) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->bbcode==NULL) return;
  bbcode_style_open(env->bbcode, fmt);
}

void ic_style_close(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->bbcode==NULL) return;
  bbcode_style_close(env->bbcode, NULL);
}


//-------------------------------------------------------------
// Interface
//-------------------------------------------------------------

ic_public bool ic_async_stop(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  if (env->tty==NULL) return false;
  return tty_async_stop(env->tty);
}

static void set_prompt_marker(ic_env_t* env, const char* prompt_marker, const char* cprompt_marker) {
  if (prompt_marker == NULL) prompt_marker = "> ";
  if (cprompt_marker == NULL) cprompt_marker = prompt_marker;
  mem_free(env->mem, env->prompt_marker);
  mem_free(env->mem, env->cprompt_marker);
  env->prompt_marker = mem_strdup(env->mem, prompt_marker);
  env->cprompt_marker = mem_strdup(env->mem, cprompt_marker);
}

ic_public const char* ic_get_prompt_marker(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return NULL;
  return env->prompt_marker;
}

ic_public const char* ic_get_continuation_prompt_marker(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return NULL;
  return env->cprompt_marker;
}

ic_public void ic_set_prompt_marker( const char* prompt_marker, const char* cprompt_marker ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  set_prompt_marker(env, prompt_marker, cprompt_marker);
}

ic_public bool ic_enable_multiline( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->singleline_only;
  env->singleline_only = !enable;
  return !prev;
}

ic_public bool ic_enable_beep( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  return term_enable_beep(env->term, enable);
}

ic_public bool ic_enable_color( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  return term_enable_color( env->term, enable );
}

ic_public bool ic_enable_history_duplicates( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  return history_enable_duplicates(env->history, enable);
}

ic_public void ic_set_history(const char* fname, long max_entries ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  history_load_from(env->history, fname, max_entries );
}

ic_public void ic_history_remove_last(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  history_remove_last(env->history);
}

ic_public void ic_history_add( const char* entry ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  history_push( env->history, entry );
}

ic_public void ic_history_clear(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  history_clear(env->history);
}

ic_public bool ic_enable_auto_tab( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->complete_autotab;
  env->complete_autotab = enable;
  return prev;
}

ic_public bool ic_enable_completion_preview( bool enable ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->complete_nopreview;
  env->complete_nopreview = !enable;
  return !prev;
}

ic_public bool ic_enable_multiline_indent(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->no_multiline_indent;
  env->no_multiline_indent = !enable;
  return !prev;
}

ic_public bool ic_enable_hint(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->no_hint;
  env->no_hint = !enable;
  return !prev;
}

ic_public long ic_set_hint_delay(long delay_ms) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  long prev = env->hint_delay;
  env->hint_delay = (delay_ms < 0 ? 0 : (delay_ms > 5000 ? 5000 : delay_ms));
  return prev;
}

ic_public void ic_set_tty_esc_delay(long initial_delay_ms, long followup_delay_ms ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  if (env->tty == NULL) return;
  tty_set_esc_delay(env->tty, initial_delay_ms, followup_delay_ms);
}


ic_public bool ic_enable_highlight(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->no_highlight;
  env->no_highlight = !enable;
  return !prev;
}

ic_public bool ic_enable_inline_help(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->no_help;
  env->no_help = !enable;
  return !prev;
}

ic_public bool ic_enable_brace_matching(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->no_bracematch;
  env->no_bracematch = !enable;
  return !prev;
}

ic_public void ic_set_matching_braces(const char* brace_pairs) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  mem_free(env->mem, env->match_braces);
  env->match_braces = NULL;
  if (brace_pairs != NULL) {
    ssize_t len = ic_strlen(brace_pairs);
    if (len > 0 && (len % 2) == 0) {
      env->match_braces = mem_strdup(env->mem, brace_pairs);
    }
  }
}

ic_public bool ic_enable_brace_insertion(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return false;
  bool prev = env->no_autobrace;
  env->no_autobrace = !enable;
  return !prev;
}

ic_public void ic_set_insertion_braces(const char* brace_pairs) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  mem_free(env->mem, env->auto_braces);
  env->auto_braces = NULL;
  if (brace_pairs != NULL) {
    ssize_t len = ic_strlen(brace_pairs);
    if (len > 0 && (len % 2) == 0) { 
      env->auto_braces = mem_strdup(env->mem, brace_pairs);
    }
  }
}

ic_private const char* ic_env_get_match_braces(ic_env_t* env) {
  return (env->match_braces == NULL ? "()[]{}" : env->match_braces);
}

ic_private const char* ic_env_get_auto_braces(ic_env_t* env) {
  return (env->auto_braces == NULL ? "()[]{}\"\"''" : env->auto_braces);
}

ic_public void ic_set_default_highlighter(ic_highlight_fun_t* highlighter, void* arg) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  env->highlighter = highlighter;
  env->highlighter_arg = arg;
}


ic_public void ic_free( void* p ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  mem_free(env->mem, p);
}

ic_public void* ic_malloc(size_t sz) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return NULL;
  return mem_malloc(env->mem, to_ssize_t(sz));
}

ic_public const char* ic_strdup( const char* s ) {
  if (s==NULL) return NULL;
  ic_env_t* env = ic_get_env(); if (env==NULL) return NULL;
  ssize_t len = ic_strlen(s);
  char* p = mem_malloc_tp_n( env->mem, char, len + 1 );
  if (p == NULL) return NULL;
  ic_memcpy( p, s, len );
  p[len] = 0;
  return p;
}

//-------------------------------------------------------------
// Terminal
//-------------------------------------------------------------

ic_public void ic_term_init(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  if (env->term==NULL) return;
  term_start_raw(env->term);
}

ic_public void ic_term_done(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  if (env->term==NULL) return;
  term_end_raw(env->term,false);
}

ic_public void ic_term_flush(void) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  if (env->term==NULL) return;
  term_flush(env->term);
}

ic_public void ic_term_write(const char* s) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  if (env->term == NULL) return;
  term_write(env->term, s);
}

ic_public void ic_term_writeln(const char* s) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  if (env->term == NULL) return;
  term_writeln(env->term, s);
}

ic_public void ic_term_writef(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  ic_term_vwritef(fmt, ap);
  va_end(ap);
}

ic_public void ic_term_vwritef(const char* fmt, va_list args) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  if (env->term == NULL) return;
  term_vwritef(env->term, fmt, args);
}

ic_public void ic_term_reset( void )  {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  if (env->term == NULL) return;
  term_attr_reset(env->term);
}

ic_public void ic_term_style( const char* style ) {
  ic_env_t* env = ic_get_env(); if (env==NULL) return;
  if (env->term == NULL || env->bbcode == NULL) return;
  term_set_attr( env->term, bbcode_style(env->bbcode, style));
}

ic_public int ic_term_get_color_bits(void) {
  ic_env_t* env = ic_get_env(); 
  if (env==NULL || env->term==NULL) return 4;  
  return term_get_color_bits(env->term);
}

ic_public void ic_term_bold(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->term==NULL) return;
  term_bold(env->term, enable);
}

ic_public void ic_term_underline(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->term==NULL) return;
  term_underline(env->term, enable);
}

ic_public void ic_term_italic(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->term==NULL) return;
  term_italic(env->term, enable);
}

ic_public void ic_term_reverse(bool enable) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->term==NULL) return;
  term_reverse(env->term, enable);
}

ic_public void ic_term_color_ansi(bool foreground, int ansi_color) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->term==NULL) return;
  ic_color_t color = color_from_ansi256(ansi_color);
  if (foreground) { term_color(env->term, color); }
             else { term_bgcolor(env->term, color); }
}

ic_public void ic_term_color_rgb(bool foreground, uint32_t hcolor) {
  ic_env_t* env = ic_get_env(); if (env==NULL || env->term==NULL) return;
  ic_color_t color = ic_rgb(hcolor);
  if (foreground) { term_color(env->term, color); }
             else { term_bgcolor(env->term, color); }
}


//-------------------------------------------------------------
// Readline with temporary completer and highlighter
//-------------------------------------------------------------

ic_public char* ic_readline_ex(const char* prompt_text,
                                ic_completer_fun_t* completer, void* completer_arg,
                                 ic_highlight_fun_t* highlighter, void* highlighter_arg )
{
  ic_env_t* env = ic_get_env(); if (env == NULL) return NULL;
  // save previous
  ic_completer_fun_t* prev_completer;
  void* prev_completer_arg;
  completions_get_completer(env->completions, &prev_completer, &prev_completer_arg);
  ic_highlight_fun_t* prev_highlighter = env->highlighter;
  void* prev_highlighter_arg = env->highlighter_arg;
  // call with current
  if (completer != NULL)   { ic_set_default_completer(completer, completer_arg); }
  if (highlighter != NULL) { ic_set_default_highlighter(highlighter, highlighter_arg); }
  char* res = ic_readline(prompt_text);
  // restore previous
  ic_set_default_completer(prev_completer, prev_completer_arg);
  ic_set_default_highlighter(prev_highlighter, prev_highlighter_arg);
  return res;
}


//-------------------------------------------------------------
// Initialize
//-------------------------------------------------------------

static void ic_atexit(void);

static void ic_env_free(ic_env_t* env) {
  if (env == NULL) return;
  history_save(env->history);
  history_free(env->history);
  completions_free(env->completions);
  bbcode_free(env->bbcode);
  term_free(env->term);
  tty_free(env->tty);
  mem_free(env->mem, env->cprompt_marker);
  mem_free(env->mem,env->prompt_marker);
  mem_free(env->mem, env->match_braces);
  mem_free(env->mem, env->auto_braces);
  env->prompt_marker = NULL;
  
  // and deallocate ourselves
  alloc_t* mem = env->mem;  
  mem_free(mem, env);

  // and finally the custom memory allocation structure
  mem_free(mem, mem);
}


static ic_env_t* ic_env_create( ic_malloc_fun_t* _malloc, ic_realloc_fun_t* _realloc, ic_free_fun_t* _free )  
{
  if (_malloc == NULL)  _malloc = &malloc;
  if (_realloc == NULL) _realloc = &realloc;
  if (_free == NULL)    _free = &free;
  // allocate
  alloc_t* mem = (alloc_t*)_malloc(sizeof(alloc_t));
  if (mem == NULL) return NULL;
  mem->malloc = _malloc;
  mem->realloc = _realloc;
  mem->free = _free;
  ic_env_t* env = mem_zalloc_tp(mem, ic_env_t);
  if (env==NULL) {
    mem->free(mem);
    return NULL;
  }
  env->mem = mem;

  // Initialize
  env->tty         = tty_new(env->mem, -1);  // can return NULL
  env->term        = term_new(env->mem, env->tty, false, false, -1 );  
  env->history     = history_new(env->mem);
  env->completions = completions_new(env->mem);
  env->bbcode      = bbcode_new(env->mem, env->term);
  env->hint_delay  = 400;   
  
  if (env->tty == NULL || env->term==NULL ||
      env->completions == NULL || env->history == NULL || env->bbcode == NULL ||
      !term_is_interactive(env->term)) 
  {
    env->noedit = true;
  }
  env->multiline_eol = '\\';
  
  bbcode_style_def(env->bbcode, "ic-prompt",    "ansi-green" );
  bbcode_style_def(env->bbcode, "ic-info",      "ansi-darkgray" );
  bbcode_style_def(env->bbcode, "ic-diminish",  "ansi-lightgray" );
  bbcode_style_def(env->bbcode, "ic-emphasis",  "#ffffd7" );
  bbcode_style_def(env->bbcode, "ic-hint",      "ansi-darkgray" );
  bbcode_style_def(env->bbcode, "ic-error",     "#d70000" );
  bbcode_style_def(env->bbcode, "ic-bracematch","ansi-white"); //  color = #F7DC6F" );

  bbcode_style_def(env->bbcode, "keyword",  "#569cd6" );
  bbcode_style_def(env->bbcode, "control",  "#c586c0" );
  bbcode_style_def(env->bbcode, "number",   "#b5cea8" );
  bbcode_style_def(env->bbcode, "string",   "#ce9178" );
  bbcode_style_def(env->bbcode, "comment",  "#6A9955" );
  bbcode_style_def(env->bbcode, "type",     "darkcyan" );
  bbcode_style_def(env->bbcode, "constant", "#569cd6" );

  set_prompt_marker(env, NULL, NULL);
  return env;
}

static ic_env_t* rpenv;

static void ic_atexit(void) {
  if (rpenv != NULL) {
    ic_env_free(rpenv);
    rpenv = NULL;
  }
}

ic_private ic_env_t* ic_get_env(void) {  
  if (rpenv==NULL) {
    rpenv = ic_env_create( NULL, NULL, NULL );
    if (rpenv != NULL) { atexit( &ic_atexit ); }
  }
  return rpenv;
}

ic_public void ic_init_custom_malloc( ic_malloc_fun_t* _malloc, ic_realloc_fun_t* _realloc, ic_free_fun_t* _free ) {
  assert(rpenv == NULL);
  if (rpenv != NULL) {
    ic_env_free(rpenv);    
    rpenv = ic_env_create( _malloc, _realloc, _free ); 
  }
  else {
    rpenv = ic_env_create( _malloc, _realloc, _free ); 
    if (rpenv != NULL) {
      atexit( &ic_atexit );
    }
  }
}

