#ifndef WILE_PARSE_H
#define WILE_PARSE_H

enum token {
    BOOLEAN = 256,
    BVECTOR = 257,
    CHARACTER = 258,
    COMPLEX = 259,
    HASHBANG = 260,
    INTEGER = 261,
    RATIONAL = 262,
    REAL = 263,
    STRING = 264,
    SYMBOL = 265,
    VECTOR = 266,
    EOI = 294
};
struct ulex_context;
unsigned int wile_parse(struct ulex_context* context,
			     void* user_data,
			     int do_actions);

#endif /* WILE_PARSE_H */
