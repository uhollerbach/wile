#ifndef WILE_LEX_H
#define WILE_LEX_H


#include "ulexlib.h"

unsigned int wile_lex(struct ulex_context*, YYSTYPE*,
			    void*, unsigned char**);


#endif /* WILE_LEX_H */
