// Wile -- the extremely stable scheming genius compiler
// Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


// (regex-match pattern string) => #f || (pre-match match post-match)

lval wile_regex_match(lptr* clos, lptr args, const char* loc)
{
    struct nfa_state* nstate;
    struct nfa_work* nwork;
    char *ms, *me, save;
    lptr sstr[2];
    lval res;

    if (args[0].vt != LV_STRING || args[1].vt != LV_STRING) {
	wile_exception("regex-match", loc, "expects two string arguments");
    }

    nstate = regex_parse(args[0].v.str, NULL, REGEX_OPTION_8BIT, 0);
    if (nstate == NULL) {
	wile_exception("regex-match", loc, "regular expression parse failed");
    }
    nwork = regex_wrap(nstate, REGEX_OPTION_8BIT);
    if (regex_match((unsigned char*) args[1].v.str, nwork,
		    (unsigned char**) &ms, (unsigned char**) &me)) {
	++me;
	save = *ms;
	*ms = '\0';
	sstr[0] = new_string(args[1].v.str);
	*ms = save;
	save = *me;
	*me = '\0';
	sstr[1] = new_string(ms);
	*me = save;
	sstr[1] = new_pair(sstr[1], new_pair(new_string(me), NULL));
	res = *(new_pair(sstr[0], sstr[1]));
    } else {
	res = LVI_BOOL(false);
    }

    regex_free(nwork);
    nfa_free(nstate);

    return res;
}

