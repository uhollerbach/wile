// Wile -- the extremely stable scheming genius compiler
// Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
// License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

#include "wile-rtl1.h"
#include "wile-rtl2.h"
#include "wile-lex.h"

extern lisp_escape_t cachalot;


static int tcp_proto = 0;

#define GET_PROTO(fname)						\
    do {								\
	if (tcp_proto == 0) {						\
	    struct protoent* pro = getprotobyname("tcp");		\
	    if (pro == NULL) {						\
		endprotoent();						\
		wile_exception((fname),					\
			       "can't find tcp protocol number! no networking?"); \
	    }								\
	    tcp_proto = pro->p_proto;					\
	    endprotoent();						\
	}								\
    } while (0)

lval wile_listen_port(lptr*, lptr args)
{
    int sd;
    FILE* fp;

    if (args[0].vt != LV_INT) {
	wile_exception("listen-on", "got a non-integer argument");
    }
    if (args[0].v.iv < 0 || args[0].v.iv > 65535) {
	wile_exception("listen-on", "got bad port number %lld",
		     (long long) args[0].v.iv);
    }
    GET_PROTO("listen-on");

    sd = socket(AF_INET, SOCK_STREAM, tcp_proto);
    if (sd < 0) {
	// DO_ERRNO();
	return LVI_BOOL(false);
    } else {
	struct sockaddr_in my_addr;

	memset(&my_addr, 0, sizeof(my_addr));
	my_addr.sin_family = AF_INET;
	my_addr.sin_addr.s_addr = htonl(INADDR_ANY);
	my_addr.sin_port = htons(args[0].v.iv);

	if (bind(sd, (struct sockaddr*) &my_addr, sizeof(my_addr)) < 0) {
	    // DO_ERRNO();
	    return LVI_BOOL(false);
	} else {
	    if (listen(sd, 16) < 0) {
		// DO_ERRNO();
		return LVI_BOOL(false);
	    } else {
		fp = fdopen(sd, "rb+");
		if (fp == NULL) {
		    // TODO: close sd? I think so
		    // DO_ERRNO();
		    return LVI_BOOL(false);
		} else {
		    setvbuf(fp, NULL, _IONBF, 0);
		    return LVI_SPORT(fp);
		}
	    }
	}
    }
}

lval wile_accept_connection(lptr*, lptr args)
{
    int fd;
    unsigned int psize;
    char buf[INET_ADDRSTRLEN+1];
    struct sockaddr_in peer;

    if (args[0].vt != LV_SOCK_PORT) {
	wile_exception("accept", "expects one socket-port argument");
    } else {
	memset(&peer, 0, sizeof(peer));
	psize = sizeof(peer);
	fd = accept(fileno(args->v.fp), (struct sockaddr*) &peer, &psize);
	if (fd < 0) {
	    // DO_ERRNO();
	    return LVI_BOOL(false);
	} else {
	    lval vs[3];

	    vs[0] = LVI_SPORT(fdopen(fd, "rb+"));
	    if (vs[0].v.fp == NULL) {
		vs[0] = LVI_BOOL(false);
		// DO_ERRNO();
	    } else {
		setvbuf(vs[0].v.fp, NULL, _IONBF, 0);
	    }
	    if (inet_ntop(AF_INET, &(peer.sin_addr), buf, sizeof(buf))) {
		vs[1] = LVI_STRING(buf);
	    } else {
		vs[1] = LVI_STRING("<unknown>");
		// DO_ERRNO();
	    }
	    vs[2] = LVI_INT(ntohs(peer.sin_port));
	    return gen_list(3, vs, NULL);
	}
    }
}

lval wile_connect_to(lptr*, lptr args)
{
    char pstr[8];
    struct addrinfo hints, *server, *sp;
    lval ret;
    int sd;

    if (args[0].vt != LV_STRING || args[1].vt != LV_INT) {
	wile_exception("connect-to",
		       "expects one string and one int argument");
    }
    if (args[1].v.iv < 0 || args[1].v.iv > 65535) {
	wile_exception("connect-to", "got bad port number %lld",
		       (long long) args[1].v.iv);
    }
    GET_PROTO("connect-to");

    snprintf(pstr, sizeof(pstr), "%d", (int) args[1].v.iv);

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;	// might as well be prepared for IPv6
    hints.ai_socktype = SOCK_STREAM;

    if (getaddrinfo(args[0].v.str, pstr, &hints, &server) != 0) {
	wile_exception("connect-to", "getaddrinfo failed");
    }

    sp = server;
    while (sp) {
	sd = socket(sp->ai_family, sp->ai_socktype, sp->ai_protocol);
	if (sd >= 0) {
	    if (connect(sd, sp->ai_addr, sp->ai_addrlen) < 0) {
		close(sd);
	    } else {
		// good
		break;
	    }
	}
	sp = sp->ai_next;
    }

    if (sp == NULL) {
	ret = LVI_BOOL(false);
    } else {
	ret = LVI_SPORT(fdopen(sd, "rb+"));
	if (ret.v.fp == NULL) {
	    ret = LVI_BOOL(false);
	    // DO_ERRNO();
	} else {
	    setvbuf(ret.v.fp, NULL, _IONBF, 0);
	}
    }
    freeaddrinfo(server);
    return ret;
}

