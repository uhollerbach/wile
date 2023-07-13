# Wile -- the extremely stable scheming genius compiler
# Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
# License: GPLv3 or later, see file 'LICENSE' for details

SHELL = /bin/sh

ALL = ttest strtest aptest dqtest htest stktest statest

all:	$(ALL)

test:	$(ALL)
	ttest
	strtest
	aptest
	dqtest
	htest
	stktest
	statest

clean:
	rm -f $(ALL)

ttest:	test-test.scm test.scm
	wile -x test-test.scm ttest

strtest:	struct-test.scm struct.scm test.scm
	wile -x struct-test.scm strtest

aptest:		arg-parse-test.scm arg-parse.scm hash.scm test.scm
	wile -x arg-parse-test.scm aptest

dqtest:		deque-test.scm deque.scm struct.scm test.scm
	wile -x deque-test.scm dqtest

htest:		hash-test.scm hash.scm test.scm
	wile -x hash-test.scm htest

stktest:	stack-test.scm stack.scm struct.scm test.scm
	wile -x stack-test.scm stktest

statest:	stats-test.scm stats.scm test.scm
	wile -x stats-test.scm statest
