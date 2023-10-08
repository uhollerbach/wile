Last update: 2023-10-08 10:00 PST

## Known bugs/limitations in `wile`

* set-car! & set-cdr! don't change the right copy of data in some cases

* Tail-calls are working except through (apply); can't get c compilers
  to do that one. Might not be able to fix this.

* Closures are limited to those that don't escape the stack;
  others are unimplemented.

* `wile` can't compile definitions of new macros yet, which means it
  can't compile itself yet -> need to implement (eval).

* No true bignums

* Need to work on macro expansion, interaction with (load) is not
  quite the way it should be

* Link issue on openbsd: library is supposed to be split so that at
  link time only required functions are pulled in -> keep things small
  and more secure too. On openbsd, this is not working somehow,
  executables are huge; however, they seem to work just as well, so
  not a major issue.

* Openbsd claims to have backtrace(), but link is not finding it?

* Possible problem in continuations.c? "val = *(CAR(ap));" I don't have a
  test case for this, this is a "maybe".

* One test of continuations shows different behavior between wile and
  mit-scheme: wile does an infinite loop, whereas mit-scheme does one
  trip. I don't understand this, my cargo-cult understanding of
  continuations says wile is correct... but I'm really not willing to
  say mit-scheme is wrong?!?

* There are occasional segfaults after running some continuations test
  cases. Probably uninitialized memory somehow, but valgrind is
  inconclusive.
