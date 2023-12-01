Last update: 2023-11-23 18:00 PST

## Known bugs/limitations in `wile`

* set-car! & set-cdr! don't change the right copy of data in some cases

* Tail-calls are working except through (apply); can't get c compilers
  to do that one. Might not be able to fix this.

* No true bignums

* Need to work on macro expansion, interaction with (load) is not
  quite the way it should be

* Link issue on openbsd: library is supposed to be split so that at
  link time only required functions are pulled in -> keep things small
  and more secure too. On openbsd, this is not working somehow,
  executables are huge; however, they seem to work just as well, so
  not a major issue.

* Openbsd claims to have backtrace(), but link is not finding it?
