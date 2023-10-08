Last update: 2023-10-10 22:15 PST

# `wile` -- the extremely stable scheming genius compiler

`wile` is a small scheme-to-c compiler which I'm writing; it's by no
means complete, but it's capable enough that writing small to medium
programs with it is starting to be pretty pleasant. `wile` **is
self-hosting**. (I did make one mistaken claim in the previous version
of this README: I said `wile` could compile a number of files in the
`library/` subdirectory; that's wrong, those files are full of
user-defined macros, which is the major area where `wile` is still
incomplete. My bad.)

My design philosophy for it is the unix way of small simple tools; I
have tried to keep it simple and self-contained, with minimal
dependencies: in a minimal version, I want it to be usable without any
requirements beyond a reasonably-modern C compiler (although I do rely
on the [Boehm garbage collector](https://www.hboehm.info/gc); without
that, you'll be limited to very small programs).

This release of it is still very incomplete: batteries *not* included,
some assembly required! This is alpha software (mainly because not
implemented yet, less because bugs). See below for a roadmap.

The name `wile` is, of course, the name of that extremely stable
super-genius schemer, Wile E. Coyote. 'nuff said

## Table of contents

- [License](#license)
- [Short installation](#short-installation)
- [Limitation and bugs](#limitations-and-bugs)
- [Roadmap for future work](#roadmap)
- [What `wile` already has](#what-wile-already-has)
- [Installation gory details](#configuration-and-installation-details)
- [How to run `wile`](#howto-run-wile)
- [Guide to compiler files](#guide-to-the-files)
- [Contact me](#contact-me)

## License

`wile` is released under GPLv3 or later. Its runtime library is released
under LGPLv3 or later; linking your code with the runtime library will
not cause your code to become GPL'd. You retain copyright of your
scheme source code and may keep your code as open or closed as you
desire.

## Short installation

The tl;dr you set the environment variable CC to your c compiler, for
example `export CC=gcc`, then run `cd bootstrap && ./boot1.sh` which
should build a very minimal `wilec` to play with. Then edit the file
`setup.env` in the `bootstrap/` directory to specify the right location
for the `wile` home, source that file, and you should be able to play.

If you are feeling slightly more ambitious and want a slightly
better-configured compiler, read the section below, edit the
`bootstrap.sh` file in the main directory according to taste, still
set CC as above, and run that `bootstrap.sh`. There is also a
`setup.env` in the `wile` main directory.

If you get link errors like `undefined reference to
wile_config_real_QD_int_I128_gc_Y_sqlite_Y`, make sure the
configuration with which you built the runtime library is the same as
the one with which you compiled and linked your main program. If there
are differences, things go crazy quickly, and this is how `wile` attempts
to detect and prevent these crazinesses.

## Limitations and Bugs

### Stuff that's missing:

* `wile` can't compile user-defined macros, which are user-defined code
  that runs at compile time: it doesn't have an interpreter built in.
  ~~This is why it can't compile itself yet.~~ In order to get around
  this and make `wile` self-hosting, I took the few macros that `wile`
  defines and stuck them as built-in macros into its list of primitives.

* Closures are incomplete: closures which obey stack discipline are
  ok, but closures which have indefinite lifetime beyond where they
  were defined are not implmented yet.

* Continuations are in progress: a number of tests seem to work, but
  I'm not at all sure it's all there (the subject makes my head
  hurt). Sometimes code with continuations crashes.

* There is no high-level macro system yet. `wile` does have some
  low-level macros built in, `def-macro` style. Macros are not available
  in user programs yet.

* There is no unicode support; `wile` speaks only ASCII so far.

* I'm aiming at R*N*RS compliance, for suitably recent value of *N*;
  not there yet, but working on it.

* There is no automake/autoconf-style build+install system yet; I only
  have a couple of hacked-up shell scripts. See below for installation
  details.

* Tail recursion seems to be largely working, but since `wile` is a
  scheme-to-c compiler, it can be a bit tricky at times. We depend on
  the c compiler, and not every c compiler will do this correctly
  in all cases.

* There are no bignums. Recent versions of gcc and clang support
  128-bit integers, and I have support for that in `wile`, but no true
  unlimited-size integers.

* A few special forms are missing: `delay`, `force`.

* There are huge amounts of documentation still to be written.

* Error messages need significant improvement

### Outright bugs:

* `set-car!` and `set-cdr!` are broken. The issue is that they need to
  mutate info that's stored in the heap, but instead they change local
  copies on the stack. Making everything live in the heap would be one
  solution to this, but that's a rather drastic change.

* There are some issues with how included files interact with macro
  expansion; this might not be an actual bug, but it's at best a
  significant infelicity.

* Continuations may work, but there are a couple of cases where I see
  segfaults.

* I think I've got the majority of numerical functions able to deal
  with any style of number (int, rational, real, complex where
  relevant) as input, but there may be some cases I've missed.

## Roadmap

* Fix all the bugs

* Implement all the stuff that's missing

* Work on standards compliance

* Enable a real "link against library" mode: part of it is already there,
  need an equivalent of wrtl.sch for other libraries, but it seems pretty
  straightforward

* Make `wile` optimize better. Right now, I'm writing c output
  directly. I plan to generate output in some intermediate
  representation that's more amenable to optimization, then start
  doing those optimizations. (Allow me to insert a plug here for the
  excellent advanced compilers course
  [CS6120](https://www.cs.cornell.edu/courses/cs6120/2020fa/self-guided/)
  taught at Cornell by [Adrian
  Sampson](https://www.cs.cornell.edu/~asampson/); a bunch of this is
  what I intend to pursue.)

* Add a decent foreign-function interface: I'd like to be able to call
  large parts of `FFTW` or `LAPACK` etc from `wile` programs, without having
  to hand-craft every single interface.

## What `wile` already has

* The usual special forms: `and` `begin` `case` `cond` `define`
  `define-primitive` `define-alias` `do` `guard` `if` `lambda` `let`
  `let*` `letrec` `letrec*` `or` `quasiquote` `unquote`
  `unquote-splicing` `quote` `set!`

* A reasonably good number of functions in the standard library:
  `*` `+` `-` `/` `/=` `<` `<=` `=` `>` `>=` `UTCtime` `_cmplx?`
  `_int->cmplx_` `_int->rat_` `_int->real_` `_int?` `_rat->cmplx_`
  `_rat->real_` `_rat?` `_real->cmplx_` `_real?` `abs` `accept` `acos`
  `acosh` `agm` `all-true?` `angle` `any-true?` `append` `apply`
  `arithmetic-geometric-mean` `asin` `asinh` `assp` `assv` `atan`
  `atanh` `begin-breakable` `bessel-j` `bessel-y` `bits-and`
  `bits-clear` `bits-flip` `bits-get` `bits-not` `bits-or` `bits-set`
  `bits-set?` `bits-shift` `bits-xor` `boolean?` `bytevector`
  `bytevector->string` `bytevector-create` `bytevector-length`
  `bytevector-ref` `bytevector-set!` `bytevector-swap!` `bytevector?`
  `c*` `c+` `c-` `c/` `caaaar` `caaadr` `caaar` `caadar` `caaddr`
  `caadr` `caar` `cadaar` `cadadr` `cadar` `caddar` `cadddddddr`
  `caddddddr` `cadddddr` `caddddr` `cadddr` `caddr` `cadr` `call/cc`
  `car` `cbrt` `cconj` `cdaaar` `cdaadr` `cdaar` `cdadar` `cdaddr`
  `cdadr` `cdar` `cddaar` `cddadr` `cddar` `cdddar` `cddddr` `cdddr`
  `cddr` `cdr` `ceiling` `ceiling-quotient` `ceiling-remainder`
  `ceiling/` `cfft-good-n?` `change-root-directory` `char->integer`
  `char->string` `char-alphabetic?` `char-alphanumeric?` `char-ci/=?`
  `char-ci<=?` `char-ci<?` `char-ci=?` `char-ci>=?` `char-ci>?`
  `char-control?` `char-downcase` `char-hex-digit?` `char-lower-case?`
  `char-lowercase?` `char-numeric?` `char-oct-digit?`
  `char-printable?` `char-upcase` `char-upper-case?` `char-uppercase?`
  `char-whitespace?` `char/=?` `char<=?` `char<?` `char=?` `char>=?`
  `char>?` `char?` `cholesky-decompose` `cholesky-solve` `cimag`
  `close-port` `cmplx` `complex-conjugate` `complex?` `conj`
  `connect-to` `cons` `continuation?` `cos` `cosh` `cosine-integral`
  `cputime` `creal` `create-directory` `create-link`
  `create-symbolic-link` `cxr` `day-of-week` `day-of-year`
  `delta-dates` `denominator` `describe-system-error` `digamma`
  `directory-exists?` `display` `display-object-hook`
  `display-stack-trace` `do-until` `do-while` `elliptic-E`
  `elliptic-K` `emergency-exit` `epochtime` `eqv?` `erfc` `eval`
  `even?` `exit` `exp` `expmod` `expt` `factorial` `file-executable?`
  `file-exists?` `file-port?` `file-readable?` `file-writable?`
  `filter` `finite?` `flatten` `float` `floor` `floor-quotient`
  `floor-remainder` `floor/` `fluid-let` `flush-port` `fmod` `foldl`
  `foldl1` `foldr` `for-each` `fork-process` `fprintf` `frexp`
  `fromto` `gcd` `gensym` `get-current-directory` `get-domain-name`
  `get-effective-group-id` `get-effective-user-id`
  `get-environment-variable` `get-errno` `get-file-position`
  `get-file-status` `get-group-id` `get-host-name`
  `get-parent-process-id` `get-process-id` `get-session-id`
  `get-symbolic-link-status` `get-user-id` `get-user-information`
  `gregorian-date` `hypot` `i*` `i+` `i++` `i-` `i--` `i/` `ilog`
  `imag-part` `infinite?` `integer` `integer->char` `integer?`
  `is-block-device?` `is-char-device?` `is-directory?` `is-leap-year?`
  `is-named-pipe?` `is-regular-file?` `is-socket?` `is-symbolic-link?`
  `julian-day` `julian-day-of-easter` `lambert-W` `lambert-W+`
  `lambert-W-` `last` `lcm` `ldexp` `length` `list` `list->bytevector`
  `list->string` `list->vector` `list-append` `list-drop-while`
  `list-filter` `list-flatten` `list-head` `list-last` `list-length`
  `list-partition` `list-ref` `list-remove-dups` `list-reverse`
  `list-sort` `list-tail` `list-take-while` `list-unhead`
  `list-untail` `list?` `listen-on` `load-library` `localtime` `log`
  `log-gamma` `magnitude` `make-bytevector` `make-polar`
  `make-rational` `make-rectangular` `make-string` `make-vector` `map`
  `map1` `max` `max/i` `max/q` `max/r` `memp` `memv` `min` `min/i`
  `min/q` `min/r` `modulo` `namespace` `nan?` `negative` `negative?`
  `newline` `not` `null?` `number->string` `number/type` `number?`
  `numerator` `odd?` `offset-date` `open-file` `open-temporary-file`
  `pair?` `parse-file` `parse-string` `partition` `phase` `pipe-port?`
  `poly-chebyshev1` `poly-chebyshev2` `poly-hermite1` `poly-hermite2`
  `poly-laguerre` `poly-legendre` `port?` `positive?` `printf`
  `procedure?` `promise?` `promote/cmplx` `promote/rat` `promote/real`
  `q*` `q+` `q-` `q/` `quot-rem` `quotient` `r*` `r+` `r-` `r/`
  `raise` `random-exponential` `random-normal-pair`
  `random-permutation` `random-poisson` `random-seed!`
  `random-uniform` `rational?` `read-all` `read-char` `read-directory`
  `read-line` `real-part` `real?` `reciprocal` `regex-match`
  `remainder` `remove-directory` `remove-file` `rename-directory`
  `rename-file` `replicate` `reverse` `round` `run-command`
  `run-read-command` `run-write-command` `send-signal` `set-car!`
  `set-cdr!` `set-current-directory` `set-effective-group-id`
  `set-effective-user-id` `set-environment-variable` `set-errno!`
  `set-file-position` `set-group-id` `set-line-buffering!`
  `set-no-buffering!` `set-session-id` `set-user-id` `sign` `sin`
  `sine-integral` `sinh` `sleep` `socket-port?` `sprintf`
  `sqlite-close` `sqlite-dump-table` `sqlite-meta-schema`
  `sqlite-meta-tables` `sqlite-open` `sqlite-port?` `sqlite-run`
  `sqlite-statement-bind` `sqlite-statement-cleanup`
  `sqlite-statement-info` `sqlite-statement-prepare`
  `sqlite-statement-run` `sqlite-statement?` `sqlite-version` `sqrt`
  `stack-trace` `stack-trace-minimal` `string->char` `string->list`
  `string->number` `string->symbol` `string-append`
  `string-ci-hash-32` `string-ci-hash-64` `string-ci/=?`
  `string-ci<=?` `string-ci<?` `string-ci=?` `string-ci>=?`
  `string-ci>?` `string-copy` `string-create` `string-downcase`
  `string-find-first-char` `string-find-last-char` `string-hash-32`
  `string-hash-64` `string-join-by` `string-length`
  `string-pad-center` `string-pad-left` `string-pad-right`
  `string-port?` `string-ref` `string-reverse` `string-set!`
  `string-split-by` `string-split-by-whitespace` `string-trim`
  `string-trim-left` `string-trim-right` `string-upcase` `string/=?`
  `string<=?` `string<?` `string=?` `string>=?` `string>?` `string?`
  `substring` `symbol->string` `symbol=?` `symbol?` `tan` `tanh`
  `token-source-line` `truncate` `truncate-file` `truncate-quotient`
  `truncate-remainder` `truncate/` `type-of` `unless`
  `unset-environment-variable` `until` `upfrom` `vector`
  `vector->list` `vector-capacity` `vector-cfft!` `vector-create`
  `vector-fill!` `vector-for-each` `vector-length` `vector-map`
  `vector-map!` `vector-number/type` `vector-promote/cmplx!`
  `vector-promote/rat!` `vector-promote/real!` `vector-ref`
  `vector-set!` `vector-sort!` `vector-swap!` `vector?` `when` `while`
  `wile-basic-build-info` `wile-build-info` `write-1str` `write-char`
  `write-string` `zero?`

* A number of other libraries, in the `library/` subdirectory:

- `arg-parse.scm`
- `deque.scm`
- `hash.scm`
- `stack.scm`
- `stats.scm`

* A modest set of test programs in `wtest/` which can serve as a
  starting point for exploration, plus a couple of programs in the
  main directory which are part of the build itself.

* A couple of example programs in `examples/`:

- `cal.scm` is a re-implementation of part of the standard unix command
  `cal` to display the current calendar. I needed that at one point
  and had not installed it on my laptop, so rather than get it from
  the distro I took it as a challenge to re-implement the bits I wanted.

- `net-server.scm` is a teeny-tiny very cheesy way to serve up the
  system's notion of the current time across the network - a
  cargo-cult implementation of `ntp` (but don't use this for anything
  requiring real precision!)

- `net-client.scm` is the client corresponding to the above server.

## Configuration and installation details

If you go for the more ambitious build described above, you have to
decide how to configure `wile`. If you read the stuff below and your
eyes start to bleed, just go back up to the tl;dr above.

* Use Boehm garbage collector, or not. This is not a real choice; you
  need this, except if you're only playing. Real programs will suck
  down goo-gobs of memory without garbage collection, and will bring
  almost any system to its knees quickly. The Boehm garbage collector
  is located [here](https://www.hboehm.info/gc), or you can probably
  install it using your system's package manager. Add "-DWILE_USES_GC"
  to WILE_CONFIG, and also add "gc" to WILE_LINK_LIBRARIES.

* Use sqlite, or not. If you are dealing with sqlite databases, this
  is very useful. Sqlite is located [here](https://www.sqlite.org/),
  and again you can probably install it using your system's package
  manager. But you can safely skip this if you don't speak SQL. If you
  want this, add "-DWILE_USES_SQLITE" to WILE_CONFIG, and also add
  "sqlite3" to WILE_LINK_LIBRARIES.

* Decide on what kind of floating-point numbers you want to use: you
  can select 64-bit regular `double`, or `long double` which may be
  64-bit or 80-bit depending on your hardware, or `__float128` aka
  quad-double which are software-emulated 128-bit. The first two are
  natively supported by most modern c compilers, the last requires the
  `quadmath` library from gcc, or again package manager.

  - To use 128-bit floats, add "-DWILE_USES_QUAD_DOUBLE" to WILE_CONFIG,
    and also add "quadmath" to WILE_LINK_LIBRARIES.

  - To use `long double`, add "-DWILE_USES_LONG_DOUBLE" to WILE_CONFIG.

  - To use `double`, add "-DWILE_USES_DOUBLE" to WILE_CONFIG.

  - *NOT IMPLEMENTED*, but would probably be pretty easy, would be
    "-DWILE_USES_FLOAT" for C-native "float" which is generally 32-bit

* Decide on what kind of integers you want to use: you can select
  (usually) 64-bit `long int`, or 128-bit `__int128` which recent
  versions of both `gcc` and `clang` support. Add
  "-DWILE_USES_LONG_INT" for `long int` or "-DWILE_USES_INT128" for
  `__int128`.

After you've made your selections, the environment variables should
look like this:

* `WILE_CONFIG="-DWILE_USES_GC -DWILE_USES_SQLITE -DWILE_USES_LONG_INT
  -DWILE_USES_DOUBLE"`
* `WILE_LINK_LIBRARIES=sqlite3:gc`

Finally, you need to set `WILE_INCLUDE_DIRECTORIES` to point to the
locations where the sqlite3, gc, and quadmath header files are
located, assuming they aren't in fully-standard system-ish
locations; and similarly set `WILE_LINK_DIRECTORIES` to point to the
locations where the sqlite3, gc, and quadmath libraries are located.
My configuration is

- `WILE_INCLUDE_DIRECTORIES=.:$WHOME:$HOME/tools/include`
- `WILE_LINK_DIRECTORIES=.:$WHOME:$HOME/tools/lib`

After you've run `bootstrap.sh`, it should have created `wilec1` and
`wilec2` which should be identical.

I don't have a clean post-build install procedure yet, it's best to
leave `libwrtl.a`, `wrtl.sch`, and all header files in place. The
`wilec1` executable can be moved to some `bin` directory in your
path, and renamed `wile`. I still have some cleanup to do in this area.

## Howto run `wile`

To start with, you need to set up the above environment variables in
the environment where you'll run `wile`. You can do that by sourcing
the file `setup.env` from the main directory. Then try `wile -h` for a
tiny usage message, which directs you to try `wile -help`. That list
the command-line flags that `wile` understands. Note that `wile` is
picky about filename extensions: scheme source files must end in
`.scm`, c files in `.c`, object files in `.o`. I may relax this at
some point, but for now it seems safer. `wile` also requires that the
input file comes before the output file on the command line, unlike
say `gcc`.

So here's a tiny scheme program, call it hello.scm:

`(write-string (if (even? (list-length command-line-arguments))
		  "saluton mondo" "coi munje") #\newline)`

Run `wile -x hello.scm hello`, then `hello`; it should say `saluton
mondo`. If you run it as `hello 1`, there is one command-line
argument, and it should say `coi munje` instead. And if that works,
you're off and running. (The greetings are "hello world" in Esperanto
and Lojban respectively... I think.)

## Guide to the files

The compiler is composed of three files:

- `wile-main.scm` which does all the nitty-gritty of dealing with
  command-line options, inferring output file from input if required, etc.

- `wile-comp.scm` which is the heart of the compiler: this deals with
  expressions, special forms, sequencing of outputs, etc.

- `wile-prims.scm` which is the compiler's view of the runtime library:
  a bunch of little tiny leaf functions and codelets that implement
  most of the (small) primitives.

The rest of the runtime library lives in a number of C and scheme files:

- `alloc.c` which is the lowest-level interface to memory allocation,
  either plain-vanilla `malloc` or the Boehm garbage collector version.

- `print.c` which encodes all the knowledge of how to print the different
  types of scheme values.

- `location.c` is a fairly small file which helps provide source file
  location info for tokens, and thus hopefully helps make better error
  messages.

- `wile-sql.c` contains most of the interface to `sqlite`.

- `swll-cfft.c` contains `wile`'s 1D complex Fourier transform routine.
  This should really be in some other library, not the standard runtime,
  but this sort of thing is kinda what I do a lot of, so there it is.

- `continuations.c` implements `call/cc`.

- `wile-parse.c` and `wile-lex.c` are the parser and scanner,
  respectively.  If you examine these closely, you'll see that they
  appear machine-generated; that is in fact correct. There are
  currently-unused `wile.yucc` and `wile.ulex` files which are the
  true sources. A long time ago, I got curious as to how `yacc` and `lex`
  work, and I wrote my own, `yucc` and `ulex`. I never got around to
  releasing those, which is why I use the generated files. Maybe
  someday I'll release those, or possibly just rewrite this back into
  standard `yacc` and `lex`.

- `fsi_set.c`, `nfa.c`, `regex.c`, and `ulexlib.c` are support files
  for the scanner. `regex.c` also implements the very simple
  regular-expression engine in `wile`.

- `wile-rtl1.c` contains a number of low-level runtime functions

- `wile-rtl2.scm` contains a fairly large number of higher-level scheme
  routines that implement large parts of the runtime library. Many
  of these are written in a fairly low-level style; that's because they
  came into existence before some of the fancier stuff in the compiler
  got written.

- `math-funcs.c` contains a number of special mathematical functions.

The last three of these files get chopped up when compiling the
runtime library, so that there are lots of small object files; this is
so that executables stay smaller and also more secure: if a particular
buggy function gets dragged in during linking even though it never
gets used, just because it lives in the same object file as some other
function that does get used, and if the executable gets used in
cracking a system somehow, buffer overruns or other hacks could get
used to reach the broken buggy function even though the normal flow of
control never will. But if it isn't linked in... it can't get reached.

## Contact me

If you have questions, comments, bug reports, patches, please send
email to me at <uhollerbach@gmail.com>. Please put "WILE" in the
subject line.

Enjoy! - Uwe Hollerbach
