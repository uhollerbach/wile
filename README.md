Last update: 2024-1-1 23:00 PST

# `wile` - the extremely stable scheming genius compiler - version 1.2.1

`wile` is a small scheme-to-c compiler which I'm writing; it's by no
means complete, but it's capable enough that writing programs with it
is starting to be pretty pleasant. `wile` **is self-hosting**.
The name `wile` is, of course, the name of that extremely stable
super-genius schemer, Wile E. Coyote. 'nuff said

My design philosophy for it is the unix way of small simple tools; I
have tried to keep it simple and self-contained, with minimal
dependencies: in a minimal version, I want it to be usable without any
requirements beyond a reasonably-modern C compiler (although I do rely
on the [Boehm garbage collector](https://www.hboehm.info/gc); without
that, you'll be limited to small programs... that said, the
pre-autotools bootstrap stage0 compiler is configured to build without
GC, and it can recompile itself; that's not tiny.)

This release of it is starting to tend toward batteries-included.
The autotools-based build+install is looking pretty good; the files
and directories are still structured as a separate sub-tree, but
the required steps look a whole lot like the standard autotols
build: `cd autotools && configure && make && make install` produces
a working compiler, and that compiler can compile itself and
almost all of the tests: two tests are known failures, and one lists
the configuration, so that will differ from system to system.

Since Coverity offers free scans of open-source projects, I have
scanned `wile`, I intend to make it as clean as I can. There are two
remaining sets of flagged issues: first, Coverity claims (quite
correctly) that my use of `drand48()` is not cryptographically
secure. That's ok, I think, for the purposes for which I'm using
it. Second, there are a number of places where Coverity flags
"structurally dead" code. That too is correct, these are spots where
`wile` put in tail calls, and the dead-code eliminator, which is still
pretty stupid, failed to clean up all of the dead code that it should
have. But these are harmless. So overall it is quite clean already.

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

## Limitations and Bugs

### Stuff that's missing:

* There is no high-level macro system yet. `wile` does have some
  low-level macros built in, `def-macro` style.

* There is no unicode support; `wile` speaks only ASCII so far.

* I'm aiming at R*N*RS compliance, for suitably recent value of *N*;
  not there yet, but working on it.

* Tail recursion seems to be largely working, but since `wile` is a
  scheme-to-c compiler, it can be a bit tricky at times. We depend on
  the c compiler, and not every c compiler will do this correctly
  in all cases. In particular, there is a path through `(apply)` that
  should be a tail call, but I've been unable to persuade either `gcc`
  or `clang` to make that happen.

* There are no bignums. Recent versions of gcc and clang support
  128-bit integers, and I have support for that in `wile`, but no true
  unlimited-size integers.

* A few special forms are still missing: `delay`, `force`.

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

* A reasonably good number of functions in the standard library: `*`
  `+` `-` `/` `/=` `<` `<=` `=` `>` `>=` `UTCtime` `abs` `accept`
  `acos` `acosh` `agm` `all-true?` `angle` `any-true?` `append`
  `apply` `apply-interp` `apply-lambda` `arithmetic-geometric-mean`
  `asin` `asinh` `assp` `assv` `atan` `atanh` `begin-breakable`
  `begin-form?` `bessel-j` `bessel-y` `bits-and` `bits-clear`
  `bits-flip` `bits-get` `bits-not` `bits-or` `bits-set` `bits-set?`
  `bits-shift` `bits-xor` `boolean?` `bytevector` `bytevector->list`
  `bytevector->string` `bytevector-create` `bytevector-for-each`
  `bytevector-length` `bytevector-ref` `bytevector-set!`
  `bytevector-swap!` `bytevector?` `c*` `c+` `c-` `c/` `caaaar`
  `caaadr` `caaar` `caadar` `caaddr` `caadr` `caar` `cadaar` `cadadr`
  `cadar` `caddar` `cadddddddr` `caddddddr` `cadddddr` `caddddr`
  `cadddr` `caddr` `cadr` `call-with-current-continuation` `call/cc`
  `car` `case-lambic` `cbrt` `cconj` `cdaaar` `cdaadr` `cdaar`
  `cdadar` `cdaddr` `cdadr` `cdar` `cddaar` `cddadr` `cddar` `cdddar`
  `cddddr` `cdddr` `cddr` `cdr` `ceiling` `ceiling-quotient`
  `ceiling-remainder` `ceiling/` `cfft-good-n?` `change-file-owner`
  `change-root-directory` `change-symbolic-link-owner` `char->integer`
  `char->string` `char-alphabetic?` `char-alphanumeric?` `char-ci/=?`
  `char-ci<=?` `char-ci<?` `char-ci=?` `char-ci>=?` `char-ci>?`
  `char-control?` `char-downcase` `char-hex-digit?` `char-lower-case?`
  `char-lowercase?` `char-numeric?` `char-oct-digit?`
  `char-printable?` `char-upcase` `char-upper-case?` `char-uppercase?`
  `char-whitespace?` `char/=?` `char<=?` `char<?` `char=?` `char>=?`
  `char>?` `char?` `cholesky-decompose` `cholesky-solve` `cimag`
  `clear-file-error` `close-port` `cmplx` `compiled-procedure?`
  `complex-conjugate` `complex?` `compose` `conj` `connect-to` `cons`
  `continuation?` `cos` `cosh` `cosine-integral` `cputime` `creal`
  `create-directory` `create-link` `create-symbolic-link` `curry`
  `cxr` `day-of-week` `day-of-year` `def-struct` `define-form?`
  `delta-dates` `denominator` `describe-system-error` `digamma`
  `directory-exists?` `display` `display-object-hook`
  `display-stack-trace` `do-until` `do-while` `elliptic-E`
  `elliptic-K` `emergency-exit` `emit-code` `epochtime` `eqv?` `erfc`
  `eval` `eval-begin` `eval-define` `even?` `exit` `exp` `expmod`
  `expt` `factorial` `file-executable?` `file-exists?` `file-port?`
  `file-readable?` `file-writable?` `filter` `finite?` `flatten`
  `float` `floor` `floor-quotient` `floor-remainder` `floor/`
  `fluid-let` `flush-port` `fmod` `foldl` `foldl1` `foldr` `for-each`
  `fork-process` `fprintf` `frexp` `fromto` `gc-version` `gcd`
  `gensym` `get-current-directory` `get-domain-name`
  `get-effective-group-id` `get-effective-user-id`
  `get-environment-variable` `get-errno` `get-file-eof`
  `get-file-error` `get-file-position` `get-file-status`
  `get-group-id` `get-group-information` `get-host-name`
  `get-interpreted-procedure-argu` `get-interpreted-procedure-arit`
  `get-interpreted-procedure-body` `get-interpreted-procedure-envi`
  `get-interpreted-procedure-macr` `get-iproc-args` `get-iproc-arity`
  `get-iproc-body` `get-iproc-env` `get-iproc-macro`
  `get-parent-process-id` `get-process-id` `get-session-id`
  `get-symbolic-link-status` `get-user-id` `get-user-information`
  `gregorian-date` `hypot` `i*` `i+` `i++` `i-` `i--` `i/` `ilog`
  `imag-part` `infinite?` `integer` `integer->char` `integer?`
  `interpreted-procedure?` `is-block-device?` `is-char-device?`
  `is-directory?` `is-leap-year?` `is-named-pipe?` `is-prime?`
  `is-regular-file?` `is-socket?` `is-symbolic-link?` `julian-day`
  `julian-day-of-easter` `lambert-W` `lambert-W+` `lambert-W-` `last`
  `lcm` `ldexp` `length` `list` `list->bytevector` `list->string`
  `list->vector` `list-append` `list-drop-while` `list-filter`
  `list-flatten` `list-group-by` `list-head` `list-last` `list-length`
  `list-length<=?` `list-length<?` `list-length=?` `list-length>=?`
  `list-length>?` `list-partition` `list-ref` `list-remove-dups`
  `list-reverse` `list-sort` `list-tail` `list-take-while`
  `list-unhead` `list-untail` `list?` `listen-on` `load-file-path`
  `load-form?` `load-library` `localtime` `log` `log-gamma`
  `magnitude` `make-bytevector` `make-interpreted-procedure`
  `make-iproc` `make-polar` `make-rational` `make-rectangular`
  `make-string` `make-vector` `map` `max` `max/i` `max/q` `max/r`
  `memp` `memv` `min` `min/i` `min/q` `min/r` `modulo` `namespace`
  `nan?` `negative` `negative?` `newline` `next-prime` `not` `null?`
  `number->string` `number/type` `number?` `numerator` `odd?`
  `offset-date` `open-file` `open-temporary-file` `pair?` `parse-file`
  `parse-string` `partition` `phase` `pipe-port?` `poly-chebyshev1`
  `poly-chebyshev2` `poly-hermite1` `poly-hermite2` `poly-laguerre`
  `poly-legendre` `port?` `positive?` `printf` `procedure?` `promise?`
  `promote/cmplx` `promote/rat` `promote/real` `q*` `q+` `q-` `q/`
  `quot-rem` `quotient` `r*` `r+` `r-` `r/` `raise` `random-cauchy`
  `random-exponential` `random-normal-pair` `random-permutation`
  `random-poisson` `random-seed!` `random-uniform` `rational?`
  `read-all` `read-bytes` `read-char` `read-directory` `read-line`
  `real-part` `real?` `reciprocal` `regex-match` `remainder`
  `remove-directory` `remove-file` `rename-directory` `rename-file`
  `replicate` `reverse` `root-bisect` `root-bracket` `root-ridders`
  `round` `run-command` `run-read-command` `run-write-command`
  `send-signal` `set-car!` `set-cdr!` `set-current-directory`
  `set-effective-group-id` `set-effective-user-id`
  `set-environment-variable` `set-errno!` `set-file-position`
  `set-group-id` `set-interpreted-procedure-envi`
  `set-interpreted-procedure-macr` `set-iproc-env!` `set-iproc-macro!`
  `set-line-buffering!` `set-no-buffering!` `set-session-id`
  `set-user-id` `sha-256` `sha-256-init` `sha-256-update`
  `sha-256-finish` `sign` `sin` `sine-integral` `sinh` `sleep`
  `socket-port?` `sprintf` `sqlite-close` `sqlite-dump-table`
  `sqlite-meta-schema` `sqlite-meta-tables` `sqlite-open`
  `sqlite-port?` `sqlite-run` `sqlite-statement-bind`
  `sqlite-statement-cleanup` `sqlite-statement-info`
  `sqlite-statement-prepare` `sqlite-statement-run`
  `sqlite-statement?` `sqlite-version` `sqrt` `stack-trace`
  `stack-trace-minimal` `string->char` `string->list` `string->number`
  `string->symbol` `string-append` `string-ci-hash-32`
  `string-ci-hash-64` `string-ci/=?` `string-ci<=?` `string-ci<?`
  `string-ci=?` `string-ci>=?` `string-ci>?` `string-copy`
  `string-create` `string-downcase` `string-find-first-char`
  `string-find-last-char` `string-hash-32` `string-hash-64`
  `string-join-by` `string-length` `string-pad-center`
  `string-pad-left` `string-pad-right` `string-port?` `string-ref`
  `string-reverse` `string-set!` `string-split-by`
  `string-split-by-whitespace` `string-trim` `string-trim-left`
  `string-trim-right` `string-upcase` `string/=?` `string<=?`
  `string<?` `string=?` `string>=?` `string>?` `string?` `substring`
  `symbol->string` `symbol=?` `symbol?` `tan` `tanh`
  `token-source-line` `truncate` `truncate-file` `truncate-quotient`
  `truncate-remainder` `truncate/` `type-of` `unless`
  `unset-environment-variable` `until` `upfrom` `vector`
  `vector->list` `vector-capacity` `vector-cfft!` `vector-create`
  `vector-fill!` `vector-for-each` `vector-length` `vector-map`
  `vector-map!` `vector-number/type` `vector-promote/cmplx!`
  `vector-promote/rat!` `vector-promote/real!` `vector-ref`
  `vector-set!` `vector-sort!` `vector-swap!` `vector?` `wait-process`
  `when` `while` `wile-architecture-name` `wile-basic-build-info`
  `wile-build-info` `wile-environment-with-macros` `wile-os-name`
  `wile-standard-environment` `write-1str` `write-bytes` `write-char`
  `write-string` `zero?`

* A number of other libraries, in the `library/` subdirectory:

- `arg-parse.scm`
- `deque.scm`
- `gthread.scm`
- `hash.scm`
- `monad.scm` and `monad-list.scm`
- `stack.scm`
- `stats.scm`
- `struct.scm`
- `test.scm`

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

- `n-queens.scm` solves the N-queens problem, using the `monad` code
  in the library.

- In the `wtest/` directory, not in `examples/`, `test_53.scm` is a
  Runge-Kutta-based solver for the Volterra equations; in addition
  to testing first-class closures, this might be a useful example
  for an ODE solver.

- `gthread-test.scm` is a test of the very simple cooperative threads
  library, but already throws around a fair number of continuations.

## Configuration and installation details

Increasingly, the stuff I wrote here is obsolete. Instead, I'll briefly
describe what to do if you want to recompile the compiler itself. The
simplest way is to build the `build-stages` program in the top-level
directory, then in that directory, run it: it will rebuild the compiler
and libraries either two or three times, each time using the previous
build result. If the SHA-256 hashes of the corresponding stage1 and stage2
files all match, it will stop early, and if they don't match, it will
build stage3. Either after stage2 or after stage3, the SHA-256 hashes
should match; if they do not, I think there is something seriously wrong.

If you want to build cross-compiled or cross-configured programs, you
will need to do some manual tweaking of the installed files. Look for
the wile-config.dat file in the locations where the `wile` files got
installed, make a copy, and tweak it + build a copy of libwrtl.a with
the tweaked settings. If you try this, you will need to tell `wile` to
use the tweaked versions rather than the originals; you can set the
environment variable WILE_CONFIG_FILE or use the -CF command-line
option.

## Howto run `wile`

After you've built `wile` with the autotools dance, you need to either
put the directory where it's installed into your PATH if it's not
already, or you need to invoke it with the explicit full path; that
should be all.

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
subject line. You can also reach me on Mastodon, where I am
@UweHalfHand@norcal.social.

Enjoy! - Uwe Hollerbach
