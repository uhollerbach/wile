Last update: 2023-11-23 02:00 PST

# Changelog for `wile`

## Current version `wile` "1.0.1" 2023-11-29 23:45 PST

- make remove-unused-vars routine use bytevectors instead of hashes:
  this makes it run slightly faster and consuming slightly less memory,
  enough so that the non-GC stage0 bootstrap compiler can still use
  it to clean up its output (in 8 GB of memory)

- memoize wrappers around primitives, so that there are not multiple copies

- add a slot to the "standard" function call to pass caller location around
  and start tracking call location - big change that touches everything,
  hopefully no effect (except better error messages)

## Current version `wile` "1.0.0" 2023-11-23 02:00 PST

- add a few functions which rely on first-class closures
  curry, compose, list-group-by; add Miller-Rabin probabilistic
  primality test

- make continuations work?!? it seems so...

- change format of version to "N1.N2.N3" from (N1 N2 N3), the former
  is more-standard

## Current version `wile` (0 11 0) 2023-11-19 23:00 PST

- implement first-class closures

- move compile-with-output macro out of prims, make it a simple (defmacro)

- try to move emit-code out of prims, but revert that; it leads to
  instability, perhaps not of the executables themselves, but at least
  of the MD5 sums of the executables: something changes from build to
  build. until that's cleared up, keep this as a macro in prims.

- add root-finding routines to RTL, check a couple of inputs to primitives
  a bit more carefully

## Current version `wile` (0 10 2) 2023-11-11

- add a couple of primitives: read-bytes, write-bytes, no-args version
  of get-user-information, get-file-eof, get-file-error, clear-file-error,
  get-group-information

- harden a couple of SQL routines against injection attacks

- finish adding at least minimal non-empty doc-string for all primitives

## Current version `wile` (0 10 1) 2023-11-5

- add a few special cases mapN to make things a bit faster

- treat a few cases specially in interpreter, attempt to properly
  update environment - still in progress

- change a number of calls to system malloc to call GC_malloc instead;
  I believe this fixes my heisenbug!!!

## Current version `wile` (0 10 0) 2023-11-1

- implement 'do' special form in interpreter

- add literal strings to the stuff that can be immediately defined
  in library code, and use that to clean up an infelicity in printf

- move standard interpreter environment stuff into RTL; remove 'when
  'until 'while etc as special forms from interpreter code; add them as
  generic standard macros in interpreter environment; move interpreter
  code into RTL

- close the loop on applying interpreted functions to args... defmacro
  seems to work ok in compiler

## Current version `wile` (0 9 2) 2023-10-27

- implement something very like case-lambda, for convenient wrapping
  of the ambiguous primitives - can't use it inside the prims-table,
  since those internal lambdas include one more argument than the
  externally-visible ones

- stick basically all the wile primitives into the interpreter

- most of the special forms are in the interpreter

- add a batch mode to the interpreter

- add better error checking of let-binding and function args lists:
  must be all symbols, must be unique. kinda important.

- add functions to list GC version, architecture name

- fix a bug in (display): objects registered with display-object-hook
  were only printing correctly as top-level singletons; if stored in a
  list or vector, they'd show as ordinary not-special vectors.

- add repl and repl-dbg recipes into Makefile

- add change-file-owner and change-symbolic-link-owner primitives

- a whole lot of progress in adding macros into repl, some but not yet as
  much into wile

- move standard environment into wile-rtl2.scm

- add a compile-only mode to test-wile-progs.scm, to allow comparison
  between what the interpreted compiler and the compiled compiler produce

- add a limited ability to immediately define global variables in library
  mode: if the initializing value is nil or a boolean, char, int, rational,
  or real, it gets added on the declaration line. Can't handle complex
  numbers yet, or compound initializers, or ... but this is already a good
  beginning.

## Current version `wile` (0 9 1) 2023-10-15

- rework internal buffers to no longer use scratch files

- still working on adding doc-strings to more and more functions

- more work on making RTL stack-friendly

- work toward making closures fully first-class; still a ways to go

- start working on eval - a simple REPL is already possible, although
  still very limited: proof of concept only so far

## Current version `wile` (0 9 0) 2023-10-11

- implement `((val val val...) => proc-ish)` syntax for `case`

- implement more robust detection of mismatch in configuration between
  library and main program during compilation: put a do-nothing stub
  routine encoding the configuration into the library, and call that
  routine from main. each routine's name is generated by C macros,
  so they will differ for different configurations -> link fails

- make `floor` `ceiling` `round` handle all real-valued numbers

- bump `global-tc-min-args` up to 8... that removes a lot of warnings
  when the compiler rebuilds itself

- add an examples subdirectory with a couple of tiny separate programs
  that actually do non-compiler-related stuff

- rework code to build the RTL; now optimized and debug versions are
  equally easy to build

- fix bugs in `memv` `assv`

- change order of bits in output of `(wile-build-info)`

- make `char->string` work on list of characters as well as a literal
  list of character arguments, ie `(char->string '(#\a #\b #\c))` and
  `(char->string #\a #\b #\c)` will both generate the same "abc" output

- **MAKE WILE SELF-HOSTING** w00t!

## `wile` (0 8 0) initial release 2023-10-07
