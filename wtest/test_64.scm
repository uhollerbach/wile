;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be 6\t\t")
(sandbox
 (display (+ 1 (reset (+ 2 3))))
 (newline))

