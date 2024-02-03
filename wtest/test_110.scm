;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be 3\t\t")
(sandbox
 (display (reset0-toplevel (reset0 (* 2 (shift0 f 3)))))
 (newline))

