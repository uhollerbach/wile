;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be (1 3 2 2 4)\t")
(sandbox
 (display (cons 1 (reset (cons 2 (shift k (cons 3 (k (k (cons 4 '())))))))))
 (newline))

