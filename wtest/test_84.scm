;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be ()\t\t")
(sandbox
 (display
  (reset0-toplevel
   (reset0 (cons 'a (reset0 (shift0 f (shift0 g '())))))))
 (newline))

