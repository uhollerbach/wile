;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be ()\t\t")
(sandbox
 (display
  (prompt-toplevel
   (prompt (let ((y (control f (cons 'a (f '()))))) (control g y)))))
 (newline))

