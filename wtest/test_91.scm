;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(define (doit2 flag)
  (if flag
      (write-string "should be (1 4 2 5)\t")
      (write-string "should be (1 3)\t\t"))
  (sandbox
   (display
    (cons 1 (reset (cons 2 (if flag
			       (shift k2 (cons 4 (k2 (cons 5 '()))))
			       (shift k1 (cons 3 '())))))))
   (newline)))

(doit2 #f)
(doit2 #t)

