;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(define (doit1 flag)
  (if flag
      (write-string "should be (1 3)\t\t")
      (write-string "should be (1 4 2 5)\t"))
  (sandbox
   (display
    (cons 1 (reset (cons 2 (if flag
			       (shift k1 (cons 3 '()))
			       (shift k2 (cons 4 (k2 (cons 5 '())))))))))
   (newline)))

(doit1 #f)
(doit1 #t)

