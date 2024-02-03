;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(define (doit5 flag)
  (if flag
      (write-string "should be (1 3 2 4)\t")
      (write-string "should be (1 5 2 6)\t"))
  (sandbox
   (display
    (cons 1
	  (reset (cons 2 (if flag
			     (shift k1 (cons 3 (k1 (cons 4 '()))))
			     (shift k2 (cons 5 (k2 (cons 6 '())))))))))
   (newline)))

(doit5 #f)
(doit5 #t)

