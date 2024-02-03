;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(define (doit7 flag)
  (sandbox
   (display
    (cons 1
	  (reset (cons 2 (if flag
			     (shift k1 (set! flag (not flag))
				    (cons 3 (k1 (cons 4 '()))))
			     (shift k2 (set! flag (not flag))
				    (cons 5 (k2 (cons 6 '())))))))))
   (newline)))

(write-string "should be (1 5 2 6)\t")
(doit7 #f)

(write-string "should be (1 3 2 4)\t")
(doit7 #t)

