;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(define (doit4 flag)
  (if flag
      (write-string "should be (1 4 2 3)\t")
      (write-string "should be (1 4 2 5)\t"))
  (sandbox
   (display
    (cons 1
	  (let ((k2 (lambda (v2)
		      (reset (cons 2 (if flag (cons 3 '()) v2))))))
	    (reset (cons 4 (k2 (cons 5 '())))))))
   (newline)))

(doit4 #f)
(doit4 #t)

