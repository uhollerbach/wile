;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(define (doit3 flag)
  (write-string "should be (1 4 3)\t")	; independent of flag
  (sandbox
   (display
    (cons 1
	  (let ((k2 (lambda (v2)
		      (let ((k1 (lambda (v1)
				  (reset (cons 2 (if flag v1 v2))))))
			(cons 3 '())))))
	    (reset (cons 4 (k2 (cons 5 '())))))))
   (newline)))

(doit3 #f)
(doit3 #t)

