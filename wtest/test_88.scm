;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be (a)\t\t")
(sandbox
 (display
  (prompt-toplevel
   (prompt
    (prompt (cons 'a (prompt
		      (let ((y (control f (control g (cons 'b (f '()))))))
			(control h y))))))))
 (newline))

