;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be ()\t\t")
(sandbox
 (display
  (prompt0-toplevel
   (prompt0
    (prompt0 (cons 'a (prompt0
		       (let ((y (control0 f (control0 g (cons 'b (f '()))))))
			 (control0 h y))))))))
 (newline))

