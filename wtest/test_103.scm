;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be 7\t\t")
(sandbox
 (display
  (prompt0-toplevel
   (prompt0 (* 5 (
		  (prompt0 (* 2
			      (control0 f2 (lambda ()
					     (* 3 (control0 f3 7)))))))))))
 (newline))

