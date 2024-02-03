;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be 35\t\t")
(sandbox
 (display
  (prompt-toplevel
   (prompt (* 5
	      (prompt (* 2
			 (control f2 (* 3 (control f3 7)))))))))
 (newline))

