;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be 7\t\t")
(sandbox
 (display
  (reset0-toplevel
   (reset0 (* 5
	      (reset0 (* 2
			 (shift0 f2 (* 3 (shift0 f3 7)))))))))
 (newline))

