;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be 7\t\t")
(sandbox
 (display (reset (* 5 (
		       (reset (* 2
				 (shift f2 (lambda ()
					     (* 3 (shift f3 7))))))))))
 (newline))

