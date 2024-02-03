;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be 59\t\t")
(sandbox
 (display (+ 1 (reset (+ 2 (shift k1 (+ 3
					(k1 5)
					(k1 1)
					(reset (* 10 (shift k2 (+ (k1 3)
								  (k2 4)))))
					))))))
 (newline))

