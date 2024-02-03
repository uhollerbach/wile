;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be 1 2 3\t\t")
(sandbox
 (reset (write-string "1 ")
	(shift c (write-string "2 ") (c 'ignore))
	(write-string "3 "))
 (newline))

