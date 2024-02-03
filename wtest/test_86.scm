;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be 132342344234442344442344444234444442344...\n\t  ")
(write-string "it passes! but it's an infinite loop, skip for now\n")
;; (sandbox
;;  (prompt (begin (control f (begin (f (display 1)) (f (display 2))))
;; 		(control f (begin (f (display 3)) (f (display 4))))))
;;  (newline))

