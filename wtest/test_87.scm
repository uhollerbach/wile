;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

(load-library "call-cc-macros.scm")


(write-string "should be (a b)\t\t")
(sandbox
 (display
  (reset
   (reset (cons 'a (reset
		    (let ((y (shift f (shift g (cons 'b (f '()))))))
		      (shift h y)))))))
 (newline))

