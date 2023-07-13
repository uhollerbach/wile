;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

(load "test.scm")
(load "struct.scm")
(load "stack.scm")

;;; (make-stack . args)    alias stack-create
;;; (stack? obj)
;;; (stack-empty? s)
;;; (stack-push s v)
;;; (stack-pop s def)
;;; (stack-top s def)
;;; (stack-size s)
;;; (stack-list s)
;;; (stack-reverse s)
;;; (stack-peek-nth s k)

(test-title "stack tests")

;;; 'off 'summary 'report-failed 'report

(test-mode 'report-failed)

(test #(stack (1 2 3)) (make-stack 1 2 3))

(define s (make-stack 1 3 5 7 2 4 6 8))

(test #t (stack? s))

(test #f (stack? 'cheese))

(test #f (stack-empty? s))

(test "[stack 1 3 5 7 2 4 6 8]" (sprintf "%v" s))

(test #(stack (9 1 3 5 7 2 4 6 8)) (stack-push s 9))

(test 9 (stack-top s #f))

(test 9 (stack-pop s #f))

(test 7 (stack-peek-nth s 3 #f))

(test 8 (stack-size s))

(test (list 1 3 5 7 2 4 6 8) (stack-list s))

(test #(stack (8 6 4 2 7 5 3 1)) (stack-reverse s))

(test-report "stack test final result")

(exit (if (test-expected 12 0 0 0) 0 1))
