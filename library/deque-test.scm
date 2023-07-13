;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

(load "struct.scm")
(load "deque.scm")
(load "test.scm")

(test-title "deque tests")

;;; 'off 'summary 'report-failed 'report

(test-mode 'report-failed)

;;; the expected is not actually equivalent, it is missing the
;;; internal bit of structure. probe that more below.

(test #(deque () ()) (make-deque))
(test #(deque () ()) (deque-create))

(define q (make-deque))

(test #t (deque? q))

(test #f (deque? 'cheese))

(test #t (deque-empty? q))

(test "[deque]" (sprintf "%v" q))

(deque-insert-front q 1)
(deque-insert-front q 2)
(deque-insert-front q 3)
(deque-insert-back q 4)
(deque-insert-back q 5)
(deque-insert-back q 6)

(test "[deque 3 2 1 4 5 6]" (sprintf "%v" q))

(test 3 (deque-peek-front q #f))
(test 2 (deque-peek-front2 q #f))
(test 3 (deque-remove-front q #f))
(test 1 (deque-peek-front2 q #f))

(test 6 (deque-peek-back q #f))
(test 5 (deque-peek-back2 q #f))
(test 6 (deque-remove-back q #f))
(test 4 (deque-peek-back2 q #f))

(test "[deque 2 1 4 5]" (sprintf "%v" q))

(test 4 (deque-size q))
(test (list 2 1 4 5) (deque-list q))

(test (list 2 1 4 5) (deque-remove-all q))
(test #(deque () ()) q)

(deque-insert-front q 1)
(deque-insert-front q 2)
(deque-insert-front q 3)
(deque-insert-front q 4)
(deque-insert-front q 5)
(deque-apply q (lambda (x) (* x x x)))

(test "[deque 125 64 27 8 1]" (sprintf "%v" q))

(deque-reverse q)

(test "[deque 1 8 27 64 125]" (sprintf "%v" q))

(let ((sum 0))
  (deque-for-each q (lambda (v) (set! sum (+ sum v))))
  (test 225 sum))

(deque-sort q >)

(test "[deque 125 64 27 8 1]" (sprintf "%v" q))

(test-report "deque test final result")

(exit (if (test-expected 24 0 0 0) 0 1))
