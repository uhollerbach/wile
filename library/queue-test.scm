;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

(load "test.scm")
(load "queue.scm")

(test-title "queue tests")

;;; 'off 'summary 'report-failed 'report

(test-mode 'report-failed)

(let ((q1 (make-queue 1 2 3))
      (q2 (queue-create))
      (q3 (make-queue 1 3 5 7 2 4 6 8))
      (q4 (queue-create 9 12 10 11)))
  (test #t (queue? q1))
  (test #t (queue? q2))
  (test #f (queue? 'cheese))
  (test #f (queue-empty? q1))
  (test #t (queue-empty? q2))
  (test '(1 2 3) (queue-list q1))
  (queue-insert-front q1 4)
  (test '(4 1 2 3) (queue-list q1))
  (test '() (queue-list q2))
  (queue-insert-front q2 4)
  (test '(4) (queue-list q2))
  (test "[queue 1 3 5 7 2 4 6 8]" (sprintf "%v" q3))
  (queue-insert-front q3 9)
  (test '(9 1 3 5 7 2 4 6 8) (queue-list q3))
  (queue-insert-back q3 0)
  (test '(9 1 3 5 7 2 4 6 8 0) (queue-list q3))
  (test 9 (queue-peek-front q3 #f))
  (test 0 (queue-peek-back q3 #f))
  (test 9 (queue-remove q3 #f))
  (test 7 (queue-peek-nth q3 3 #f))
  (test 9 (queue-size q3))
  (queue-reverse q3)
  (test '(0 8 6 4 2 7 5 3 1) (queue-list q3))
  (queue-append q3 q4)
  (test '(0 8 6 4 2 7 5 3 1 9 12 10 11) (queue-list q3))
  (test #t (queue-empty? q4))
  (queue-sort q3 <)
  (test (fromto 0 12) (queue-list q3))
  (queue-apply q3 (lambda (x) (* x x)))
  (test '(0 1 4 9 16 25 36 49 64 81 100 121 144) (queue-list q3))
  ;;; here we are getting downright familiar with the queue struct
  (let ((ll (vector-ref q3 2)))
    (set-ql-val! ll pi))
  (test (list 0 1 4 9 16 25 36 49 64 81 100 121 pi) (queue-remove-all q3))
  (test #t (queue-empty? q3)))

(test-report "queue test final result")

(exit (if (test-expected 24 0 0 0) 0 1))
