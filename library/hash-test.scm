;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

(load "hash.scm")
(load "test.scm")

(test-title "hash tests")

(test-mode 'report-failed)

(define nvals 5000)
(define ntests 100000)
(define storage (vector-create nvals))
(define ht (hash-table-create string-hash-64 string=?))

(do ((i 0 (+ i 1)))
    ((= i nvals) #t)
  (let ((vs (vector-create 3)))
    (vector-set! vs 0 (number->string i))
    (vector-set! vs 1 (number->string (random-uniform 0.0 1.0) 10 8))
    (vector-set! vs 2 #f)
    (vector-set! storage i vs)))

(do ((i 0 (+ i 1)))
    ((= i ntests) #t)
  (let ((vs (vector-ref storage (integer (random-uniform 0 nvals)))))
    (if (< 0.5 (random-uniform 0.0 1.0))
	(begin (hash-table-set! ht (vector-ref vs 0) (vector-ref vs 1))
	       (vector-set! vs 2 #t))
	(begin (hash-table-delete! ht (vector-ref vs 0))
	       (vector-set! vs 2 #f)))))

(let ((count 0))
  (do ((i 0 (+ i 1)))
      ((= i nvals) #t)
    (let ((vs (vector-ref storage i)))
      (test (vector-ref vs 2) (hash-table-contains? ht (vector-ref vs 0)))
      (when (vector-ref vs 2)
	    (set! count (+ count 1)))))
  (test count (hash-table-size ht)))

(hash-table-clear! ht)
(test 0 (hash-table-size ht))

(test-report "hash test final result")

(unless (test-expected (+ nvals 2) 0 0 0) (exit 1))
