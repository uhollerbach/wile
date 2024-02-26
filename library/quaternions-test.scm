;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2024, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; test quaternions code

(load-library "test.scm")
(load-library "quaternions.scm")

(test-title "quaternion tests")
(test-mode 'report-failed)	;;; 'off 'summary 'report-failed 'report

(define qr (quaternion-create 1 0 0 0))
(define qi (quaternion-create 0 1 0 0))
(define qj (quaternion-create 0 0 1 0))
(define qk (quaternion-create 0 0 0 1))

(define qnr (quaternion-neg qr))
(define qni (quaternion-neg qi))
(define qnj (quaternion-neg qj))
(define qnk (quaternion-neg qk))

(test #t (quaternion=? (quaternion-mul qr qr) qr))
(test #t (quaternion=? (quaternion-mul qi qi) qnr))
(test #t (quaternion=? (quaternion-mul qj qj) qnr))
(test #t (quaternion=? (quaternion-mul qk qk) qnr))

(test #t (quaternion=? (quaternion-mul qr qi) qi))
(test #t (quaternion=? (quaternion-mul qr qj) qj))
(test #t (quaternion=? (quaternion-mul qr qk) qk))

(test #t (quaternion=? (quaternion-mul qi qr) qi))
(test #t (quaternion=? (quaternion-mul qj qr) qj))
(test #t (quaternion=? (quaternion-mul qk qr) qk))

(test #t (quaternion=? (quaternion-mul qi qj) qk))
(test #t (quaternion=? (quaternion-mul qj qk) qi))
(test #t (quaternion=? (quaternion-mul qk qi) qj))

(test #t (quaternion=? (quaternion-mul qj qi) qnk))
(test #t (quaternion=? (quaternion-mul qk qj) qni))
(test #t (quaternion=? (quaternion-mul qi qk) qnj))

(let* ((q1 (quaternion-create 1 3 2 5))
       (q2 (quaternion-conj q1))
       (n1 (quaternion-norm q1))
       (n2 (quaternion-mul q1 q2))
       (n3 (quaternion-mul q2 q1)))
  (test 39.0 (* n1 n1))
  (test 39 (get-quaternion-a n2))
  (test 39 (get-quaternion-a n3))
  (test q2
	(quaternion-scale -1/2
			  (quaternion-add
			   q1
			   (quaternion-add
			    (quaternion-mul qi (quaternion-mul q1 qi))
			    (quaternion-add
			     (quaternion-mul qj (quaternion-mul q1 qj))
			     (quaternion-mul qk (quaternion-mul q1 qk))))))))

(test-report "quaternion tests")

(if (test-expected 20 0 0 0)
    (write-string "results are as expected\n")
    (write-string "results are unexpected!\n"))
