;;; Explorations of the list monad: N-queens puzzle
;;; Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: 2clause BSD

(load-library "monad.scm")
(load-library "monad-list.scm")

(define >>= list->>=)
(define wrap list-wrap)

(define (prqueens qs)
  (let ((nq (list-length qs)))
    (map (lambda (q)
	   (do ((i 0 (+ i 1)))
	       ((= i nq) #t)
	     (write-char (if (= i q) #\X #\.)))
	   (newline))
	 qs)
    (newline)
    (wrap qs)))

;;; elegant version

(define (n-queens n)
  (printf "#### %d queens\n" n)
  (letrec* ((nsols 0)
	    (nq (lambda (pqs)
		  (let ((nps (list-length pqs)))
		    (if (= n nps)
			(begin
			  (set! nsols (+ nsols 1))
			  (prqueens (list-reverse pqs)))
			(doM >>=
			     (<- qc (fromto 0 (- n 1)))
			     (= qds (map (lambda (qp) (abs (- qc qp))) pqs))
			     (= tms (map (lambda (qd) (/= qd 0)) qds))
			     (= tds (map (lambda (qd di) (/= qd di))
					 qds (fromto 1 nps)))
			     (if (and (all-true? tms) (all-true? tds))
				 (nq (cons qc pqs))
				 ())))))))
	   (doM >>=
		(<- q1 (fromto 0 (- n 1)))
		(nq (list q1)))
	   (printf "#### %d queens found %d solutions\n" n nsols)))

(n-queens 1)
(n-queens 2)
(n-queens 3)
(n-queens 4)
(n-queens 5)
(n-queens 6)
(n-queens 7)
(n-queens 8)

;;; only try these compiled! interpreted is too slow
;;; (n-queens 9)
;;; (n-queens 10)
;;; (n-queens 11)
;;; (n-queens 12)
