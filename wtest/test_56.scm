(load-library "bigint.scm")

;;; Lucas-Lehmer test for primality of Mersenne numbers:
;;; let p be a prime > 2, and define
;;;
;;;  M_p = 2^p - 1
;;;  s_i = 4			if i == 0
;;;        s_{i-1}^2 - 2		otherwise
;;;
;;; then M_p is prime IFF s_{p-2} = 0 mod M_p

(define (mersenne-prime? e)
  (if (= e 2)
      #t
      (let* ((big-two (bigint-from-integer 2))
	     (m-num (bigint-diff
		     (bigint-expt
		      big-two (bigint-from-integer e)) bigint-const-1)))
	(let loop ((s (bigint-from-integer 4))
		   (c (i- e 2)))
	  (if (zero? c)
	      (bigint-zero? (bigint-remainder s m-num))
	      (loop (bigint-remainder
		     (bigint-diff (bigint-prod s s) big-two)
		     m-num)
		    (i- c 1)))))))

(if (and (not (null? command-line-arguments))
	 (string=? (car command-line-arguments) "full"))
    (let ((ps '(2 3 5 7)))
      (do ((i 9 (i+ i 2)))
	  ((> i 1300) #t)
	(let ((dd #f))
	  (for-each (lambda (p)
		      (when (> (gcd i p) 1)
			(set! dd #t)))
		    ps)
	  (unless dd
	    (set! ps (list-append ps (list i))))))
      (for-each (lambda (p)
		  (when (mersenne-prime? p)
		    (printf "M_%d is a Mersenne prime\n" p)))
		ps))
    (for-each (lambda (p)
		(if (mersenne-prime? p)
		    (printf "M_%d is a Mersenne prime\n" p)
		    (printf "M_%d is NOT a Mersenne prime\n" p)))
	      '(2 3 5 7 13 17 19 31 43 61 89 107 127 419 521 607)))
