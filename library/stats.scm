;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; Compute the arithmetic mean of a number of samples xi

(define (average v . vs)
  (unless (list? v) (set! v (list v)))
  (set! vs (append v vs))
  (/ (apply + vs) (length vs)))

;;; Compute the corrected sample standard deviation
;;; of a number of samples xi

(define (stdev v . vs)
  (unless (list? v) (set! v (list v)))
  (set! vs (append v vs))
  (let* ((s0 (length vs))
	 (s1 (apply + vs))
	 (av (/ s1 s0))
	 (sq (lambda (x) (let ((dx (- x av))) (* dx dx))))
	 (s2 (apply + (map sq vs))))
    (sqrt (/ s2 (- s0 1)))))

(define (median v . vs)
  (unless (list? v) (set! v (list v)))
  (set! vs (append v vs))
  (let* ((vl (length vs))
	 (svt (list-tail (list-sort < vs) (quotient (- vl 1) 2))))
    (if (even? vl)
	(/ (+ (car svt) (cadr svt)) 2)
	(car svt))))

;;; Compute the Pearson's sample correlation coefficient
;;; of a number of samples (xi yi)

(define (correlation v . vs)
  (unless (list? (car v)) (set! v (list v)))
  (set! vs (append v vs))
  (let* ((sq (lambda (x) (* x x)))
	 (mxy (lambda (xy) (apply * xy)))
	 (s0 (length vs))
	 (sx1 (apply + (map car vs)))
	 (sy1 (apply + (map cadr vs)))
	 (sx2 (apply + (map sq (map car vs))))
	 (sy2 (apply + (map sq (map cadr vs))))
	 (sxy (apply + (map mxy vs)))
	 (sdx (sqrt (- (* s0 sx2) (sq sx1))))
	 (sdy (sqrt (- (* s0 sy2) (sq sy1))))
	 (num (- (* s0 sxy) (* sx1 sy1))))
    (/ num sdx sdy)))

;;; Find coefficients A & B in Y = A + B*X

(define (best-fit-line v . vs)
  (unless (list? (car v)) (set! v (list v)))
  (set! vs (append v vs))
  (let* ((sq (lambda (x) (* x x)))
	 (mxy (lambda (xy) (apply * xy)))
	 (s0 (length vs))
	 (sx (apply + (map car vs)))
	 (sy (apply + (map cadr vs)))
	 (sx2 (apply + (map sq (map car vs))))
	 (sxy (apply + (map mxy vs)))
	 (d (- (* s0 sx2) (sq sx))))
    (list (/ (- (* sy sx2) (* sx sxy)) d) (/ (- (* s0 sxy) (* sx sy)) d))))

;;; Find coefficients A B C in Y = A + B*X + C*X^2

(define (best-fit-parabola v . vs)
  (unless (list? (car v)) (set! v (list v)))
  (set! vs (append v vs))
  (let ((s0 (length vs)))
    (if (< s0 3)
	(append (best-fit-line vs) (list 0))
	(let* ((sq (lambda (x) (* x x)))
	       (mul (lambda (x1 x2) (* x1 x2)))
	       (xs (map car vs))
	       (x2s (map sq xs))

	       (ys (map cadr vs))

	       (sx (apply + xs))
	       (sx2 (apply + x2s))
	       (sx3 (apply + (map mul xs x2s)))
	       (sx4 (apply + (map sq x2s)))

	       (sy (apply + ys))
	       (syx (apply + (map mul ys xs)))
	       (syx2 (apply + (map mul ys x2s)))

	       (c1 (/ sx sx2))
	       (t22 (- sx2 (* c1 sx3)))
	       (t23 (- sx3 (* c1 sx4)))
	       (t2y (- syx (* c1 syx2)))

	       (c2 (/ s0 sx2))
	       (t12 (- sx (* c2 sx3)))
	       (t13 (- sx2 (* c2 sx4)))
	       (t1y (- sy (* c2 syx2)))

	       (c3 (/ t12 t22))
	       (u13 (- t13 (* c3 t23)))
	       (u1y (- t1y (* c3 t2y)))

	       (c4 (/ u1y u13))
	       (b (/ (- t2y (* c4 t23)) t22)))
	  (list (/ (- syx2 (* sx3 b) (* sx4 c4)) sx2) b c4)))))

;;; Find coefficients A & B in Y = A*exp(B*X)
;;; Assumes that all Y values are positive

(define (best-fit-exponential v . vs)
  (unless (list? (car v)) (set! v (list v)))
  (set! vs (append v vs))
  (let ((cs (best-fit-line (map (lambda (pt)
				  (list (car pt) (log (cadr pt))))
				vs))))
    (list (exp (car cs)) (cadr cs))))

;;; Find coefficients A & B in Y = A*X**B
;;; Assumes that all X and Y values are positive

(define (best-fit-power v . vs)
  (unless (list? (car v)) (set! v (list v)))
  (set! vs (append v vs))
  (let ((cs (best-fit-line (map (lambda (pt)
				  (list (log (car pt)) (log (cadr pt))))
				vs))))
    (list (exp (car cs)) (cadr cs))))

;;; (load-library "cholesky.scm")

;;; model is (y 1 x) so we have lots of (yi 1 xi) triples
;;; (y1 1 x1)
;;; (y2 1 x2)
;;; ...
;;; 
;;; transpose is
;;; (y1 y2 ...)
;;; (1  1  ...)
;;; (x1 x2 ...)
;;; 
;;; multiply rectangular matrices, get
;;; 
;;; [ s(yi^2 ) s(yi) s(xi*yi) ]
;;; [ s(yi)    s(1)  s(xi)    ]
;;; [ s(xi*yi) s(xi) s(xi^2)  ]
;;; 
;;; the right equations drop out if we ignore the first row, then
;;; multiply by a vector (-1, a, b) and use the remaining rows to
;;; solve for a and b
;;; 
;;; why do we ignore the first row? we write the residual as s[(yi - a
;;; - b*xi)^2]; if we had a coefficient c in front of the yi, the
;;; equivalent of using a vector (c, a, b), we could trivially and
;;; uninformatively always get residual zero by setting all of them to 0.

(define (best-fit-general model vs)
  (letrec* ((d1 (lambda (v1 v2 acc)		; dot-product of two vectors
		  (if (or (null? v1) (null? v2))
		      acc
		      (d1 (cdr v1) (cdr v2) (+ acc (* (car v1) (car v2)))))))
	    (d2 (lambda (vs)			; dot products of upper or lower
		  (if (null? vs)		; triangle of normal equations
		      ()
		      (cons (map (lambda (v) (d1 (car vs) v 0)) vs)
			    (d2 (cdr vs))))))
	    (m1 (d2 (let loop ((rs (map model vs)))
		      (if (null? (car rs))
			  '()
			  (cons (map car rs) (loop (map cdr rs))))))))
	   (cholesky-solve (cholesky-decompose (cdr m1)) (cdar m1))))
