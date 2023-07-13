;;; test Cholesky decomposition and solve routines

(define (dn val)
  (display val)
  (newline))

(write-string "Cholesky decompose\n")

;;; ((14 68/7 -39/34 20/13) ((1 1/7 1/2 5/14) (1 7/17 1/34) (1 7/13) (1)))

(dn (cholesky-decompose
     (list (list 14 2 7 5)
	   (list 10 5 1)
	   (list 4 2)
	   (list 3))))

;;; ((3 3 11/3 11/3 24/11 45/22) ((1 0 0 1/3 1/3 1/3) (1 1/3 0 1/3 1/3) (1 0 -1/11 2/11) (1 2/11 -1/11) (1 -1/4) (1)))

(dn (cholesky-decompose
     (list (list 3 0 0 1 1 1)
	   (list 3 1 0 1 1)
	   (list 4 0 0 1)
	   (list 4 1 0)
	   (list 3 0)
	   (list 3))))

;;; ((10 5 1) ((1 2 3) (1 4) (1)))
(dn (cholesky-decompose
     (list (list 10 20 30)
	   (list 45 80)
	   (list 171))))

;;; ((4 1 9) ((1 3 -4) (1 5) (1)))
(dn (cholesky-decompose
     (list (list 4 12 -16)
	   (list 37 -43)
	   (list 98))))

(write-string "Cholesky solve 1\n")

(let ((cde (list (list 10 5 1) (list (list 1 2 3) (list 1 4) (list 1)))))
  ;;; (259/10 -102/5 5)
  (dn  (cholesky-solve cde (list 1 0 0)))
  ;;; (-102/5 81/5 -4)
  (dn  (cholesky-solve cde (list 0 1 0)))
  ;;; (5 -4 1)
  (dn  (cholesky-solve cde (list 0 0 1))))

(write-string "Cholesky solve 2\n")

(let ((cde (list (list 4 1 9) (list (list 1 3 -4) (list 1 5) (list 1)))))
  ;;; (1777/36 -122/9 19/9)
  (dn  (cholesky-solve cde (list 1 0 0)))
  ;;; (-122/9 34/9 -5/9)
  (dn  (cholesky-solve cde (list 0 1 0)))
  ;;; (19/9 -5/9 1/9)
  (dn  (cholesky-solve cde (list 0 0 1))))
