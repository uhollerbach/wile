(define (dn v)
  (display v)
  (newline))

(define sq (lambda (v) (* v v)))

(dn (sq 13))

(define (mappit proc lst)
  (if (null? lst)
      ()
      (cons (proc (car lst)) (mappit proc (cdr lst)))))

(dn (mappit sq (list 1 2 3 4 5)))
(dn (mappit (lambda (v) (* v v v)) (list 1 2 3 4 5)))
