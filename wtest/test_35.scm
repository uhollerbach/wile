(define (dn val)
  (display val)
  (newline))

(define (main ig)
  (dn 1)
  (dn (call/cc (lambda (cont)
		 (dn 2)
		 (cont 3)
		 (dn "yeek"))))
  (dn 4))

(main #t)
(exit 0)
