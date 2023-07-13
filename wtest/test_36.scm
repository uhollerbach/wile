(define (dn val)
  (display val)
  (newline))

(define saved-cont #f)

(define (main ig)
  (define count 0)
  (dn (+ 100 (call/cc (lambda (cont)
			(set! saved-cont cont)
			(cont 100)
			(dn "yeek")  ))))
  (when (< count 3)
    (set! count (+ 1 count))
    (dn "to the moon!")
    (saved-cont count)))

(main 1)
(exit 0)
