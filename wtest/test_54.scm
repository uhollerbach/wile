(define (primey i n)
  (if (<= n 0)
      #t
      (let ((j (next-prime i)))
	(display j)
	(newline)
	(primey (* 2 j) (- n 1)))))

(primey 2 30)
