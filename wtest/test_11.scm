(define (froomtow f l)
  (cond ((= f l) (list f))
	((< f l) (let loop ((f f) (n l) (acc ()))
		   (if (< n f) acc (loop f (- n 1) (cons n acc)))))
	((> f l) (let loop ((f f) (n l) (acc ()))
		   (if (> n f) acc (loop f (+ n 1) (cons n acc)))))))

(display (froomtow 4 4))
(newline)
(display (froomtow 2 6))
(newline)
(display (froomtow 6 2))
(newline)

