;;; test multiple layers of scopes for closures

(define (blab msg n sep)
  (let loop1 ((i 0))
    (let ((k (min n (+ i 3))))
      (let loop2 ((j i))
	(write-string (number->string j) sep)
	(when (< j k)
	  (loop2 (+ j 1))))
      (write-string msg #\newline)
      (when (< i n)
	(loop1 (+ i 1))))))

(blab "foo" 9 #\space)
(blab "bar" 9 "..")
