(define (eeee? n)
  (if (zero? n)
      #t
      (oooo? (- n 1))))

(define (oooo? n)
  (if (zero? n)
      #f
      (eeee? (- n 1))))

(write-string "is 7 eeee? ")
(display (eeee? 7))
(newline)

(write-string "is 8 eeee? ")
(display (eeee? 8))
(newline)
