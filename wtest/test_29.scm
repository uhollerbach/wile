(define (proc1)
  (display '(1 2 (a b) 3 4))
  (newline)
  (display (list 1 2 (list 'a 'b) 3 4))
  (newline)
  (display '(1 2 (a b) 3 4 . 5))
  (newline))

(define (proc2)
  (display '())
  (newline)
  (display (list))
  (newline))

(define (proc3)
  (display #(1 2 #(a b) 3 4))
  (newline)
  (display (vector 1 2 (vector 'a 'b) 3 4))
  (newline))

(define (proc4)
  (display #())
  (newline)
  (display (vector))
  (newline))

(do ((i 1 (+ i 1)))
    ((> i 5) #t)
  (printf "######## iteration %d\n" i)
  (proc1)
  (proc2)
  (proc3)
  (proc4))
