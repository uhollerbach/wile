(define (dn val)
  (display val)
  (newline))

(dn (list 'b 'e 'h))
(dn (map cadr (list (list 'a 'b) (list 'd 'e) (list 'g 'h))))

(dn (list 1 4 27 256 3125))
(dn (map (lambda (n) (expt n n)) (list 1 2 3 4 5)))

(dn (list 6 8 10 12))
(dn (map + (list 1 2 3 4) (list 5 6 7 8)))

(dn (list (list 1 'a) (list 2 'b) (list 3 'c)))
(dn (map list (list 1 2 3) (list 'a 'b 'c)))
(for-each (lambda (a b) (dn (list a b)))
	  (list 1 2 3) (list 'a 'b 'c))

(dn (list 5 7 9))
(dn (map + (list 1 2 3) (list 4 5 6)))
(dn (list 12 15 18))
(dn (map + (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(for-each (lambda (a b c) (dn (+ a b c)))
	  (list 1 2 3) (list 4 5 6) (list 7 8 9))

(dn (list 4 10 18))
(dn (map * (list 1 2 3) (list 4 5 6)))

(let ((lst (list 1 2 7 9 3 4 4 6 -1 8 11 -24)))
  (dn (list 3 5 15 19 7 9 9 13 -1 17 23 -47))
  (dn (map (lambda (x) (+ 1 (* 2 x))) lst)))

(dn (list #\N #\O #\W))
(dn (map char-upcase (list #\n #\o #\w)))

(dn (list #\t #\i #\m #\e))
(dn (map char-downcase (list #\T #\I #\M #\E)))
