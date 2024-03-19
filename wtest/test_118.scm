(display (list-group-by eqv? (string->char "Mississippi")))
(newline)

(display (list-group-by (lambda (a b) (eqv? (even? a) (even? b)))
			'(4 1 2 6 8 3 4 2 7 5 5 4)))
(newline)

(display (list-group-by < '(1 2 3 3 4 5 4 2 3 5 77 67)))
(newline)
