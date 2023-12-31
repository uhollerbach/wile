(define (sum val . vals)
  (let ((s val)
	(vs vals))
    (until (null? vs)
	   (set! s (+ s (car vs)))
	   (set! vs (cdr vs)))
    s))

(display (gcd 45 165))
(newline)
(display (apply gcd (list 45 165)))
(newline)

(display (sum 45 165))
(newline)
(display (apply sum (list 45 165)))
(newline)

(display (list "foo" "bar" "baz" "bletch" "quux"))
(newline)
(display (apply list "foo" "bar" (list "baz" "bletch" "quux")))
(newline)

(display (vector "foo" "bar" "baz" "bletch" "quux"))
(newline)
(display (apply vector "foo" "bar" (list "baz" "bletch" "quux")))
(newline)

(display (append (list "foo" "bar") (list "baz" "bletch" "quux")))
(newline)
(display (apply append (list "foo" "bar") (list "baz" "bletch" "quux") ()))
(newline)

(display (string-append "foo" "bar" "baz" "bletch" "quux"))
(newline)
(display (apply string-append "foo" "bar" (list "baz" "bletch" "quux")))
(newline)

(display (string-append))
(newline)
(display (apply string-append ()))
(newline)

(display (string-join-by "," "foo" "bar" "baz" "bletch" "quux"))
(newline)
(display (apply string-join-by "," "foo" "bar" (list "baz" "bletch" "quux")))
(newline)

(display (char->string #\A #\B #\C #\D))
(newline)
(display (apply char->string #\A #\B #\C #\D ()))
(newline)
