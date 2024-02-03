;;; test 1 of cond-expand

(cond-expand
 (mes
  (display "mes?\n")
  (define (foo)
    (display "mes!\n")
    (exit 0)))
 (guile
  (display "guile?\n")
  (define (foo)
    (display "guile!\n")
    (exit guile?)))
 (wile
  (display "wile?\n")
  (define (foo)
    (display "wile!\n")
    (exit 0)))
 (else
  (exit 1)))

(foo)
(display "what?!?\n")
(exit 1)
