;;; test 1 of cond-expand

(cond-expand
 (mes
  (display "mes!\n")
  (exit 0))
 (guile
  (display "guile!\n")
  (exit guile?))
 (wile
  (display "wile!\n")
  (exit 0))
 (else
  (display "what?!?\n")
  (exit 1)))
