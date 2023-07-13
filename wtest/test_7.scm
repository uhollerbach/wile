(define l1 (list 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9))
(define l2 (list "foo" "bar" "bletch" "quux" "shnick"))

(define (dn val)
  (display val)
  (newline))

(dn (list-sort < l1))
(dn (list-sort > l1))

(dn (list-sort string<? l2))
(dn (list-sort string>? l2))
