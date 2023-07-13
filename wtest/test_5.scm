(define (dn foo)
  (display foo)
  (newline))

(dn (string-join-by ""))
(dn (string-join-by "" "foo"))
(dn (string-join-by "" "foo" "bar" "baz" "quux"))

(dn (string-join-by ","))
(dn (string-join-by "," "foo"))
(dn (string-join-by "," "foo" "bar" "baz" "quux"))

(dn (string-join-by "" ()))
(dn (string-join-by "" (list "foo")))
(dn (string-join-by "" (list "foo" "bar" "baz" "quux")))

(dn (string-join-by "," ()))
(dn (string-join-by "," (list "foo")))
(dn (string-join-by "," (list "foo" "bar" "baz" "quux")))
