(define (dn v)
  (display v)
  (newline))

(dn (char->string #\a #\b #\c))
(dn (apply char->string #\a #\b #\c ()))
(dn (char->string '(#\a #\b #\c)))
(dn (apply char->string '(#\a #\b #\c)))

(dn (char->string #\a #\b))
(dn (apply char->string #\a #\b ()))
(dn (char->string '(#\a #\b)))
(dn (apply char->string '(#\a #\b)))

(dn (char->string #\a))
(dn (apply char->string #\a ()))
(dn (char->string '(#\a)))
(dn (apply char->string '(#\a)))

(dn (char->string))
(dn (apply char->string ()))
(dn (char->string '()))

(dn (list->string #\a #\b #\c))
(dn (apply list->string #\a #\b #\c ()))
(dn (list->string '(#\a #\b #\c)))
(dn (apply list->string '(#\a #\b #\c)))

(dn (list->string #\a #\b))
(dn (apply list->string #\a #\b ()))
(dn (list->string '(#\a #\b)))
(dn (apply list->string '(#\a #\b)))

(dn (list->string #\a))
(dn (apply list->string #\a ()))
(dn (list->string '(#\a)))
(dn (apply list->string '(#\a)))

(dn (list->string))
(dn (apply list->string ()))
(dn (list->string '()))
