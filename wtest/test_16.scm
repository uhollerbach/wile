(define s (get-environment-variable "WILE_MOD"))
(define (mod+ a b) (modulo (+ a b) 17))

(display ((if s + mod+) 13 41))
(newline)
(display (+ 13 41))
(newline)
(display (mod+ 13 41))
(newline)
