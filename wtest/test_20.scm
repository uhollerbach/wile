;;; test string/symbol case, implicit no-default default #f value

(define (letter-type val)
  (case val
    ((a e i o u) 'vowel)
    ((y w) 'semivowel)
    ((b c d f g h j k l m n p q r s t v x z ch sh th) 'consonant)
    ((cucumber) 'vegetable-you-idiot!)))

(define (dn v)
  (display v)
  (newline))

(dn (letter-type 'a))
(dn (letter-type 'y))
(dn (letter-type 'g))
(dn (letter-type 'th))
(dn (letter-type 'splut))
(dn (letter-type 'cucumber))
