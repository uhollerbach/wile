;;; test that tail calls don't step on themselves...
;;; and that we can suppress them: pragma

(pragma suppress-tail-call-generation)

(define (typof v)
  (cond ((null? v) 'nil)
	((symbol? v) 'symbol)
	((boolean? v) 'boolean)
	((char? v) 'char)
	((string? v) 'string)
	((integer? v) 'integer)
	((rational? v) 'rational)
	((real? v) 'real)
	((complex? v) 'complex)
	((pair? v) 'pair)
	((vector? v) 'vector)
	((bytevector? v) 'bytevector)
	((procedure? v) 'procedure)
	(else 'unknown!)))

(define (fn1 val1 val2)
  (display val2)
  (write-string ": ")
  (display (typof val2))
  (newline))

(define (fn2 val)
  (fn1 #t val))

(fn2 #f)
(fn2 42)
(fn2 3.14159)
(fn2 #\space)
(fn2 "foo")
(fn2 ())
(fn2 '(a b))
(fn2 fn1)
