;;; test quasiquoting

(define (get v)
  (display v)
  (newline))

(get `a)
(get `12345)
(get `#\Q)
(get `#(1 2 3))					; fail
;;; (get `#u8(1 2 3))				; fail; unimplemented in skeem
(get `(1 2 3))
(get `(1 2 . 3))
(get `(1 ,(+ 1 2) 4))
(get `(,(+ 1 2) 4))
(get `(1 2 ,(+ 1 2)))
(get `(1 2 . ,(+ 1 2)))

(get `(foo1 ,(+ 1 2) 4))

(get `(1 ,(+ 2 3) ,@(fromto 4 7)))
(get `(1 ,@(fromto 4 7) ,(+ 2 3)))
(get `(1 ,(+ 2 3) . ,@(fromto 4 7)))		; MIT scheme barfs here
						; ",@ in illegal context"

(get `#(1 ,(+ 2 3) ,@(fromto 4 7)))		; fail
;;; (get `#u8(1 ,(+ 2 3) ,@(fromto 4 7)))	; fail; unimplemented in skeem

(let ((name 'a))
  (get `(list ,name ',name)))

(get `(( foo2 ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))

(get `(a `(b ,(+ 1 2) ,(foo3 ,(+ 1 3) d) e) f))

(let ((name1 'x)
      (name2 'y))
  (get `(a `(b ,,name1 ,',name2 d) e)))
