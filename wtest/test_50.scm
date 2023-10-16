;;; wilec isn't happy with the load-library yet
;;; (load-library "struct.scm")

(def-struct foo good bad)

(display-object-hook 'foo
 (lambda (o p)
   (fprintf p "[foo %v]" (get-foo-good o))))

(define f (make-foo 'good 'BADD))

(define g (replicate f 5))

(define h (list->vector g))

(display f)
(newline)
(display g)
(newline)
(display h)
(newline)
