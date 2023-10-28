;;; The list monad: pretty trivial
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: 2clause BSD

(define (list-wrap v) (list v))

(define (list->>= mv f)
  (apply append (map f mv)))

(define (list->> mv f)
  (list->>= mv (lambda (ig) f)))
