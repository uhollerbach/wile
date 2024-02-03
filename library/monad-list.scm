;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; The list monad: rather trivial!

(define (list-wrap v) (list v))

(define (list->>= mv f)
  (apply append (map f mv)))

(define (list->> mv f)
  (list->>= mv (lambda (ig) f)))
