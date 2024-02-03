;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; monad laws - scheme notation
;;;     (>>= (return v) f)			=	(f v)
;;;     (>>= m (lambda (v) (return v)))		=	m
;;;     (>>= m (lambda (v) (>>= (f v) g)))	=	(>>= (>>= m f) g)
;;;     v is not free in g nor in f

;;; do-notation
;;;
;;; doM act1			act1
;;;
;;; doM act1 act2 ... actN	act1 >> (doM act2 ... actN)
;;;
;;; here act1 is (something that produces) a wrapped value
;;; doM (<- nm1 act1)		act1 >>= (lambda (nm1) doM act2 ... actN)
;;;     act2 ... actN
;;;
;;; here val1 is a non-wrapped value
;;; doM (= nm1 val1)		((lambda (name1) doM act2 ... actN) val1)
;;;     act2 ... actN
;;;
;;; for example:
;;;
;;; (writer-show
;;;  (doM >>=
;;;       (writer-record "And so it begins")
;;;       (<- foo (fetch "foo1"))
;;;       (writer-record "g'a foo")
;;;       (<- bar (fetch "bar1"))
;;;       (writer-record "g'a bar")
;;;       (= baz (sin (* 0.225 pi)))
;;;       (writer-record "g'THE baz")
;;;       (return (+ (* foo bar) baz))))

(defmacro (doM mbind act . acts)
  (let loop ((as (cons act acts)))
    (let ((a1 (car as)))
      (cond ((null? (cdr as)) a1)
	    ((eqv? '<- (car a1))
	     `(,mbind ,(caddr a1) (lambda (,(cadr a1)) ,(loop (cdr as)))))
	    ((eqv? '= (car a1))
	     `((lambda (,(cadr a1)) ,(loop (cdr as))) ,(caddr a1)))
	    (else
	     `(,mbind ,a1 (lambda (,(gensym)) ,(loop (cdr as)))))))))
