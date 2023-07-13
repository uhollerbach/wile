;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; Stack data structure: a tiny bit more object-like than lists

;;; (make-stack . args)    alias stack-create
;;; (stack? obj)
;;; (stack-empty? s)
;;; (stack-push s v)
;;; (stack-pop s def)
;;; (stack-top s def)
;;; (stack-size s)
;;; (stack-list s)
;;; (stack-reverse s)
;;; (stack-peek-nth s k def)

;;; (load-library "struct.scm")

(def-struct stack-struct top)		;;; override struct name below

(define (make-stack . args)
  (let ((s (make-stack-struct ())))
    (vector-set! s 0 'stack)
    (set-stack-struct-top! s args)
    s))

(define stack-create make-stack)

(define (stack? obj)
  (and (vector? obj) (eqv? (vector-ref obj 0) 'stack)))

(display-object-hook 'stack
 (lambda (s port)
   (write-string port "[stack")
   (let loop ((vs (get-stack-struct-top s)))
     (if (null? vs)
	 (write-string port #\])
	 (begin (write-string port #\space)
		(display (car vs) port)
		(loop (cdr vs)))))))

(define (stack-empty? s)
  (if (stack? s)
      (null? (get-stack-struct-top s))
      (raise (list "stack-empty? got a non-stack object" s))))

(define (stack-push s v)
  (if (stack? s)
      (begin
	(set-stack-struct-top! s (cons v (get-stack-struct-top s)))
	s)
      (raise (list "stack-push got a non-stack object" s))))

(define (stack-pop s def)
  (if (stack? s)
      (let ((vs (get-stack-struct-top s)))
	(if (null? vs)
	    def
	    (begin
	      (set-stack-struct-top! s (cdr vs))
	      (car vs))))
      (raise (list "stack-pop got a non-stack object" s))))

(define (stack-top s def)
  (if (stack? s)
      (let ((vs (get-stack-struct-top s)))
	(if (null? vs)
	    def
	    (car vs)))
      (raise (list "stack-top got a non-stack object" s))))

(define (stack-size s)
  (if (stack? s)
      (list-length (get-stack-struct-top s))
      (raise (list "stack-size got a non-stack object" s))))

(define (stack-list s)
  (if (stack? s)
      (get-stack-struct-top s)
      (raise (list "stack-list got a non-stack object" s))))

(define (stack-reverse s)
  (if (stack? s)
      (begin
	(set-stack-struct-top! s (list-reverse (get-stack-struct-top s)))
	s)
      (raise (list "stack-reverse got a non-stack object" s))))

(define (stack-peek-nth s k def)
  (if (stack? s)
      (let ((vs (get-stack-struct-top s)))
	(if (or (negative? k) (> k (list-length vs)))
	    def
	    (list-ref vs k)))
      (raise (list "stack-peek-nth got a non-stack object" s))))
