;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; True double-ended queue

;;; (make-deque . args) alias (deque-create . args)
;;; (deque? obj)
;;; (deque-empty? q)
;;; (deque-insert-front q obj)
;;; (deque-insert-back q obj)
;;; (deque-remove-front q def)
;;; (deque-remove-back q def)
;;; (deque-remove-all q)
;;; (deque-peek-front q def)
;;; (deque-peek-back q def)
;;; (deque-peek-front2 q def)		;;; TODO names of these two routines?
;;; (deque-peek-back2 q def)
;;; (deque-peek-nth q n def)
;;; (deque-size q)
;;; (deque-list q)
;;; (deque-sort q is-lt?)
;;; (deque-reverse q)
;;; (deque-append q1 q2)
;;; The first version is for the values and updates each value in the deque,
;;; the second version is for side effects and does not modify the deque.
;;; (deque-apply q proc)
;;; (deque-for-each q proc)

;;; (load-library "struct.scm")

(namespace
 (make-deque
  deque-create
  deque?
  deque-empty?
  deque-insert-front
  deque-insert-back
  deque-peek-front
  deque-peek-back
  deque-peek-front2
  deque-peek-back2
  deque-remove-front
  deque-remove-back
  deque-size
  deque-list
  deque-remove-all
  deque-apply
  deque-for-each
  deque-reverse
  deque-sort)
  
;;; (load-library "struct.scm")
 (def-struct deque-struct front back)		;;; override struct name below
 (def-struct deque-int val next prev)		;;; next and prev are the neighboring
						;;; deque-int structs; this makes the
						;;; datastructure cyclic, hence the
						;;; need for the display hook below

 (define (make-deque . args)
   (let ((q (make-deque-struct () ())))
     (vector-set! q 0 'deque)
     (let loop ((vs args))
       (if (null? vs)
	   q
	   (begin (deque-insert-back q (car vs)) (loop (cdr vs)))))))

 (define deque-create make-deque)

 (define (deque? obj)
   (and (vector? obj) (eqv? (vector-ref obj 0) 'deque)))

 (display-object-hook 'deque
		      (lambda (q port)
			(write-string port "[deque")
			(let loop ((vs (get-deque-struct-front q)))
			  (if (null? vs)
			      (write-string port #\])
			      (begin
				(write-string port #\space)
				(display (get-deque-int-val vs) port)
				(loop (get-deque-int-next vs)))))))

 (define (deque-empty? q)
   (if (deque? q)
       (null? (get-deque-struct-front q))
       (raise "deque-empty? got a non-deque object")))

 (define (deque-insert-front q obj)
   (if (deque? q)
       (let ((bo (make-deque-int obj () ()))
	     (qh (get-deque-struct-front q)))
	 (if (null? qh)
	     (set-deque-struct-back! q bo)
	     (begin (set-deque-int-next! bo qh)
		    (set-deque-int-prev! qh bo)))
	 (set-deque-struct-front! q bo))
       (raise "deque-insert-front got a non-deque object")))

 (define (deque-insert-back q obj)
   (if (deque? q)
       (let ((bo (make-deque-int obj () ()))
	     (qh (get-deque-struct-back q)))
	 (if (null? qh)
	     (set-deque-struct-front! q bo)
	     (begin (set-deque-int-prev! bo qh)
		    (set-deque-int-next! qh bo)))
	 (set-deque-struct-back! q bo))
       (raise "deque-insert-front got a non-deque object")))

 (define (deque-peek-front q def)
   (if (deque? q)
       (let ((lo (get-deque-struct-front q)))
	 (if (null? lo)
	     def
	     (get-deque-int-val lo)))
       (raise "deque-peek-front got a non-deque object")))

 (define (deque-peek-back q def)
   (if (deque? q)
       (let ((lo (get-deque-struct-back q)))
	 (if (null? lo)
	     def
	     (get-deque-int-val lo)))
       (raise "deque-peek-back got a non-deque object")))

 (define (deque-peek-front2 q def)
   (if (deque? q)
       (let ((l1 (get-deque-struct-front q)))
	 (if (null? l1)
	     def
	     (let ((l2 (get-deque-int-next l1)))
	       (if (null? l2)
		   def
		   (get-deque-int-val l2)))))
       (raise "deque-peek-front2 got a non-deque object")))

 (define (deque-peek-back2 q def)
   (if (deque? q)
       (let ((l1 (get-deque-struct-back q)))
	 (if (null? l1)
	     def
	     (let ((l2 (get-deque-int-prev l1)))
	       (if (null? l2)
		   def
		   (get-deque-int-val l2)))))
       (raise "deque-peek-back2 got a non-deque object")))

 (define (deque-remove-front q def)
   (if (deque? q)
       (let ((lo (get-deque-struct-front q)))
	 (if (null? lo)
	     def
	     (begin (set-deque-struct-front! q (get-deque-int-next lo))
		    (if (null? (get-deque-struct-front q))
			(set-deque-struct-back! q ())
			(set-deque-int-prev! (get-deque-struct-front q) ()))
		    (get-deque-int-val lo))))
       (raise "deque-remove-front got a non-deque object")))

 (define (deque-remove-back q def)
   (if (deque? q)
       (let ((lo (get-deque-struct-back q)))
	 (if (null? lo)
	     def
	     (begin (set-deque-struct-back! q (get-deque-int-prev lo))
		    (if (null? (get-deque-struct-back q))
			(set-deque-struct-front! q ())
			(set-deque-int-next! (get-deque-struct-back q) ()))
		    (get-deque-int-val lo))))
       (raise "deque-remove-back got a non-deque object")))

 (define (deque-size q)
   (if (deque? q)
       (let loop ((vs (get-deque-struct-front q))
		  (n 0))
	 (if (null? vs)
	     n
	     (loop (get-deque-int-next vs) (+ n 1))))
       (raise "deque-size got a non-deque object")))

 (define (deque-list q)
   (if (deque? q)
       (let loop ((vs (get-deque-struct-back q))
		  (ls ()))
	 (if (null? vs)
	     ls
	     (loop (get-deque-int-prev vs) (cons (get-deque-int-val vs) ls))))
       (raise "deque-list got a non-deque object")))

 (define (deque-remove-all q)
   (if (deque? q)
       (let ((lo (deque-list q)))
	 (set-deque-struct-front! q ())
	 (set-deque-struct-back! q ())
	 lo)
       (raise "deque-remove-all got a non-deque object")))

 (define (deque-apply q proc)
   (if (deque? q)
       (let loop ((vs (get-deque-struct-front q)))
	 (if (null? vs)
	     q
	     (begin (set-deque-int-val! vs (proc (get-deque-int-val vs)))
		    (loop (get-deque-int-next vs)))))
       (raise "deque-apply got a non-deque object")))

 (define (deque-for-each q proc)
   (if (deque? q)
       (let loop ((vs (get-deque-struct-front q)))
	 (if (null? vs)
	     q
	     (begin (proc (get-deque-int-val vs))
		    (loop (get-deque-int-next vs)))))
       (raise "deque-apply got a non-deque object")))

 (define (deque-reverse q)
   (if (deque? q)
       (let loop ((vs (deque-remove-all q)))
	 (if (null? vs)
	     q
	     (begin (deque-insert-front q (car vs))
		    (loop (cdr vs)))))
       (raise "deque-reverse got a non-deque object")))

 (define (deque-sort q is-lt?)
   (if (deque? q)
       (let loop ((vs (list-sort is-lt? (deque-remove-all q))))
	 (if (null? vs)
	     q
	     (begin (deque-insert-back q (car vs))
		    (loop (cdr vs)))))
       (raise "deque-sort got a non-deque object")))

;;; TODO: from here on down
 (define (deque-peek-nth q n def)
   (raise "deque-peek-nth not implemented yet")
   (if (deque? q)
       (if (negative? n)
	   def
	   (let loop ((lo (get-deque-struct-front q))
		      (n n))
	     (cond ((null? lo) def)
		   ((zero? n) (car lo))
		   (else (loop (cdr lo) (- n 1))))))
       (raise "deque-peek-nth got a non-deque object")))

 (define (deque-append q1 q2)
   (raise "deque-append not implemented yet")
   (if (and (deque? q1) (deque? q2))
       (begin (unless (null? (vector-ref q2 1))
		(if (null? (vector-ref q1 1))
		    (vector-set! q1 1 (vector-ref q2 1))
		    (set-cdr! (vector-ref q1 2) (vector-ref q2 1)))
		(vector-set! q1 2 (vector-ref q2 2))
		(vector-set! q2 1 ())
		(vector-set! q2 2 ()))
	      q1)
       (raise "deque-append got a non-deque object")))
)
