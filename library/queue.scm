;;; Queue: insert at front and back, remove at front

;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; (make-queue . args) alias (queue-create . args)
;;; (queue? obj)
;;; (queue-empty? q)
;;; (queue-insert-front q obj)
;;; (queue-insert-back q obj)
;;; (queue-remove q def)
;;; (queue-remove-all q)
;;; (queue-peek-front q def)
;;; (queue-peek-back q def)
;;; (queue-peek-nth q n def)
;;; (queue-size q)
;;; (queue-list q)
;;; (queue-sort q is-lt?)
;;; (queue-reverse q)
;;; (queue-append q1 q2)
;;; (queue-apply q proc)

;;; (load-library "struct.scm")

(namespace
 (make-queue
  queue-create
  queue?
  queue-empty?
  queue-insert-front
  queue-insert-back
  queue-remove
  queue-remove-all
  queue-peek-front
  queue-peek-back
  queue-peek-nth
  queue-size
  queue-list
  queue-sort
  queue-reverse
  queue-append
  queue-apply)

 ;;; ql are structs that provide list structure for queues: regular lists
 ;;; should work but don't, because set-c[ad]r! are broken. ql structs are
 ;;; little vectors inside, and that's good enough to un-break the failure
 ;;; mode of set-c[ad]r!

 ;;; cons	<-> make-ql
 ;;; car	<-> get-ql-val
 ;;; cdr	<-> get-ql-next
 ;;; set-car!	<-> set-ql-val!
 ;;; set-cdr!	<-> set-ql-next!
 ;;; ()		<-> ()
 ;;; null?	<-> null?

 (def-struct ql val next)

 (define (ql-reverse ql)
   (let loop ((ql ql)
	      (t ()))
     (if (null? ql)
	 t
	 (loop (get-ql-next ql) (make-ql (get-ql-val ql) t)))))

 (define (list->ql lst)
   (let loop ((l lst)
	      (ql ()))
     (if (null? l)
	 (ql-reverse ql)
	 (loop (cdr l) (make-ql (car l) ql)))))

 (define (ql->list ql)
   (let loop ((ql ql)
	      (lst ()))
     (if (null? ql)
	 (list-reverse lst)
	 (loop (get-ql-next ql) (cons (get-ql-val ql) lst)))))

;;; (load-library "struct.scm")
 (def-struct queue-struct front back)		;;; override struct name below

 (define (make-queue . args)
   (let ((q (make-queue-struct () ())))
     (vector-set! q 0 'queue)
     (unless (null? args)
       (let ((qla (list->ql args)))
	 (set-queue-struct-front! q qla)
	 (until (null? (get-ql-next qla))
		(set! qla (get-ql-next qla)))
	 (set-queue-struct-back! q qla)))
     q))

 (define queue-create make-queue)

 (define (queue? obj)
   (and (vector? obj) (eqv? (vector-ref obj 0) 'queue)))

 (display-object-hook 'queue
		      (lambda (q port)
			(write-string port "[queue")
			(let loop ((vs (get-queue-struct-front q)))
			  (if (null? vs)
			      (write-string port #\])
			      (begin (write-string port #\space)
				     (display (get-ql-val vs) port)
				     (loop (get-ql-next vs)))))))

 (define (queue-empty? q)
   (if (queue? q)
       (null? (get-queue-struct-front q))
       (raise (list "queue-empty? got a non-queue object" q))))

 (define (queue-insert-front q obj)
   (if (queue? q)
       (let* ((qf (get-queue-struct-front q))
	      (lo (make-ql obj qf)))
	 (when (null? qf)
	   (set-queue-struct-back! q lo))
	 (set-queue-struct-front! q lo))
       (raise (list "queue-insert-front got a non-queue object" q))))

 (define (queue-insert-back q obj)
   (if (queue? q)
       (let ((qf (get-queue-struct-front q)))
	 (if (null? qf)
	     (let ((lo (make-ql obj qf)))
	       (set-queue-struct-front! q lo)
	       (set-queue-struct-back! q lo))
	     (let ((lo (make-ql obj ())))
	       (set-ql-next! (get-queue-struct-back q) lo)
	       (set-queue-struct-back! q lo))))
       (raise (list "queue-insert-back got a non-queue object" q))))

 (define (queue-peek-front q def)
   (if (queue? q)
       (let ((lo (get-queue-struct-front q)))
	 (if (null? lo)
	     def
	     (get-ql-val lo)))
       (raise (list "queue-peek-front got a non-queue object" q))))

 (define (queue-peek-back q def)
   (if (queue? q)
       (let ((lo (get-queue-struct-back q)))
	 (if (null? lo)
	     def
	     (get-ql-val lo)))
       (raise (list "queue-peek-back got a non-queue object" q))))

 ;;; TODO: make negative n mean from the back?

 (define (queue-peek-nth q n def)
   (if (queue? q)
       (if (negative? n)
	   def
	   (let loop ((def def)
		      (lo (get-queue-struct-front q))
		      (n n))
	     (cond ((null? lo) def)
		   ((zero? n) (get-ql-val lo))
		   (else (loop def (get-ql-next lo) (- n 1))))))
       (raise (list "queue-peek-nth got a non-queue object" q))))

 (define (queue-size q)
   (if (queue? q)
       (let loop ((ql (get-queue-struct-front q))
		  (c 0))
	 (if (null? ql)
	     c
	     (loop (get-ql-next ql) (+ c 1))))
       (raise (list "queue-size got a non-queue object" q))))

 (define (queue-list q)
   (if (queue? q)
       (ql->list (get-queue-struct-front q))
       (raise (list "queue-list got a non-queue object" q))))

 (define (queue-remove q def)
   (if (queue? q)
       (let ((lo (get-queue-struct-front q)))
	 (if (null? lo)
	     def
	     (let ((lt (get-ql-next lo)))
	       (set-queue-struct-front! q lt)
	       (when (null? lt)
		 (set-queue-struct-back! q ()))
	       (get-ql-val lo))))
       (raise (list "queue-remove got a non-queue object" q))))

 (define (queue-remove-all q)
   (if (queue? q)
       (let ((lo (ql->list (get-queue-struct-front q))))
	 (set-queue-struct-front! q ())
	 (set-queue-struct-back! q ())
	 lo)
       (raise (list "queue-remove-all got a non-queue object" q))))

 (define (queue-sort q is-lt?)
   (if (queue? q)
       (let ((lo (list->ql (list-sort is-lt?
				      (ql->list (get-queue-struct-front q))))))
	 (set-queue-struct-front! q lo)
	 (unless (null? lo)
	   (until (null? (get-ql-next lo))
		  (set! lo (get-ql-next lo))))
	 (set-queue-struct-back! q lo))
       (raise (list "queue-sort got a non-queue object" q))))

 (define (queue-reverse q)
   (if (queue? q)
       (let ((lo (ql-reverse (get-queue-struct-front q))))
	 (set-queue-struct-front! q lo)
	 (unless (null? lo)
	   (until (null? (get-ql-next lo))
		  (set! lo (get-ql-next lo))))
	 (set-queue-struct-back! q lo))
       (raise (list "queue-reverse got a non-queue object" q))))

 (define (queue-append q1 q2)
   (if (and (queue? q1) (queue? q2))
       (begin (unless (null? (get-queue-struct-front q2))
		(if (null? (get-queue-struct-front q1))
		    (set-queue-struct-front! q1 (get-queue-struct-front q2))
		    (set-ql-next! (get-queue-struct-back q1)
				  (get-queue-struct-front q2)))
		(set-queue-struct-back! q1 (get-queue-struct-back q2))
		(set-queue-struct-front! q2 ())
		(set-queue-struct-back! q2 ()))
	      q1)
       (raise (list "queue-append got a non-queue object" q1 q2))))

 (define (queue-apply q proc)
   (if (queue? q)
       (let loop ((lo (get-queue-struct-front q)))
	 (if (null? lo)
	     q
	     (begin (set-ql-val! lo (proc (get-ql-val lo)))
		    (loop (get-ql-next lo)))))
       (raise (list "queue-apply got a non-queue object" q))))
)
