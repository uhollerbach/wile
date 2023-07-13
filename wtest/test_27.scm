;;; (defmacro (namespace syms . defs)
;;;   (let* ((nm (gensym))
;;; 	 (sa (gensym))
;;; 	 (s0 (map (lambda (s) (if (symbol? s) (list s s) s)) syms))
;;; 	 (s1 (map (lambda (s)
;;; 		    (let ((p (cadr s)))
;;; 		      `(define ,p #f))) s0))
;;; 	 (s2 (map (lambda (s)
;;; 		    (let ((p (car s)))
;;; 		      `((symbol=? ,sa ',p) ,p))) s0))
;;; 	 (s3 (map (lambda (s)
;;; 		    (let ((p1 (car s))
;;; 			  (p2 (cadr s)))
;;; 		      `(set! ,p2 (,nm ',p1)))) s0)))
;;;     `(begin ,@s1
;;; 	    (let ((,nm ((lambda ()
;;; 			  ,@defs
;;; 			  (lambda (,sa) (cond ,@s2))))))
;;; 	      ,@s3))))

(namespace
 (pub-change
  pub-val
  supervisor
  super-access)

 (define pri-val 43)

 (define (pri-change new-val)
   (printf "pri routine changing data from %v to %v\n" pri-val new-val)
   (let ((old-val pri-val))
     (set! pri-val new-val)
     old-val))
 
 (define pub-val "cool")

 (define (pub-change new-val)
   (printf "pub routine changing data from %v to %v\n" pub-val new-val)
   (let ((old-val pub-val))
     (set! pub-val new-val)
     old-val))

 (define (supervisor)
   (printf "pub data is %v\npri data is %v\n" pub-val pri-val))

 (define (super-access v-pri v-pub)
   (pri-change v-pri)
   (pub-change v-pub)))

(supervisor)
(printf "access pub data %v\n" pub-val)
(display (pub-change "groovy"))
(newline)
(printf "access pub data %v\n" pub-val)
(supervisor)

;;; In both skeem and wile, attempting to access the private parts is,
;;; correctly, a failure

;;; (supervisor)
;;; (printf "access pri data %v\n" pri-val)
;;; (display (pri-change 172))
;;; (newline)
;;; (printf "access pri data %v\n" pri-val)
;;; (supervisor)

(super-access 172 "protean")

;;; In both skeem and wile, these two direct accesses to pub-val don't
;;; work: they return the old obsolete value that was active at the
;;; time the namespace macro finished. This is because there are two
;;; different scopes in which there is a variable named pub-val, and
;;; this accesses the one outside the namespace.

(printf "access pub data %v\n" pub-val)
(supervisor)
(set! pub-val "mesmerizing")
(supervisor)
