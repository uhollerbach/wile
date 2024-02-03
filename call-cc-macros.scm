;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

;;; for mzscheme, replace the definition of sandbox with
;;;	(define (sandbox . expr) #t)
;;; it won't catch exceptions, but then mzscheme doesn't produce
;;; any, except for the intentional syntax error at the end

(defmacro (sandbox . expr)
  (let ((err (gensym)))
    `(guard
      (,err ((begin (write-string "caught exception '")
		    (display ,err)
		    (write-string "'\n")
		    #t)))
      ,@expr)))

;;; reset/shift

(define *meta-cont*
  (lambda (value)
    (raise (list "no top-level reset" value))))

(defmacro (reset . body)
  (let ((m (gensym))
	(k (gensym))
	(v (gensym))
	(r (gensym)))
    `(let ((,m *meta-cont*))
       (call/cc
	(lambda (,k)
	  (set! *meta-cont* (lambda (,v) (set! *meta-cont* ,m) (,k ,v)))
	  (let ((,r (begin ,@body)))
	    (*meta-cont* ,r)))))))

(defmacro (shift var . body)
  (let ((k (gensym))
	(r (gensym))
	(v (gensym)))
    `(call/cc
      (lambda (,k)
	(let ((,r (let ((,var (lambda (,v) (reset (,k ,v)))))
		    ,@body)))
	  (*meta-cont* ,r))))))

;;; prompt/control -- these can be trickier, since more context gets
;;; swallowed: one test hits the "no top-level reset" meta-cont,
;;; fixed with addition of prompt-toplevel

(define (prompt-send v)
  (lambda (mc) (if mc ((mc v) #f) v)))

(define (prompt-compose c mc1)
  (if mc1
      (lambda (v) (lambda (mc2) ((c v) (prompt-compose mc1 mc2))))
      c))

(defmacro (prompt . body)
  (let ((c (gensym)))
    `(shift ,c (,c ((reset (prompt-send (begin ,@body))) #f)))))

(defmacro (prompt-toplevel . body)
  `((reset (prompt-send (begin ,@body))) #f))

(defmacro (control f . body)
  (let ((c1 (gensym))
	(c2 (gensym))
	(mc1 (gensym))
	(mc2 (gensym)))
    `(shift ,c1
	    (lambda (,mc1)
	      (let ((,f (lambda (x)
			  (shift ,c2
				 (lambda (,mc2)
				   (((prompt-compose ,c1 ,mc1) x)
				    (prompt-compose ,c2 ,mc2)))))))
		((reset (prompt-send (begin ,@body))) #f))))))

;;; reset0/shift0

(define (reset0-propagate v)
  (lambda (lc)
    (if (null? lc)
	v
	(((car lc) v) (cdr lc)))))

(defmacro (reset0-toplevel . body)
  `((reset (reset0-propagate (begin ,@body))) '()))

(defmacro (reset0 . body)
  (let ((c (gensym))
	(lc (gensym)))
    `(shift ,c
	    (lambda (,lc)
	      ((reset (reset0-propagate (begin ,@body))) (cons ,c ,lc))))))

(defmacro (shift0 f . body)
  (let ((c1 (gensym))
	(c2 (gensym))
	(lc1 (gensym))
	(lc2 (gensym))
	(x (gensym)))
    `(shift ,c1
	    (lambda (,lc1)
	      (let ((,f (lambda (,x)
			  (shift ,c2
				 (lambda (,lc2)
				   ((,c1 ,x) (cons ,c2 ,lc2)))))))
		((reset ((car ,lc1) (begin ,@body))) (cdr ,lc1)))))))

;;; prompt0/control0 -- two tests still failing, car gets a bad argument

(define (prompt0-send-propagate v)
  (lambda (mc)
    (if mc
	((mc v) #f)
	(lambda (lc)
	  (if (null? lc)
	      v
	      ((((car lc) v) #f) (cdr lc)))))))

(defmacro (prompt0 . body)
  (let ((c (gensym))
	(mc (gensym))
	(lc (gensym)))
    `(shift ,c
	    (lambda (,mc)
	      (lambda (,lc)
		(((reset (prompt0-send-propagate (begin ,@body))) #f)
		 (cons (prompt-compose ,c ,mc) ,lc)))))))

(defmacro (prompt0-toplevel . body)
  `(((reset (prompt0-send-propagate (begin ,@body))) #f) '()))

(defmacro (control0 f . body)
  (let ((c1 (gensym))
	(c2 (gensym))
	(mc1 (gensym))
	(mc2 (gensym))
	(lc (gensym))
	(x (gensym)))
    `(shift ,c1
	    (lambda (,mc1)
	      (lambda (,lc)
		(let ((,f (lambda (,x)
			    (shift ,c2
				   (lambda (,mc2)
				     (((prompt-compose ,c1 ,mc1) ,x)
				      (prompt-compose ,c2 ,mc2)))))))
		  (((reset ((car ,lc) (begin ,@body))) #f)
		   (cdr ,lc))))))))
