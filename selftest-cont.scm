;;; various tests of delimited continuations (and, since these are currently
;;; implemented as macros around call/cc, tests of call/cc)

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

; for mzscheme, replace the above definition of sandbox with
;	(define (sandbox . expr) #t)
; it won't catch exceptions, but then mzscheme doesn't produce
; any, except for the intentional syntax error at the end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(write-string "all answers are checked against mzscheme\n")

(write-string "examples from community.schemewiki.org\n")

(write-string "should be 6\t\t")
(sandbox
 (display (+ 1 (reset (+ 2 3))))
 (newline))

(write-string "should be (1 2)\t\t")
(sandbox
 (display (cons 1 (reset (cons 2 '()))))
 (newline))

(write-string "should be 4\t\t")
(sandbox
 (display (+ 1 (reset (+ 2 (shift k 3)))))
 (newline))

(write-string "should be (1 3)\t\t")
(sandbox
 (display (cons 1 (reset (cons 2 (shift k (cons 3 '()))))))
 (newline))

(write-string "should be 10\t\t")
(sandbox
 (display (+ 1 (reset (+ 2 (shift k (+ 3 (k 4)))))))
 (newline))

(write-string "should be 14\t\t")
(sandbox
 (display (+ 1 (reset (+ 2 (shift k (+ 3 (k 5) (k 1)))))))
 (newline))

(write-string "should be (1 3 2 4)\t")
(sandbox
 (display (cons 1 (reset (cons 2 (shift k (cons 3 (k (cons 4 '()))))))))
 (newline))

(write-string "should be (1 3 2 2 4)\t")
(sandbox
 (display (cons 1 (reset (cons 2 (shift k (cons 3 (k (k (cons 4 '())))))))))
 (newline))

(write-string "multiple shifts\n")

; wile fails these - all work, but then raise an exception, bad type

(write-string "should be 2\t\t")
(sandbox
 (reset (display (begin (shift k1 (k1 1)) (shift k2 (k2 2)))))
 (newline))


; haskeem fails this -- restart issue
(write-string "should be 12\t\t")
(sandbox
 (reset (begin (display (shift k1 (k1 1))) (display (shift k2 (k2 2)))))
 (newline))

(write-string "should be 12\t\t")
(sandbox
 (reset (display (shift k1 (k1 1))) (display (shift k2 (k2 2))))
 (newline))

(write-string "should be 134234\t")
(sandbox
 (reset (begin (shift k1 (begin (k1 (display 1)) (k1 (display 2))))
	       (shift k2 (begin (k2 (display 3)) (k2 (display 4))))))
 (newline))

(write-string "should be 134234\t")
(sandbox
 (reset (begin (shift k1 (k1 (display 1)) (k1 (display 2)))
	       (shift k2 (k2 (display 3)) (k2 (display 4)))))
 (newline))

(write-string "should be 134234\t")
(sandbox
 (reset (shift k1 (k1 (display 1)) (k1 (display 2)))
	(shift k2 (k2 (display 3)) (k2 (display 4))))
 (newline))

(write-string "should be 13564562356456\n\t  ")
(sandbox
 (reset (begin (shift k1 (begin (k1 (display 1)) (k1 (display 2))))
	       (shift k2 (begin (k2 (display 3)) (k2 (display 4))))
	       (shift k3 (begin (k3 (display 5)) (k3 (display 6))))))
 (write-string "\n\t  ")
 (reset (begin (shift k1 (k1 (display 1)) (k1 (display 2)))
	       (shift k2 (k2 (display 3)) (k2 (display 4)))
	       (shift k3 (k3 (display 5)) (k3 (display 6)))))
 (write-string "\n\t  ")
 (reset (shift k1 (k1 (display 1)) (k1 (display 2)))
	(shift k2 (k2 (display 3)) (k2 (display 4)))
	(shift k3 (k3 (display 5)) (k3 (display 6))))
 (newline))

(write-string "should be 57\t\t")
(sandbox
 (display (+ 1 (reset (+ 2 (shift k1 (+ 3
					(k1 5)
					(k1 1)
					(reset (* 10 (shift k2 (+ 3 (k2 4)))))
					))))))
 (newline))

(write-string "should be 59\t\t")
(sandbox
 (display (+ 1 (reset (+ 2 (shift k1 (+ 3
					(k1 5)
					(k1 1)
					(reset (* 10 (shift k2 (+ (k1 3)
								  (k2 4)))))
					))))))
 (newline))

; haskeem fails the following nine -- restart issue

(write-string "should be 1 3 2\t\t")
(sandbox
 (reset (begin (write-string "1 ")
	       (shift c (begin (c 'ignore)
			       (write-string "2 ")))
	       (write-string "3 ")))
 (newline))

(write-string "should be 1 3 2\t\t")
(sandbox
 (reset (begin (write-string "1 ")
	       (shift c (c 'ignore) (write-string "2 "))
	       (write-string "3 ")))
 (newline))
(write-string "should be 1 3 2\t\t")
(sandbox
 (reset (write-string "1 ")
	(shift c (c 'ignore) (write-string "2 "))
	(write-string "3 "))
 (newline))

(write-string "should be 1 2 3\t\t")
(sandbox
 (reset (begin (write-string "1 ")
	       (shift c (begin (write-string "2 ")
			       (c 'ignore)))
	       (write-string "3 ")))
 (newline))
(write-string "should be 1 2 3\t\t")
(sandbox
 (reset (begin (write-string "1 ")
	       (shift c (write-string "2 ") (c 'ignore))
	       (write-string "3 ")))
 (newline))
(write-string "should be 1 2 3\t\t")
(sandbox
 (reset (write-string "1 ")
	(shift c (write-string "2 ") (c 'ignore))
	(write-string "3 "))
 (newline))

(write-string "should be 1 3 2 3\t")
(sandbox
 (reset (begin (write-string "1 ")
	       (shift c (begin (c 'ignore)
			       (write-string "2 ")
			       (c 'ignore)))
	       (write-string "3 ")))
 (newline))
(write-string "should be 1 3 2 3\t")
(sandbox
 (reset (begin (write-string "1 ")
	       (shift c (c 'ignore) (write-string "2 ") (c 'ignore))
	       (write-string "3 ")))
 (newline))
(write-string "should be 1 3 2 3\t")
(sandbox
 (reset (write-string "1 ")
	(shift c (c 'ignore) (write-string "2 ") (c 'ignore))
	(write-string "3 "))
 (newline))

(write-string "from Shan, \"Shift to Control\"\n")

(write-string "should be (a)\t\t")
(sandbox
 (display (reset (cons 'a (reset (shift f (shift g '()))))))
 (newline))

;;;	; wile fails these, raises exception, bad type
;;;	(write-string "should be ()\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (reset0-toplevel
;;;	   (reset0 (cons 'a (reset0 (shift0 f (shift0 g '())))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be (a)\t\t")
;;;	(sandbox
;;;	 (display (reset (let ((y (shift f (cons 'a (f '()))))) (shift g y))))
;;;	 (newline))
;;;	
;;;	(write-string "should be ()\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt-toplevel
;;;	   (prompt (let ((y (control f (cons 'a (f '()))))) (control g y)))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 132342344234442344442344444234444442344...\n\t  ")
;;;	(write-string "it passes! but it's an infinite loop, skip for now\n")
;;;	;; (sandbox
;;;	;;  (prompt (begin (control f (begin (f (display 1)) (f (display 2))))
;;;	;; 		(control f (begin (f (display 3)) (f (display 4))))))
;;;	;;  (newline))
;;;	
;;;	
;;;	(write-string "should be (a)\t\t")
;;;	(sandbox
;;;	 (display (reset (let ((y (shift f (cons 'a (f '()))))) (shift g y))))
;;;	 (newline))
;;;	
;;;	(write-string "should be (a)\t")
;;;	(sandbox
;;;	 (display
;;;	  (reset0-toplevel
;;;	   (reset0 (let ((y (shift0 f (cons 'a (f '()))))) (shift0 g y)))))
;;;	 (newline))
;;;	
;;;	(write-string "should be ()\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt-toplevel
;;;	   (prompt (let ((y (control f (cons 'a (f '()))))) (control g y)))))
;;;	 (newline))
;;;	
;;;	(write-string "should be () XXX\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt0-toplevel
;;;	   (prompt0 (let ((y (control0 f (cons 'a (f '()))))) (control0 g y)))))
;;;	 (newline))
;;;	

(write-string "should be (a b)\t\t")
(sandbox
 (display
  (reset
   (reset (cons 'a (reset
		    (let ((y (shift f (shift g (cons 'b (f '()))))))
		      (shift h y)))))))
 (newline))

;;;	(write-string "should be (a)\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt-toplevel
;;;	   (prompt
;;;	    (prompt (cons 'a (prompt
;;;			      (let ((y (control f (control g (cons 'b (f '()))))))
;;;				(control h y))))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be (b)\t")
;;;	(sandbox
;;;	 (display
;;;	  (reset0-toplevel
;;;	   (reset0
;;;	    (reset0 (cons 'a (reset0
;;;			      (let ((y (shift0 f (shift0 g (cons 'b (f '()))))))
;;;				(shift0 h y))))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be ()\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt0-toplevel
;;;	   (prompt0
;;;	    (prompt0 (cons 'a (prompt0
;;;			       (let ((y (control0 f (control0 g (cons 'b (f '()))))))
;;;				 (control0 h y))))))))
;;;	 (newline))

(write-string "two-armed tests\n")
(write-string "test 1: non-trivial shift only in false arm of if\n")
(define (doit1 flag)
  (if flag
      (write-string "should be (1 3)\t\t")
      (write-string "should be (1 4 2 5)\t"))
  (sandbox
   (display
    (cons 1 (reset (cons 2 (if flag
			       (shift k1 (cons 3 '()))
			       (shift k2 (cons 4 (k2 (cons 5 '())))))))))
   (newline)))

(doit1 #f)
(doit1 #t)

(write-string "test 2: reverse of previous\n")
(define (doit2 flag)
  (if flag
      (write-string "should be (1 4 2 5)\t")
      (write-string "should be (1 3)\t\t"))
  (sandbox
   (display
    (cons 1 (reset (cons 2 (if flag
			       (shift k2 (cons 4 (k2 (cons 5 '()))))
			       (shift k1 (cons 3 '())))))))
   (newline)))

(doit2 #f)
(doit2 #t)

(write-string "test 3: expansion of test 1\n")

(define (doit3 flag)
  (write-string "should be (1 4 3)\t")	; independent of flag
  (sandbox
   (display
    (cons 1
	  (let ((k2 (lambda (v2)
		      (let ((k1 (lambda (v1)
				  (reset (cons 2 (if flag v1 v2))))))
			(cons 3 '())))))
	    (reset (cons 4 (k2 (cons 5 '())))))))
   (newline)))

(doit3 #f)
(doit3 #t)

(write-string "test 4: expansion of test 2\n")
(define (doit4 flag)
  (if flag
      (write-string "should be (1 4 2 3)\t")
      (write-string "should be (1 4 2 5)\t"))
  (sandbox
   (display
    (cons 1
	  (let ((k2 (lambda (v2)
		      (reset (cons 2 (if flag (cons 3 '()) v2))))))
	    (reset (cons 4 (k2 (cons 5 '())))))))
   (newline)))

(doit4 #f)
(doit4 #t)

(write-string "test 5: modified version, with (shift) in both arms of (if)\n")
(define (doit5 flag)
  (if flag
      (write-string "should be (1 3 2 4)\t")
      (write-string "should be (1 5 2 6)\t"))
  (sandbox
   (display
    (cons 1
	  (reset (cons 2 (if flag
			     (shift k1 (cons 3 (k1 (cons 4 '()))))
			     (shift k2 (cons 5 (k2 (cons 6 '())))))))))
   (newline)))

(doit5 #f)
(doit5 #t)

(write-string
 "test 6: modified version of test 5, with (if) lifted out of (reset)\n")

(define (doit6 flag)
  (if flag
      (write-string "should be (1 3 2 4)\t")
      (write-string "should be (1 5 2 6)\t"))
  (sandbox
   (display
    (cons 1
	  (if flag
	      (reset (cons 2 (shift k1 (cons 3 (k1 (cons 4 '()))))))
	      (reset (cons 2 (shift k2 (cons 5 (k2 (cons 6 '())))))))))
   (newline)))

(doit6 #f)
(doit6 #t)

(write-string "examples from Queinnec, augmented to test all versions\n")

;;;	(write-string "prompt/control examples\n")
;;;	(write-string "should be 3\t\t")
;;;	(sandbox
;;;	 (display (prompt-toplevel (prompt (* 2 (control f 3)))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 30\t\t")
;;;	(sandbox
;;;	 (display (prompt-toplevel (prompt (* 2 (control f (* 5 (f 3)))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 12\t\t")
;;;	(sandbox
;;;	 (display (prompt-toplevel (prompt (* 2 (control f (f (f 3)))))))
;;;	 (newline))

;;; this test is really interesting... it fails in skeem with a
;;; "no top-level reset" exception, then it causes the previous tests
;;; to be repeated, in the pattern
;;; ... 3 30 12 6<fail1>
;;;       30 12 6<fail2>
;;;          12 6<fail3>
;;;             6<fail4>
;;; and then tests continue below at 35 ...
;;; most fascinating!
;;; update: fixed with introduction of prompt-toplevel
;;;	
;;;	(write-string "should be 6\t\t")
;;;	(sandbox
;;;	 (display (prompt-toplevel ((prompt (* 2 (control f f))) 3)))
;;;	 (newline))
;;;	
;;;	(write-string "should be 35\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt-toplevel
;;;	   (prompt (* 5
;;;		      (prompt (* 2
;;;				 (control f2 (* 3 (control f3 7)))))))))
;;;	 (newline))

;;;	(write-string "should be 7\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt-toplevel
;;;	   (prompt (* 5 (
;;;			 (prompt (* 2
;;;				    (control f2 (lambda ()
;;;						  (* 3 (control f3 7)))))))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 21\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt-toplevel
;;;	   (prompt (* 5
;;;		      ((lambda (x) (control f1 x))
;;;		       (* 3 (control f2 (* 2 (f2 7)))))))))
;;;	 (newline))
;;;	
;;;	(write-string "prompt0/control0 examples\n")
;;;	(write-string "should be 3\t\t")
;;;	(sandbox
;;;	 (display (prompt0-toplevel (prompt0 (* 2 (control0 f 3)))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 30\t\t")
;;;	(sandbox
;;;	 (display (prompt0-toplevel (prompt0 (* 2 (control0 f (* 5 (f 3)))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 12\t\t")
;;;	(sandbox
;;;	 (display (prompt0-toplevel (prompt0 (* 2 (control0 f (f (f 3)))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 6\t\t")
;;;	(sandbox
;;;	 (display (prompt0-toplevel ((prompt0 (* 2 (control0 f f))) 3)))
;;;	 (newline))
;;;	
;;;	(write-string "should be 7\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt0-toplevel
;;;	   (prompt0 (* 5
;;;		       (prompt0 (* 2
;;;				   (control0 f2 (* 3 (control0 f3 7)))))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 7\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt0-toplevel
;;;	   (prompt0 (* 5 (
;;;			  (prompt0 (* 2
;;;				      (control0 f2 (lambda ()
;;;						     (* 3 (control0 f3 7)))))))))))
;;;	 (newline))
;;;	
;;;	; There's something odd with this test: in mzscheme, it prints the
;;;	; result 21, then the whole rest of the test vanishes; apparently
;;;	; enough of the surrounding context is sucked up that the rest of the
;;;	; world just vanishes. If I surround this with a (reset ...), the
;;;	; test continues, but this doesn't print anything. Hmmm!
;;;	
;;;	(write-string "should be 21 XXX\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (prompt0-toplevel
;;;	   (prompt0 (* 5
;;;		       ((lambda (x) (control0 f1 x))
;;;			(* 3 (control0 f2 (* 2 (f2 7)))))))))
;;;	 (newline))
;;;	
(write-string "reset/shift examples\n")
(write-string "should be 3\t\t")
(sandbox
 (display (reset (* 2 (shift f 3))))
 (newline))

(write-string "should be 30\t\t")
(sandbox
 (display (reset (* 2 (shift f (* 5 (f 3))))))
 (newline))

(write-string "should be 12\t\t")
(sandbox
 (display (reset (* 2 (shift f (f (f 3))))))
 (newline))

;;;	(write-string "should be 6\t\t")
;;;	(sandbox
;;;	 (display ((reset (* 2 (shift f f))) 3))
;;;	 (newline))

(write-string "should be 35\t\t")
(sandbox
 (display (reset (* 5
		    (reset (* 2
			      (shift f2 (* 3 (shift f3 7))))))))
 (newline))

(write-string "should be 7\t\t")
(sandbox
 (display (reset (* 5 (
		       (reset (* 2
				 (shift f2 (lambda ()
					     (* 3 (shift f3 7))))))))))
 (newline))

(write-string "should be 42\t\t")
(sandbox
 (display (reset (* 5
		    ((lambda (x) (shift f1 x))
		     (* 3 (shift f2 (* 2 (f2 7))))))))
 (newline))

;;;	(write-string "reset0/shift0 examples\n")
;;;	(write-string "should be 3\t\t")
;;;	(sandbox
;;;	 (display (reset0-toplevel (reset0 (* 2 (shift0 f 3)))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 30\t\t")
;;;	(sandbox
;;;	 (display (reset0-toplevel (reset0 (* 2 (shift0 f (* 5 (f 3)))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 12\t\t")
;;;	(sandbox
;;;	 (display (reset0-toplevel (reset0 (* 2 (shift0 f (f (f 3)))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 6\t\t")
;;;	(sandbox
;;;	 (display (reset0-toplevel ((reset0 (* 2 (shift0 f f))) 3)))
;;;	 (newline))

;;;	(write-string "should be 7\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (reset0-toplevel
;;;	   (reset0 (* 5
;;;		      (reset0 (* 2
;;;				 (shift0 f2 (* 3 (shift0 f3 7)))))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 7\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (reset0-toplevel
;;;	   (reset0 (* 5 (
;;;			 (reset0 (* 2
;;;				    (shift0 f2 (lambda ()
;;;						 (* 3 (shift0 f3 7)))))))))))
;;;	 (newline))
;;;	
;;;	(write-string "should be 42\t\t")
;;;	(sandbox
;;;	 (display
;;;	  (reset0-toplevel
;;;	   (reset0 (* 5
;;;		      ((lambda (x) (shift0 f1 x))
;;;		       (* 3 (shift0 f2 (* 2 (f2 7)))))))))
;;;	 (newline))

(define (doit7 flag)
  (sandbox
   (display
    (cons 1
	  (reset (cons 2 (if flag
			     (shift k1 (set! flag (not flag))
				    (cons 3 (k1 (cons 4 '()))))
			     (shift k2 (set! flag (not flag))
				    (cons 5 (k2 (cons 6 '())))))))))
   (newline)))

(write-string "should be (1 5 2 6)\t")
(doit7 #f)

(write-string "should be (1 3 2 4)\t")
(doit7 #t)

;;;	(write-string "\nsomething actually useful! a small table of factorials\n")
;;;	
;;;	; Just to show I've got nothing up my sleeve, here is a perfectly cromulent
;;;	; function; we could equally well use the built-in (factorial) function
;;;	
;;;	(define (my-fact n)
;;;	  (if (<= n 0)
;;;	      1
;;;	      (* n (my-fact (- n 1)))))
;;;	
;;;	(define from 1)
;;;	(define to 21)
;;;	(define step 3)
;;;	
;;;	(sandbox
;;;	 (reset (display (my-fact 
;;;			  (shift f
;;;				 (do ((i from (+ i step)))
;;;				     ((> i to) #f)
;;;				   (write-string (number->string i))
;;;				   (write-string "! =\t")
;;;				   (f i)))))
;;;		(newline)))
;;;	
;;;	
;;;	; Eventually we want to be able to use code like this:
;;;	
;;;	(defmacro (range from step to)
;;;	  (let ((f (gensym))
;;;		(i (gensym)))
;;;	    `(shift ,f
;;;		    (do ((,i ,from (+ ,i ,step)))
;;;			((> ,i ,to) #f)
;;;		      (,f ,i)))))
;;;	
;;;	;;; aaand: amazing! it woiks!
;;;	
;;;	(write-string "\nclean version, with range macro (slightly different format)\n")
;;;	
;;;	(reset (begin (display (my-fact (range from step to))) (newline)))
;;;	
;;;	(write-string
;;;	 "\nsyntax error coming up! should be an error... this'll kill mzscheme,\n")
;;;	(write-string
;;;	 "but (ha)?skeem should catch the exception raised by the syntax error\n")
;;;	
;;;	(sandbox (display (+ 1 (reset (+ 2 (shift (+ k 1) 3))))))

(write-string "\nIt's alive! It's ALIVE!! IT'S ALIIIIVE!!!\n")
