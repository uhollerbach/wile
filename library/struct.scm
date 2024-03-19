;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; Structures: (def-struct name fields) expands into definitions of:
;;;
;;;     * constructor (make-<name> field-values)
;;;     * type predicate (isa-<name>? object),
;;;     * getters (get-<name>-<field> object)
;;;     * setters (set-<name>-<field>! object value)
;;;
;;; for all the fields: a total of 2 + 2*#fields functions.
;;;
;;; What is a struct? a vector of length 1 + #fields: slot 0 is the
;;; identifier for the struct type, just the name symbol, and slot j
;;; is the jth field, counting from 1.
;;;
;;; Possible enhancements: make the getters and setters type-check the
;;; input object; allow restriction of the types of the fields, ie
;;; year:integer or some such.
;;;
;;; See struct-test.scm for a simple example.

(defmacro (def-struct name field . fields)
  (let* ((info (let loop ((ss (cons field fields))
			  (ix 0)
			  (acc ()))
		 (if (null? ss)
		     (list-reverse acc)
		     (let* ((sc (car ss))
			    (lc (if (symbol? sc) 1 (cadr sc)))
			    (nc (if (symbol? sc) sc (car sc))))
		       (loop (cdr ss) (+ ix lc)
			     (cons (list nc (+ ix 1) lc) acc))))))
	 (fs (map car info))
	 (ss (map cadr info))
	 (ls (map caddr info))
	 (lfs (list-length fs))
	 (len (let ((lf (list-last info)))
		(+ (cadr lf) (caddr lf))))
	 (J0 (lambda strs (apply string-append strs)))
	 (J1 (lambda (pre main) (string->symbol (J0 pre main))))
	 (J2 (lambda (pre main post) (string->symbol (J0 pre main post))))
	 (nstr (symbol->string name))
	 (mstr (J1 "make-" nstr))
	 (istr (J2 "isa-" nstr "?"))
	 (gpre (J0 "get-" nstr "-"))
	 (spre (J0 "set-" nstr "-"))
	 (msym (gensym))
	 (istrs (map (lambda (f i l)
		       (if (= l 1)
			   `(vector-set! ,msym ,i ,f)
			   (let* ((ixs (fromto 0 (- l 1)))
				  (jxs (fromto i (+ l i -1)))
				  (vss (map (lambda (jx ix)
					      `(vector-set!
						,msym ,jx (list-ref ,f ,ix)))
					    jxs ixs)))
			     `(begin ,@ vss))))
		     fs ss ls))
	 (gstrs (map (lambda (f i l)
		       (let* ((gfn (J1 gpre (symbol->string f)))
			      (err (string-append (symbol->string gfn)
						  " index out of bounds")))
			 (if (= l 1)
			     `(define (,gfn it)
				(vector-ref it ,i))
			     `(define (,gfn it ix)
				(if (and (>= ix 0) (< ix ,l))
				    (vector-ref it (+ ix ,i))
				    (raise ,err))))))
		     fs ss ls))
	 (sstrs (map (lambda (f i l)
		       (let* ((sfn (J2 spre (symbol->string f) "!"))
			      (err (string-append (symbol->string sfn)
						  " index out of bounds")))
			 (if (= l 1)
			     `(define (,sfn it val)
				(vector-set! it ,i val))
			     `(define (,sfn it ix val)
				(if (and (>= ix 0) (< ix ,l))
				    (vector-set! it (+ ix ,i) val)
				    (raise ,err))))))
		     fs ss ls))
	 (defs `(begin
		  (define (,mstr ,@fs)
		    (let ((,msym (vector-create ,len)))
		      (vector-set! ,msym 0 ',name)
		      ,@istrs
		      ,msym))
		  (define (,istr it)
		    (and (vector? it) (eqv? (vector-ref it 0) ',name)))
		  ,@gstrs ,@sstrs)))
    defs))
