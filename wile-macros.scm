;;; macros that got transmogrified into wile primitives; for test-wile.scm,
;;; they are inaccessible there, so bring them back here and include this
;;; file into test-wile.scm

(set-environment-variable "SKEEM_LIBRARY_PATH"
			  (get-environment-variable "WILE_LIBRARY_PATH"))

;;; stuff that skeem doesn't know about

(define list-append append)

(define list-reverse reverse)

(define(list-length=? n lst)
  (cond ((and (zero? n) (null? lst)) #t)
	((or (zero? n) (null? lst)) #f)
	(else (list-length=? (- n 1) (cdr lst)))))

(define (list-length>=? n lst)
  (cond ((zero? n) #t)
	((null? lst) #f)
	(else (list-length>=? (- n 1) (cdr lst)))))

(define (list-length>? n lst)
  (cond ((zero? n) (not (null? lst)))
	((null? lst) #f)
	(else (list-length>? (- n 1) (cdr lst)))))

(define (list-length<? n lst)
  (not (list-length>=? n lst)))

(define (list-length<=? n lst)
  (not (list-length>? n lst)))

(define (make-iproc args ig-arity body ig-env ig-mac)
  (eval (cons 'lambda (cons args body))))

;;; the first group of macros are somewhat generic

(defmacro (load-library fname)
  (letrec* ((paths (string-split-by
		    (lambda (c) (eqv? c #\:))
		    (get-environment-variable "WILE_LIBRARY_PATH")))
	    (find (lambda (ps)
		    (if (null? ps)
			#f
			(let ((fp (string-join-by "/" (car ps) fname)))
			  (if (file-exists? fp)
			      fp
			      (find (cdr ps)))))))
	    (filepath (find paths)))
	   (if filepath
	       `(load ,filepath)
	       `(write-string "unable to find file '" ,fname "'\n"))))

(defmacro (case-lambic n lam . fns)
  (let* ((args (gensym))
	 (group
	  (let loop ((cs (list (list n lam)))
		     (fs fns))
	    (cond ((null? fs)
		   (list-reverse (cons (list #f ()) cs)))
		  ((null? (cdr fs))
		   (list-reverse (cons (list #t (car fs)) cs)))
		  (else
		   (loop (cons (list (car fs) (cadr fs)) cs) (cddr fs))))))
	 (cases (map (lambda (nl)
		       (let ((n (car nl))
			     (l (cadr nl)))
			 (cond ((integer? n)
				`((,n) (apply ,l ,args)))
			       ((boolean? n)
				(if n
				    `(else (apply ,l ,args))
				    '(else (raise "case-lambic exhausted all cases, no match found!"))))
			       ((all-true? (map integer? n))
				`(,n (apply ,l ,args)))
			       (else (ERR "bad n-args spec %v" n)))))
		     group)))
    `(lambda ,args
       (case (list-length ,args)
	 ,@cases))))

(load-library "struct.scm")

;;; the macros below are very specific to wile

(defmacro (compile-with-output dest . body)
  (let ((tport (gensym)))
    `(let ((,tport (make-string-bag ())))
       (fluid-let ((global-out ,tport))
	 ,@body
	 (when ,dest
	   (transfer-all-lines ,tport ,dest))))))

(defmacro (emit-code . strs)
  (let ((xform
	 (let loop ((cs (string->list (apply string-join-by "\n" strs)))
		    (accs ())
		    (acca ()))
	   (cond ((null? cs)
		  (cons (list->string (list-reverse accs))
			(map (lambda (c)
			       (if (char=? c #\@)
				   'r
				   (string->symbol (char->string #\a c))))
			     (list-reverse acca))))
		 ((char=? (car cs) #\@)
		  (loop (cddr cs)
			(cons #\s (cons #\% accs))
			(cons (cadr cs) acca)))
		 (else
		  (loop (cdr cs) (cons (car cs) accs) acca))))))
    `(begin (when r (emit-decl r))
	    (emit-str (apply sprintf ,@xform ()))
	    (emit-str #\newline)
	    r)))

(defmacro (add-output val) `(set! output (cons ,val output)))
