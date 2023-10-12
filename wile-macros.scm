;;; macros that got transmogrified into wile primitives; for test-wile.scm,
;;; they are inaccessible there, so bring them back here and include this
;;; file into test-wile.scm

(set-environment-variable "SKEEM_LIBRARY_PATH"
			  (get-environment-variable "WILE_LIBRARY_PATH"))

(define list-append append)

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

(load-library "struct.scm")
