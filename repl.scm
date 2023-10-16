;;; -*- mode: scheme; -*-

;;; Wile -- the extremely stable scheming genius compiler *and* interpreter
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: GPLv3 or later, see file 'LICENSE' for details

;;; TODO: validate syntax better, in lots of places

;;; TODO: it's somehow somewhere not properly tail-recursive

;;; TODO: In the absence of working set-c[ad]r!, we need to be a tiny
;;; bit fancy with environments, so that we can do set!: thus define a
;;; bbox structure, for "binding-box": that has a durable pointer to
;;; a location in heap.

(def-struct bbox name value)

(define else-def (make-bbox 'else #t))

(define (ERR fmt . args)
  (flush-port stderr)
  (raise (apply sprintf
		(string-append fmt " at " (token-source-line args))
		args)))

(define (atom? val)
  (or (null? val)
      (boolean? val)
      (char? val)
      (string? val)
      (integer? val)
      (rational? val)
      (real? val)
      (complex? val)
      (file-port? val)
      (pipe-port? val)
      (socket-port? val)
      (string-port? val)
      (sqlite-port? val)
      (sqlite-statement? val)))

(define (special-form? val)
  (and (symbol? val)
       (or (symbol=? val 'quote)
	   (symbol=? val 'and)
	   (symbol=? val 'begin)
	   (symbol=? val 'case)
	   (symbol=? val 'cond)
;;;	   (symbol=? val 'define)	;;; these are special-special forms
;;;	   (symbol=? val 'defmacro)
	   (symbol=? val 'do)
	   (symbol=? val 'guard)
	   (symbol=? val 'if)
	   (symbol=? val 'lambda)
	   (symbol=? val 'let)
	   (symbol=? val 'let*)
	   (symbol=? val 'letrec)
	   (symbol=? val 'letrec*)
	   (symbol=? val 'or)
	   (symbol=? val 'set!)
	   (symbol=? val 'quasiquote)
	   (symbol=? val 'unquote)
	   (symbol=? val 'unquote-splicing)

	   (symbol=? val 'pragma)

	   ;;; these are really macros, rewrite them that way
	   ;;; once we can actually do macros
	   (symbol=? val 'when)
	   (symbol=? val 'unless)
	   (symbol=? val 'while)
	   (symbol=? val 'do-while)
	   (symbol=? val 'until)
	   (symbol=? val 'do-until)
	   )))

;;; TODO: add defmacro here at the appropriate time

(define (define-form? expr)
  (and (pair? expr)
       (symbol? (car expr))
       (or (symbol=? (car expr) 'define)
	   (symbol=? (car expr) 'defmacro))))

(define (unique-symbols? lst)
  (let ((syms? (let loop ((ls lst))	;;; check symbolness
		 (cond ((null? ls) #t)
		       ((symbol? (car ls)) (loop (cdr ls)))
		       (else #f)))))
    (if syms?				;;; if symbolness, check uniqueness
	(let loop ((ss (list-sort (lambda (a b) (string<? a b))
				   (map symbol->string lst))))
	  (cond ((null? ss) #t)
		((null? (cdr ss)) #t)
		((string=? (car ss) (cadr ss)) #f)
		(else (loop (cdr ss)))))
	#f)))

(define (symbol-lookup box? env sym)
  (cond ((null? env)
	 (ERR "symbol '%s' lookup failed!" sym))
	((symbol=? sym (get-bbox-name (car env)))
	 (if box? (car env) (get-bbox-value (car env))))
	(else (symbol-lookup box? (cdr env) sym))))

(define (eval-binding env expr)
  (if (and (pair? expr) (symbol? (car expr)))
      (make-bbox (car expr) (eval-expr env (cadr expr)))
      (ERR "malformed binding expression '%v'" expr)))

(define (eval-define macro env expr)
  (let* ((bbv (cond ((symbol? (car expr))
		     (eval-binding env expr))
		    ((and (pair? (car expr)) (symbol? (caar expr)))
		     (eval-binding env (list (caar expr)
					     `(lambda ,(cdar expr)
						,@(cdr expr)))))
		    (else
		     (ERR "malformed 'define' expression '%v'" expr))))
	 (bv (get-bbox-value bbv))
	 (new-env (cons bbv env)))
    (when (interpreted-procedure? bv)
      (set-iproc-macro! bv macro)
      (set-iproc-env! bv new-env))
    new-env))

(define (eval-begin env expr)
  (if (null? expr)
      ()		;;; TODO: this could alternately be an error
      (let loop ((es expr))
	(let ((ev (if (define-form? (car es))
		      (begin
			(set! env (eval-define (symbol=? (caar es) 'defmacro)
					       env (cdar es)))
			(get-bbox-value (car env)))
		      (eval-expr env (car es)))
		  ))
	  (if (null? (cdr es))
	      ev
	      (loop (cdr es)))))))

(define (eval-if env exprs)
  (if (list-length=? 3 exprs)
      (let ((t (eval-expr env (car exprs))))
	(eval-expr env ((if t cadr caddr) exprs)))
      (ERR "malformed 'if' expression '%v'" exprs)))

(define (eval-when env exprs)
  (if (null? exprs)
      (ERR "malformed 'when' expression")
      (when (eval-expr env (car exprs)) (eval-begin env (cdr exprs)))))

(define (eval-unless env exprs)
  (if (null? exprs)
      (ERR "malformed 'unless' expression")
      (unless (eval-expr env (car exprs)) (eval-begin env (cdr exprs)))))

(define (eval-while env exprs)
  (if (null? exprs)
      (ERR "malformed 'while' expression")
      (while (eval-expr env (car exprs)) (eval-begin env (cdr exprs)))))

(define (eval-do-while env exprs)
  (if (null? exprs)
      (ERR "malformed 'do-while' expression")
      (do-while (eval-expr env (car exprs)) (eval-begin env (cdr exprs)))))

(define (eval-until env exprs)
  (if (null? exprs)
      (ERR "malformed 'until' expression")
      (until (eval-expr env (car exprs)) (eval-begin env (cdr exprs)))))

(define (eval-do-until env exprs)
  (if (null? exprs)
      (ERR "malformed 'do-until' expression")
      (do-until (eval-expr env (car exprs)) (eval-begin env (cdr exprs)))))

(define (eval-and init env exprs)
  (cond ((not init) init)
	((null? exprs) init)
	(else
	 (eval-and (eval-expr env (car exprs)) env (cdr exprs)))))

(define (eval-or init env exprs)
  (cond (init init)
	((null? exprs) init)
	(else
	 (eval-or (eval-expr env (car exprs)) env (cdr exprs)))))

(define (check-let-bindings type llist)
  (if (and (list? llist) (list-length>=? 2 llist))
      (let ((blist (car llist)))
	(unless (list? blist)
	  (ERR "malformed '%s' bindings '%v' are not a list" type blist))
	(for-each (lambda (bv)
		    (unless (list? bv)
		      (ERR "malformed '%s' binding '%v' is not a list"
			   type bv)))
		  blist)
	(let ((vs (map car blist)))
	  (unless (unique-symbols? vs)
	    (ERR "malformed '%s' bindings list '%v'" type vs))))
      (ERR "malformed '%s' expression '%v'" type llist)))

(define (eval-let star? env exprs)
  (cond ((null? exprs)
	 (ERR "malformed 'let' expression '%v'" exprs))
	((and (list? exprs) (symbol? (car exprs)))
	 (check-let-bindings "named-let" (cdr exprs))
	 (let* ((args (map car (cadr exprs)))
		(vals (map (lambda (v) (eval-expr env (cadr v))) (cadr exprs)))
		(fn (make-iproc args (list-length args) (cddr exprs) () #f))
		(new-env (cons (make-bbox (car exprs) fn) env)))
	   (set-iproc-env! fn new-env)
	   (apply-lambda fn vals)))
	(else
	 (check-let-bindings (if star? "let*" "let") exprs)
	 (let* ((new-env env)
		(lvs (map (lambda (iex)
			    (let ((lv (eval-binding new-env iex)))
			      (when star? (set! new-env (cons lv new-env)))
			      lv))
			  (car exprs))))
	   (unless star?
	     (set! new-env (list-append (list-reverse lvs) env)))
	   (eval-begin new-env (cdr exprs))))))

(define (eval-letrec env exprs)
  (if (null? exprs)
      (ERR "malformed 'letrec' expression '%v'" exprs)
      (begin
	(check-let-bindings "letrec" exprs)
	(let* ((ts (map (lambda (iex) (cons (car iex) #f)) (car exprs)))
	       (tmp-env (list-append (list-reverse ts) env))
	       (lvs (map (lambda (iex)
			   (eval-binding tmp-env iex))
			 (car exprs)))
	       (new-env (list-append (list-reverse lvs) env)))
	  (for-each (lambda (box)
		      (let ((bval (get-bbox-value box)))
			(when (interpreted-procedure? bval)
			  (set-iproc-env! bval new-env))))
		    lvs)
	  (eval-begin new-env (cdr exprs))))))

(define (eval-qq-olist level env expr)
  (let* ((lh (eval-qq level env (car expr)))
	 (lt (eval-qq level env (cdr expr)))
	 (splice? (and (pair? (car expr))
		       (symbol? (caar expr))
		       (symbol=? (caar expr) 'unquote-splicing))))
    (if splice? (list-append lh lt) (cons lh lt))))

(define (eval-qq level env expr)
  (cond ((atom? expr) expr)
	((symbol? expr)
	 (if (positive? level) expr (symbol-lookup #f env expr)))
	((pair? expr)
	 (if (symbol? (car expr))
	     (cond ((or (symbol=? (car expr) 'unquote)
			(symbol=? (car expr) 'unquote-splicing))
		    (if (= level 1)
			(eval-expr env (cadr expr))
			(eval-qq-olist (- level 1) env expr)))
		   ((symbol=? (car expr) 'quasiquote)
		    (eval-qq-olist (+ level 1) env expr))
		   (else
		    (eval-qq-olist level env expr)))
	     (eval-qq-olist level env expr)))
	((vector? expr)
	 (list->vector (eval-qq-olist level env (vector->list expr))))

;;; bytevectors are a little problematic, because they assume the contents
;;; are always bytes, and thus only have space for bytes. so storing some
;;; arbitrary expression in there is not possible, even if it evaluates to
;;; a byte at the end. probably not going to support this for a while if
;;; ever. instead, use a vector, generate only bytes, and then convert it
;;; to a bytevector

;;;	((bytevector? expr)
;;;	 (list->bytevector (eval-qq-olist
;;;			    level env (bytevector->list expr))))
	(else				;;; unimplemented or impossible cases
	 (ERR "malformed 'quasiquote' expression '%v'" expr))))

;;; TODO: this still has one tiny syntactic flaw: if 'else occurs in a
;;; clause before the last, it'll still be evaluated as true, and none
;;; of the others will matter

(define (eval-cond env expr)
  (cond ((null? expr) #f)
	((pair? (car expr))
	 (if (eval-expr env (caar expr))
	     (eval-begin env (cdar expr))
	     (eval-cond env (cdr expr))))
	(else (ERR "malformed 'cond' expression '%v'" expr))))

(define (eval-set! env expr)
  (if (and (pair? expr)
	   (symbol? (car expr))
	   (list-length=? 2 expr))
      (let ((box (symbol-lookup #t env (car expr))))
	(set-bbox-value! box (eval-expr env (cadr expr)))
	(get-bbox-value box))
      (ERR "malformed 'set!' expression '%v'" expr)))

(define (eval-guard env expr)
  (let* ((re-raise #f)
	 (re-err #f)
	 (guard-val
	  (guard (err (#t (let ((err-env
				 (cons (make-bbox (caar expr) err) env)))
			    (let loop ((cs (cdar expr)))
			      (cond ((null? cs)
				     (set! re-raise #t)
				     (set! re-err err))
				    ((eval-expr err-env (caar cs))
				     (eval-begin err-env (cdar cs)))
				    (else (loop (cdr cs))))))))
		 (eval-begin env (cdr expr)))))
    (if re-raise
	(raise re-err)
	guard-val)))

;;; TODO: implement => syntax

(define (eval-case env expr)
  (if (and (list? expr) (list-length>=? 2 expr))
      (let ((val (eval-expr env (car expr))))
	(let loop ((cs (cdr expr)))
	  (if (null? cs)
	      #f
	      (if (and (list? (car cs))
		       (or (and (list? (caar cs))
				(not (null? (caar cs))))
			   (eqv? (caar cs) 'else))
		       (not (null? (cdar cs))))
		  (if (or (eqv? (caar cs) 'else)
			  (memv val (caar cs)))
		      (eval-begin env (cdar cs))
		      (loop (cdr cs)))
		  (ERR "malformed 'case' clause '%v'" (car cs))))))
      (ERR "malformed 'case' expression '%v'" expr)))

;;; Convert a possibly dotted-list into a proper list, counting the
;;; number of entries along the way; return the length as in arity
;;; above plus the proper list: proper lists report positive length,
;;; dotted lists report negative length:
;;;
;;;     (args-list ())		=> (0 ())
;;;     (args-list '(1 2 3))	=> (3 (1 2 3))
;;;     (args-list '(1 2 . 3))	=> (-3 (1 2 3))
;;;     (args-list 'a)		=> (-1 (a))

(define (args-list dotted-list)
  (let loop ((as dotted-list)
	     (pl ())
	     (na 0))
    (cond ((null? as) (list na (list-reverse pl)))
	  ((pair? as) (loop (cdr as) (cons (car as) pl) (+ na 1)))
	  (else (list (- (- na) 1) (list-reverse (cons as pl)))))))

(define (eval-lambda env expr)
  (let* ((args (car expr))
	 (argies (args-list args))
	 (arity (car argies))
	 (formals (cadr argies)))
    (unless (unique-symbols? formals)
      (ERR "malformed 'lambda' args list '%v'" args))
    (make-iproc formals arity (cdr expr) env #f)))

;;; (define-syntax do
;;;   (syntax-rules ()
;;;     ((do ((var init step ...) ...)
;;; 	 (test expr ...)
;;;        command ...)
;;;      (letrec
;;; 	 ((loop
;;; 	   (lambda (var ...)
;;; 	     (if test
;;; 		 (begin
;;; 		   (if #f #f)
;;; 		   expr ...)
;;; 		 (begin
;;; 		   command
;;; 		   ...
;;; 		   (loop (do "step" var step ...)
;;; 			 ...))))))
;;;        (loop init ...)))
;;;     ((do "step" x)
;;;      x)
;;;     ((do "step" x y)
;;;      y)))

(define (check-syntax-do expr)
  (and (list? expr)
       (list-length>=? 2 expr)
       (list? (car expr))
       (list? (cadr expr))
       (list-length>? 0 (cadr expr))))

(define (eval-do env expr)
  (if (check-syntax-do expr)
      (let ((var-info (car expr))
	    (test (cadr expr))
	    (body (cddr expr)))
	(for-each (lambda (vdef)
		    (unless (and (list? vdef)
				 (list-length>=? 2 vdef)
				 (symbol? (car vdef)))
		      (ERR "malformed 'do' variable-expr '%v'" vdef)))
		  var-info)
	(write-string "so far so good...\n")
	)
      (ERR "malformed 'do' expression '%v'" expr)))

;;; (do ((i 2 (+ i 1)))
;;;     ((> i 25) #t)
;;;   (set! default-int-base i)
;;;   (display i)
;;;   (write-string #\space)
;;;   (display 17)
;;;   (newline))

(define (apply-lambda fn args)
  (let* ((formals (get-iproc-args fn))
	 (arity (get-iproc-arity fn))
	 (a-len (list-length args)))
    (if (negative? arity)
	(let ((nreq (- (- arity) 1)))
	  (unless (>= a-len nreq)
	    (ERR "malformed 'lambda' evaluation: %d required vs %d given args"
		 nreq a-len))
	  (set! args (list-append (list-head args nreq)
				  (list (list-tail args nreq)))))
	(unless (= a-len arity)
	  (ERR "malformed 'lambda' evaluation: %d formal vs %d actual args"
	       arity a-len)))
    (eval-begin
     (let loop ((fs formals)
		(as args)
		(env (get-iproc-env fn)))
       (if (null? fs)
	   env
	   (loop (cdr fs) (cdr as)
		 (cons (make-bbox (car fs) (car as)) env))))
     (get-iproc-body fn))))

(define (apply-interp fn . args)
  (let* ((a1 (reverse args))
	 (a2 (let loop ((acc (car a1))
			(lst (cdr a1)))
	       (if (null? lst)
		   acc
		   (loop (cons (car lst) acc) (cdr lst))))))
    (if (interpreted-procedure? fn)
	(apply-lambda fn a2)
	(apply fn a2))))

(define (eval-expr env expr)
  (cond ((or (atom? expr) (vector? expr) (bytevector? expr)) expr)
	((symbol? expr)
	 (symbol-lookup #f env expr))
	((pair? expr)
	 (cond ((special-form? (car expr))
		(case (car expr)
		  ((quote) (cadr expr))
		  ((begin) (eval-begin env (cdr expr)))
		  ((if) (eval-if env (cdr expr)))
		  ((and) (eval-and #t env (cdr expr)))
		  ((or) (eval-or #f env (cdr expr)))
		  ((let) (eval-let #f env (cdr expr)))
		  ((let*) (eval-let #t env (cdr expr)))
		  ((letrec letrec*) (eval-letrec env (cdr expr)))
		  ((cond) (eval-cond (cons else-def env) (cdr expr)))
		  ((set!) (eval-set! env (cdr expr)))
		  ((guard) (eval-guard env (cdr expr)))
		  ((case) (eval-case env (cdr expr)))
		  ((lambda) (eval-lambda env (cdr expr)))
		  ((do) (eval-do env (cdr expr)))

		  ((pragma) (fprintf stderr ";;; ignoring %v\n" expr))

		  ((quasiquote)
		   (if (list-length=? 2 expr)
		       (eval-qq 1 env (cadr expr))
		       (ERR "malformed 'quasiquote' expression '%v'"
			    (cdr expr))))
		  ((unquote)
		   (ERR "naked 'unquote' expression '%v'" expr))
		  ((unquote-splicing)
		   (ERR "naked 'unquote-splicing' expression '%v'" expr))
		  ((when) (eval-when env (cdr expr)))
		  ((unless) (eval-unless env (cdr expr)))
		  ((while) (eval-while env (cdr expr)))
		  ((do-while) (eval-do-while env (cdr expr)))
		  ((until) (eval-until env (cdr expr)))
		  ((do-until) (eval-do-until env (cdr expr)))
		  (else 'unimplemented-special-form)))
	       ((define-form? expr)
		(ERR "misplaced 'define' expression '%v'" expr))
	       (else
		(let ((ator (eval-expr env (car expr))))
		  (if (interpreted-procedure? ator)
		      (if (get-iproc-macro ator)
			  (eval-expr env (apply-lambda ator (cdr expr)))
			  (apply-lambda ator (map (lambda (e)
						    (eval-expr env e))
						  (cdr expr))))
		      (apply ator (map (lambda (e) (eval-expr env e))
				       (cdr expr))))))))))

;;;	((promise? expr) 'promise)
;;;	((procedure? expr) 'procedure)
;;;	((continuation? expr) 'continuation)

(define (finish chirp)
  (when chirp
    (write-string chirp))
  (exit 0))

(define (run-repl env)
  (guard (err (#t (fprintf stderr "caught exception\n    %v\n" err)
		  (run-repl env)))
	 (let loop ()
	   (write-string "wile> ")
	   (flush-port stdout)
	   (let ((line (read-line stdin)))
	     (if line
		 (let ((line (string-trim char-whitespace? line)))
		   (cond ((string=? line "bye")
			  (finish ";;; gis baldau!\n"))
			 ((string=? line "")
			  (loop))
			 (else
			  (let ((expr (car (parse-string line))))
			    (display (if (define-form? expr)
					 (begin
					   (set! env
						 (eval-define
						  (symbol=? (car expr)
							    'defmacro)
						  env (cdr expr)))
					   (get-bbox-value (car env)))
					 (eval-expr env expr)))
			    (newline)
			    (loop)))))
		 (finish "\n"))))))

(define (run-batch env file)
  (guard
   (err (#t (fprintf stderr "caught exception\n    %v\n" err) (exit 1)))
   (let loop ((es (read-all file)))
     (cond ((null? es) (finish #f))
	   ((define-form? (car es))
	    (set! env (eval-define
		       (symbol=? (caar es) 'defmacro) env (cdar es)))
	    (get-bbox-value (car env)))
	   (else (eval-expr env (car es))))
     (loop (cdr es)))))

(let ((top-env
       (list-append (list
		     (make-bbox 'command-name command-name)
		     (make-bbox 'command-line-arguments command-line-arguments)
		     (make-bbox 'apply apply-interp))
		    (wile-standard-environment))))
  (if (null? command-line-arguments)
      (begin
	(write-string
	 ";;; Welcome to wile          https://github.com/uhollerbach/wile\n"
	 ";;; Copyright 2023 Uwe Hollerbach         available under GPLv3+\n")
	(for-each (lambda (v) (printf ";;;   %v\n" v)) (wile-build-info))
	(run-repl top-env))
      (run-batch top-env (car command-line-arguments))))
