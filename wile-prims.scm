;;; -*- mode: scheme; -*-

;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: GPLv3 or later, see file 'LICENSE' for details

;;; table of primitive functions

;;; TODO: deal with errno? also make sure as many numeric types
;;; as are relevant are supported everywhere

;;; TODO: add type-checking somehow

;;; Variadic functions, unless they are regular and can be expanded
;;; inside wile as roughly the following, must have a codelet that
;;; includes the build-basic-list bit: with this template, the
;;; build-variadic-bypass hack below can be uniformly used to signal
;;; whether it's a direct call to the v-fn, in which case the basic
;;; list has to get built, or whether it's a wrapper call for use by
;;; apply, in which case apply has already done the work. The
;;; build-variadic-bypass is a horrible hack, but it seems better than
;;; all the other horrible hacks.
;;;
;;;	 (lambda (r . as)
;;;	   (apply build-basic-list r as)
;;;	   (emit-fstr "%s = wile_string_append(&(%s));\n" r r)
;;;	   r))

(define build-variadic-bypass #f)

(define (build-basic-list r . as)
  (if (null? as)
      (emit-code "@@ = LVI_NIL();")
      (let ((ac (list-length as)))
	(emit-decl r)
	(if build-variadic-bypass
	    (emit-fstr "%s = %s;\n" r build-variadic-bypass)
	    (begin
	      (emit-fstr "{\nlval vs[%d];\n" ac)
	      (for-each (lambda (i v)
			  (emit-fstr "vs[%d] = %s;\n" (- i 1) v))
			(fromto 1 ac) as)
	      (emit-fstr "%s = gen_list(%d, vs, NULL);\n}\n" r ac)))
	r)))

;;; used here for apply and in wile-comp.scm

(define (compile-runtime-apply r . as)
  (apply build-basic-list r as)
  (emit-fstr "%s = wile_apply_function(&(%s), __FILE__, __LINE__);\n" r r)
  r)

;;; Promote a number to be at least real type; checking for complex
;;; or non-number is the caller's responsibility

(define (promote/real r a1)
  (emit-code
   "if (@1.vt == LV_INT) {"
   "@@ = LVI_REAL((lisp_real_t) @1.v.iv);"
   "} else if (@1.vt == LV_RAT) {"
   "@@ = LVI_REAL(LV_RAT2REAL(@1));"
   "} else {"
   "@@ = @1;"
   "}"))

;;; Promote a number to real type, failing if the input is not real-valued

;;; In several of these functions, we could also rename the arguments:
;;; in promote/real+check, change s-name directly to a9, etc.

(define (promote/real+check s-name r a1)
  (let ((a9 s-name))
    (emit-code
     "if (@1.vt == LV_INT) {"
     "@@ = LVI_REAL((lisp_real_t) @1.v.iv);"
     "} else if (@1.vt == LV_RAT) {"
     "@@ = LVI_REAL(LV_RAT2REAL(@1));"
     "} else if (@1.vt == LV_REAL) {"
     "@@ = @1;"
     "} else {"
     "WILE_EX(\"@9\", \"expects a real-valued input\");"
     "}")))

(define (build-special-math-real s-name c-name r a1)
  (let ((a9 c-name)
	(a8 s-name))
    (emit-code
     "{"
     "lisp_real_t r;"
     "switch (@1.vt) {"
     "case LV_INT:"
     "r = @1.v.iv;"
     "break;"
     "case LV_RAT:"
     "r = LV_RAT2REAL(@1);"
     "break;"
     "case LV_REAL:"
     "r = @1.v.rv;"
     "break;"
     "case LV_CMPLX:"
     "WILE_EX(\"@8\", \"is not implemented for complex values\");"
     "default:"
     "WILE_EX(\"@8\", \"got a non-numeric argument\");"
     "}"
     "@@ = LVI_REAL(@9(r));"
     "}")))

(define (build-special-math-cmplx s-name c-name r a1)
  (let ((a9 c-name)
	(a8 s-name))
    (emit-code
     "{"
     "lisp_cmplx_t z;"
     "switch (@1.vt) {"
     "case LV_INT:"
     "z = @1.v.iv;"
     "break;"
     "case LV_RAT:"
     "z = LV_RAT2REAL(@1);"
     "break;"
     "case LV_REAL:"
     "z = @1.v.rv;"
     "break;"
     "case LV_CMPLX:"
     "z = @1.v.cv;"
     "break;"
     "default:"
     "WILE_EX(\"@8\", \"got a non-numeric argument\");"
     "}"
     "z = @9(z);"
     "if (CIMAG(z) == 0.0) {"
     "@@ = LVI_REAL(CREAL(z));"
     "} else {"
     "@@ = LVI_CMPLX1(z);"
     "}"
     "}")))

(define (build-special-math-rc s-name cr-name cc-name r a1)
  (let ((a9 cr-name)
	(a8 cc-name)
	(a7 s-name))
    (emit-code
     "{"
     "lisp_real_t r;"
     "lisp_cmplx_t z;"
     "bool isr;"
     "switch (@1.vt) {"
     "case LV_INT:"
     "r = @1.v.iv;"
     "isr = true;"
     "break;"
     "case LV_RAT:"
     "r = LV_RAT2REAL(@1);"
     "isr = true;"
     "break;"
     "case LV_REAL:"
     "r = @1.v.rv;"
     "isr = true;"
     "break;"
     "case LV_CMPLX:"
     "z = @1.v.cv;"
     "isr = false;"
     "break;"
     "default:"
     "WILE_EX(\"@7\", \"got a non-numeric argument\");"
     "}"
     "if (isr) {"
     "@@ = LVI_REAL(@9(r));"
     "} else {"
     "z = @8(z);"
     "if (CIMAG(z) == 0.0) {"
     "@@ = LVI_REAL(CREAL(z));"
     "} else {"
     "@@ = LVI_CMPLX1(z);"
     "}"
     "}"
     "}")))

(define (build-real-cmp a9 r a1 a2)
  (emit-code
   "switch (TYPE_COMBO(@1.vt,@2.vt)) {"
   "case TYPE_COMBO(LV_INT,LV_INT):"
   "@@ = LVI_BOOL(@1.v.iv @9 @2.v.iv);"
   "break;"
   "case TYPE_COMBO(LV_INT,LV_RAT):"
   "@@ = LVI_BOOL(@1.v.iv * @2.v.irv.den @9 @2.v.irv.num);"
   "break;"
   "case TYPE_COMBO(LV_INT,LV_REAL):"
   "@@ = LVI_BOOL(@1.v.iv @9 @2.v.rv);"
   "break;"
   "case TYPE_COMBO(LV_RAT,LV_INT):"
   "@@ = LVI_BOOL(@1.v.irv.num @9 @2.v.iv * @1.v.irv.den);"
   "break;"
   "case TYPE_COMBO(LV_RAT,LV_RAT):"
   "@@ = LVI_BOOL(@1.v.irv.num * @2.v.irv.den @9 @2.v.irv.num * @1.v.irv.den);"
   "break;"
   "case TYPE_COMBO(LV_RAT,LV_REAL):"
   "@@ = LVI_BOOL(@1.v.irv.num @9 @2.v.rv * @1.v.irv.den);"
   "break;"
   "case TYPE_COMBO(LV_REAL,LV_INT):"
   "@@ = LVI_BOOL(@1.v.rv @9 @2.v.iv);"
   "break;"
   "case TYPE_COMBO(LV_REAL,LV_RAT):"
   "@@ = LVI_BOOL(@1.v.rv * @2.v.irv.den @9 @2.v.irv.num);"
   "break;"
   "case TYPE_COMBO(LV_REAL,LV_REAL):"
   "@@ = LVI_BOOL(@1.v.rv @9 @2.v.rv);"
   "break;"
   "default:"
   "WILE_EX(\"@9\", \"inputs are not real-valued numbers\");"
   "break;"
   "}"))


;;; arity meaning:
;;; 0 or more	-> a function which accepts exactly that many arguments
;;; negative	-> a function which accepts (- (- arity) 1) or more arguments
;;;		   -1 -> 0 or more, -2 -> 1 or more, -3 -> 2 or more, etc

(define prim-table-internal
  (list

   (list 'creal 'alias 'real-part)
   (list 'cimag 'alias 'imag-part)
   (list 'last 'alias 'list-last)
   (list 'make-rectangular 'alias 'cmplx)
   (list 'phase 'alias 'angle)
   (list 'complex-conjugate 'alias 'cconj)
   (list 'conj 'alias 'cconj)
   (list 'number? 'alias 'complex?)
   (list 'char-lower-case? 'alias 'char-lowercase?)
   (list 'char-upper-case? 'alias 'char-uppercase?)
   (list 'list->string 'alias 'char->string)
   (list 'magnitude 'alias 'abs)
   (list 'modulo 'alias 'floor-remainder)
   (list 'quotient 'alias 'truncate-quotient)
   (list 'remainder 'alias 'truncate-remainder)
   (list 'quot-rem 'alias 'truncate/)
   (list 'directory-exists? 'alias 'file-exists?)
   (list 'rename-directory 'alias 'rename-file)
   (list 'make-vector 'alias 'vector-create)
   (list 'make-string 'alias 'string-create)
   (list 'vector-capacity 'alias 'vector-length)
   (list 'make-bytevector 'alias 'bytevector-create)
   (list 'substring 'alias 'string-copy)
   (list 'read-all 'alias 'parse-file)
   (list 'sqlite-close 'alias 'close-port)
   (list 'agm 'alias 'arithmetic-geometric-mean)

   (list 'call-with-current-continuation 'alias 'call/cc)

   (list 'when "expects a predicate and any number of actions; if the predicate is true, the actions are evaluated"
	 'macro -2
	 (lambda (pred . actions) `(if ,pred (begin ,@actions) #f)))

   (list 'unless "expects a predicate and any number of actions; if the predicate is false, the actions are evaluated"
	 'macro -2
	 (lambda (pred . actions) `(if ,pred #f (begin ,@actions))))

   (list 'while "expects a predicate and any number of actions; the actions are evaluated repeatedly as long as the predicate is true, zero or more times"
	 'macro -2
	 (lambda (some-cond . some-actions)
	   (let ((mc (gensym)))
	     `(do ((,mc 0 (i+ ,mc 1)))
		  ((not ,some-cond) ,mc)
		,@some-actions))))

   (list 'until "expects a predicate and any number of actions; the actions are evaluated repeatedly as long as the predicate is false, zero or more times"
	 'macro -2
	 (lambda (some-cond . some-actions)
	   (let ((mc (gensym)))
	     `(do ((,mc 0 (i+ ,mc 1)))
		  (,some-cond ,mc)
		,@some-actions))))

   (list 'do-while "expects a predicate and any number of actions; the actions are evaluated repeatedly as long as the predicate is true, one or more times"
	 'macro -2
	 (lambda (some-cond . some-actions)
	   (let ((mc (gensym)))
	     `(do ((,mc 0 (i+ ,mc 1)))
		  ((and (positive? ,mc) (not ,some-cond)) ,mc)
		,@some-actions))))

   (list 'do-until "expects a predicate and any number of actions; the actions are evaluated repeatedly as long as the predicate is true, one or more times"
	 'macro -2
	 (lambda (some-cond . some-actions)
	   (let ((mc (gensym)))
	     `(do ((,mc 0 (i+ ,mc 1)))
		  ((and (positive? ,mc) ,some-cond) ,mc)
		,@some-actions))))

   (list 'fluid-let 'macro -2
	 (lambda (vals . body)
	   (let ((svals (map (lambda (ig) (gensym)) vals))
		 (result (gensym))
		 (vars (map car vals))
		 (tvals (map cadr vals)))
	     `(let ,(map (lambda (s v) `(,s ,v)) svals vars)
		,@(map (lambda (v t) `(set! ,v ,t)) vars tvals)
		(let ((,result (begin ,@body)))
		  ,@(map (lambda (v s) `(set! ,v ,s)) vars svals)
		  ,result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; these macros are part of closing the loop for wile to compile itself

   (list 'load-library 'macro 1
	 (lambda (fname)
	   (letrec* ((paths (string-split-by
			     (lambda (c) (eqv? c #\:))
			     (get-environment-variable "WILE_LIBRARY_PATH")))
		     (find (lambda (ps)
			     (if (null? ps)
				 #f
				 (let ((fp (string-join-by
					    "/" (car ps) fname)))
				   (if (file-exists? fp)
				       fp
				       (find (cdr ps)))))))
		     (filepath (find paths)))
	     (if filepath
		 `(load ,filepath)
		 `(write-string "unable to find file '" ,fname "'\n")))))

   (list 'add-output 'macro 1
	 (lambda (val) `(set! output (cons ,val output))))

   (list 'compile-with-output 'macro -2
	 (lambda (dest . body)
	   (let ((tport (gensym)))
	     `(let ((,tport (make-string-bag ())))
		(fluid-let ((global-out ,tport))
		  ,@body
		  (when ,dest
		    (transfer-all-lines ,tport ,dest)))))))

   (list 'emit-code 'macro -1
	 (lambda strs
	   (let ((xform
		  (let loop ((cs (string->list
				  (apply string-join-by "\n" strs)))
			     (accs ())
			     (acca ()))
		    (cond ((null? cs)
			   (cons (list->string (list-reverse accs))
				 (map (lambda (c)
					(if (char=? c #\@)
					    'r
					    (string->symbol
					     (char->string #\a c))))
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
		     r))))

   (list 'def-struct 'macro -3
	 (lambda (name field . fields)
	   (let* ((fs (cons field fields))
		  (lfs (list-length fs))
		  (nfs (+ 1 lfs))
		  (J0 (lambda strs (apply string-append strs)))
		  (J1 (lambda (pre main) (string->symbol (J0 pre main))))
		  (J2 (lambda (pre main post) (string->symbol (J0 pre main post))))
		  (nstr (symbol->string name))
		  (mstr (J1 "make-" nstr))
		  (msym (gensym))
		  (istr (J2 "isa-" nstr "?"))
		  (gpre (J0 "get-" nstr "-"))
		  (spre (J0 "set-" nstr "-"))
		  (istrs (map (lambda (f i)
				`(vector-set! ,msym ,i ,f)) fs (fromto 1 lfs)))
		  (gstrs (map (lambda (f i)
				(let ((gfn (J1 gpre (symbol->string f))))
				  `(define (,gfn it)
				     (vector-ref it ,i)))) fs (fromto 1 lfs)))
		  (sstrs (map (lambda (f i)
				(let ((sfn (J2 spre (symbol->string f) "!")))
				  `(define (,sfn it val)
				     (vector-set! it ,i val)))) fs (fromto 1 lfs)))
		  (defs `(begin
			   (define (,mstr ,@fs)
			     (let ((,msym (vector-create ,nfs)))
			       (vector-set! ,msym 0 ',name)
			       ,@istrs
			       ,msym))
			   (define (,istr it)
			     (and (vector? it) (eqv? (vector-ref it 0) ',name)))
			   ,@gstrs ,@sstrs)))
	     defs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (list 'begin-breakable 'macro -2
	 (lambda (tag . actions)
	   (let ((cname (gensym)))
	     `(guard (,cname ((symbol=? ,cname ,tag))) ,@actions))))

   ;;; Namespace: a private area from which only selected symbols get
   ;;; exported.  Symbols can get exported as they are named in the
   ;;; private area, or they can get renamed: just plain sym or
   ;;; (private-sym public-sym), respectively, in the syms list:
   ;;; (namespace (sym1 sym2 sym3 ...)
   ;;;
   ;;; (namespace (bar1 bar2 bar3) ...)
   ;;; -> export internal symbols bar1 bar2 bar3
   ;;;    as the same public names bar1 bar2 bar3
   ;;;
   ;;; (namespace ((foo1 public-foo1) (foo2 public-foo2) foo3) ...)
   ;;; -> export internal symbols foo1 foo2 foo3 as public names
   ;;;    public-foo1 public-foo2 foo3 respectively

;;; TODO: this version, the correct one that I want, is interacting
;;; poorly with closures; something somewhere is referring to the
;;; wrong version of something else. Until I figure that out, use the
;;; stupid do-nothing version below.

;;;    (list 'namespace 'macro -2
;;; 	 (lambda (syms . defs)
;;; 	   (let* ((nm (gensym))
;;; 		  (sa (gensym))
;;; 		  (s0 (map (lambda (s) (if (symbol? s) (list s s) s)) syms))
;;; 		  (s1 (map (lambda (s)
;;; 			     (let ((p (cadr s)))
;;; 			       `(define ,p #f))) s0))
;;; 		  (s2 (map (lambda (s)
;;; 			     (let ((p (car s)))
;;; 			       `((symbol=? ,sa ',p) ,p))) s0))
;;; 		  (s3 (map (lambda (s)
;;; 			     (let ((p1 (car s))
;;; 				   (p2 (cadr s)))
;;; 			       `(set! ,p2 (,nm ',p1)))) s0)))
;;; 	     `(begin ,@s1
;;; 		     (let ((,nm ((lambda ()
;;; 				   ,@defs
;;; 				   (lambda (,sa) (cond ,@s2))))))
;;; 		       ,@s3)))))

   (list 'namespace 'macro -2
	 (lambda (syms . defs)
	   `(begin ,@defs)))

   (list 'cons "expects two values A and B and returns the newly-allocated pair (A B)"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lptr p1 = NULL, p2 = NULL;"
	    "if (@1.vt != LV_NIL) {"
	    "p1 = new_lv(LV_NIL);"
	    "*p1 = @1;"
	    "}"
	    "if (@2.vt != LV_NIL) {"
	    "p2 = new_lv(LV_NIL);"
	    "*p2 = @2;"
	    "}"
	    "@@ = LVI_PAIR(p1, p2);"
	    "}")))

   (list 'car "expects a pair (A B) and returns its first element A"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "if (@1.vt != LV_PAIR) {"
	    "WILE_EX(\"car\", \"input is not a pair!\");"
	    "}"
	    "@@ = (@1.v.pair.car ? *(@1.v.pair.car) : LVI_NIL());")))

   (list 'cdr "expects a pair (A B) and returns its second element B"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "if (@1.vt != LV_PAIR) {"
	    "WILE_EX(\"cdr\", \"input is not a pair!\");"
	    "}"
	    "@@ = (@1.v.pair.cdr ? *(@1.v.pair.cdr) : LVI_NIL());")))

   (list 'set-car! 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_PAIR) {"
	    "WILE_EX(\"set-car!\", \"input is not a pair!\");"
	    "}"
	    "lptr p2 = NULL;"
	    "if (@2.vt != LV_NIL) {"
	    "p2 = new_lv(LV_NIL);"
	    "*p2 = @2;"
	    "}"
	    "@1.v.pair.car = p2;"
	    "@@ = @1;"
	    "}")))

   (list 'set-cdr! 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_PAIR) {"
	    "WILE_EX(\"set-cdr!\", \"input is not a pair!\");"
	    "}"
	    "lptr p2 = NULL;"
	    "if (@2.vt != LV_NIL) {"
	    "p2 = new_lv(LV_NIL);"
	    "*p2 = @2;"
	    "}"
	    "@1.v.pair.cdr = p2;"
	    "@@ = @1;"
	    "}")))

   (list 'cxr "expects one control string and one nested list object and returns the appropriate composition of car and cdr as encoded by the control string"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "char* cp = strchr(@1.v.str, 'r');"
	    "@@ = @2;"
	    "while (*(--cp) != 'c') {"
	    "if (@@.vt != LV_PAIR) {"
	    "WILE_EX(\"cxr\", \"input does not have the right structure!\");"
	    "}"
	    "if (*cp == 'a') {"
	    "@@ = (@@.v.pair.car ? *(@@.v.pair.car) : LVI_NIL());"
	    "} else if (*cp == 'd') {"
	    "@@ = (@@.v.pair.cdr ? *(@@.v.pair.cdr) : LVI_NIL());"
	    "} else {"
	    "WILE_EX(\"cxr\", \"got malformed control string '%%s'\", @1.v.str);"
	    "}"
	    "}"
	    "}")))

   (list 'caar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caar" ,a1)))

   (list 'cadr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cadr" ,a1)))

   (list 'cdar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cdar" ,a1)))

   (list 'cddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cddr" ,a1)))

   (list 'caaar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caaar" ,a1)))

   (list 'caadr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caadr" ,a1)))

   (list 'cadar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cadar" ,a1)))

   (list 'caddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caddr" ,a1)))

   (list 'cdaar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cdaar" ,a1)))

   (list 'cdadr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cdadr" ,a1)))

   (list 'cddar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cddar" ,a1)))

   (list 'cdddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cdddr" ,a1)))

   (list 'caaaar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caaaar" ,a1)))

   (list 'caaadr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caaadr" ,a1)))

   (list 'caadar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caadar" ,a1)))

   (list 'caaddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caaddr" ,a1)))

   (list 'cadaar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cadaar" ,a1)))

   (list 'cadadr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cadadr" ,a1)))

   (list 'caddar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caddar" ,a1)))

   (list 'cadddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cadddr" ,a1)))

   (list 'cdaaar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cdaaar" ,a1)))

   (list 'cdaadr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cdaadr" ,a1)))

   (list 'cdadar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cdadar" ,a1)))

   (list 'cdaddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cdaddr" ,a1)))

   (list 'cddaar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cddaar" ,a1)))

   (list 'cddadr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cddadr" ,a1)))

   (list 'cdddar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cdddar" ,a1)))

   (list 'cddddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cddddr" ,a1)))

   (list 'caddddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caddddr" ,a1)))

   (list 'cadddddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cadddddr" ,a1)))

   (list 'caddddddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "caddddddr" ,a1)))

   (list 'cadddddddr "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cadddddddr" ,a1)))

   (list 'pair?
	 "expects one argument and returns #t if that value is a pair, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_PAIR);")))

   (list 'list 'prim -1 build-basic-list)

   (list 'list?
	 "expects one argument and returns #t if that value is a proper list, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "@@ = @1;"
	    "while (@@.vt == LV_PAIR) {"
	    "@@ = (@@.v.pair.cdr ? *(@@.v.pair.cdr) : LVI_NIL());"
	    "}"
	    "@@ = LVI_BOOL(@@.vt == LV_NIL);"
	    "}")))

   (list 'vector "expects any number of values and returns a vector containing those values"
	 'prim -1 "wile_list2vector")
   (list 'bytevector 'prim -1 "wile_list2bytevector")

   ;;; TODO: update for string and sqlite ports?
   (list 'port?
	 "expects one argument and returns #t if that value is a port, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_FILE_PORT || @1.vt == LV_PIPE_PORT || @1.vt == LV_SOCK_PORT);")))

   (list 'file-port?
	 "expects one argument and returns #t if that value is a file-port, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_FILE_PORT);")))

   (list 'pipe-port?
	 "expects one argument and returns #t if that value is a pipe-port, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_PIPE_PORT);")))

   (list 'socket-port?
	 "expects one argument and returns #t if that value is a socket-port, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_SOCK_PORT);")))

   (list 'string-port?
	 "expects one argument and returns #t if that value is a string-port, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_STR_PORT);")))

   (list 'sqlite-port?
	 "expects one argument and returns #t if that value is a sqlite-port, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_SQLITE_PORT);")))

   (list 'sqlite-statement?
	 "expects one argument and returns #t if that value is a sqlite-statement, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_SQLITE_STMT);")))

   (list 'flush-port "expects one optional argument which is a file port, and flushes all bufferend data for that port; if no argument is given, all open output ports are flushed"
	 'prim
	 0 (lambda (r) (emit-code "@@ = LVI_BOOL(fflush(NULL) == 0);"))
	 1 "wile_flushport")

   (list 'open-file "expects two string arguments: the first is the name of a file and the second is the mode in which to open the file. if the operation is successful, return a file-port; otherwise return #f"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "FILE* fp = fopen(@1.v.str, @2.v.str);"
	    "if (fp) {"
	    "@@ = LVI_FPORT(fp);"
	    "} else {"
	    "@@ = LVI_BOOL(false);"
	    "}"
	    "}")))

   (list 'open-temporary-file "expects one string argument which should be a filename template containing at least six 'X' characters (if it does not, they get appended to a copy of the input string); returns a two-list containing a file port opened in 'w+' mode and the actual name of the file that got opened"
	 'prim 1 "wile_temp_file")

   (list 'close-port "expects one expects one port argument, and closes it; returns #t if the close succeeded, #f otherwise"
	 'prim 1 "wile_closeport")

   (list 'create-link 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(link(@1.v.str, @2.v.str) == 0);")))

   (list 'create-symbolic-link 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(symlink(@1.v.str, @2.v.str) == 0);")))

   (list 'eqv? 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(do_eqv(&(@1), &(@2)));")))

   (list 'vector?
	 "expects one argument and returns #t if that value is a vector, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_VECTOR);")))

   (list 'bytevector?
	 "expects one argument and returns #t if that value is a bytevector, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_BVECTOR);")))

   (list 'promise?
	 "expects one argument and returns #t if that value is a promise, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_PROMISE);")))

   (list 'procedure?
	 "expects one argument and returns #t if that value is a procedure, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_LAMBDA);")))

   (list 'continuation?
	 "expects one argument and returns #t if that value is a continuation, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_CONT);")))

   (list 'get-process-id "expects no arguments, returns the process id of the running process" 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(getpid());")))

   (list 'get-parent-process-id "expects no arguments, returns the process id of the parent process of the running process" 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(getppid());")))

   (list 'get-user-id "expects no arguments, returns the user id of the running process" 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(getuid());")))

   (list 'set-user-id "expects one integer and sets the user id of the running process to that value; returns #t if the operation succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(setuid(@1.v.iv) == 0);")))

   (list 'get-effective-user-id "expects no arguments, returns the effective user id of the running process"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(geteuid());")))

   (list 'set-effective-user-id "expects one integer and sets the effective user id of the running process to that value; returns #t if the operation succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(seteuid(@1.v.iv) == 0);")))

   (list 'get-group-id "expects no arguments, returns the group id of the running process" 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(getgid());")))

   (list 'set-group-id "expects one integer and sets the group id of the running process to that value; returns #t if the operation succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(setgid(@1.v.iv) == 0);")))

   (list 'get-effective-group-id "expects no arguments, returns the effective group id of the running process"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(getegid());")))

   (list 'set-effective-group-id "expects one integer and sets the effective group id of the running process to that value; returns #t if the operation succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(setegid(@1.v.iv) == 0);")))

   (list 'get-session-id 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_INT(getsid(@1.v.iv));")))

   (list 'set-session-id 'prim 0
	 (lambda (r)
	   (emit-code
	    "{"
	    "pid_t si = setsid();"
	    "if (si >= 0) {"
	    "@@ = LVI_INT(si);"
	    "} else {"
	    "@@ = LVI_BOOL(false);"
	    "}"
	    "}")))

   (list 'epochtime "expects no arguments, returns the number of seconds since the epoch, 1970-01-01 00:00:00 UTC" 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(time(NULL));")))

   (list 'apply 'prim -3 compile-runtime-apply)

   (list 'gensym
	 "expects no arguments, returns one newly-generated symbol which is supposed to be unique unless the user takes hostile measures to defeat the uniqueness"
	 'prim 0 (lambda (r)
		   (emit-code "@@ = get_gensym();")))

   (list 'run-command "expects one string argument, runs that as a separate process, and returns the exit status of that run or #f if the underlying system() call failed"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = run_system_command(@1, __FILE__, __LINE__);")))

   (list 'run-read-command "expects one string argument, launches that as a separate process while opening a readable pipe to its stdout, and returns that pipe-handle, or #f if the underlying popen() call failed"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = run_pipe_command(@1, \"r\", __FILE__, __LINE__);")))

   (list 'run-write-command "expects one string argument, launches that as a separate process while opening a writable pipe to its stdin, and returns that pipe-handle, or #f if the underlying popen() call failed"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = run_pipe_command(@1, \"w\", __FILE__, __LINE__);")))

   (list 'fork-process "expects no arguments and forks the process into parent and child processes; returns 0 in the child process and the child's process id in the parent process, or #f if the underlying fork() call failed"
	 'prim 0
	 (lambda (r)
	   (emit-code
	    "{"
	    "int si = fork();"
	    "if (si >= 0) {"
	    "@@ = LVI_INT(si);"
	    "} else {"
	    "@@ = LVI_BOOL(false);"
	    "}"
	    "}")))

   (list 'get-current-directory "expects no arguments and returns a string which is the name of the current working directory" 'prim 0 "wile_getcwd")

   (list 'set-current-directory "expects one string argument, attempts to change the current working directory to that location, and returns #t if that succeeds or #f if it fails"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(chdir(@1.v.str) == 0);")))

   (list 'get-file-position 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "if (@1.vt == LV_FILE_PORT) {"
	    "long int offset = ftell(@1.v.fp);"
	    "if (offset < 0) {"
	    "@@ = LVI_BOOL(false);"
	    "} else {"
	    "@@ = LVI_INT(offset);"
	    "}"
	    "} else {"
	    "WILE_EX(\"get-file-position\", \"input is not a file port\");"
	    "}")))

   ;;; offset relative to start for 2-arg
   (list 'set-file-position 'prim 2 "wile_setfilepos2" 3 "wile_setfilepos3")
   (list 'set-line-buffering! "expects one argument which is a port of some kind: for file, pipe, or socket-type ports, set buffering mode to line buffering; for string or sqlite ports, this is a no-op. return #t or #f according to whether the call succeeds or fails"
	 'prim 1 "wile_setlinebuffering")
   (list 'set-no-buffering! "expects one argument which is a port of some kind: for file, pipe, or socket-type ports, set buffering mode to no buffering; for string or sqlite ports, this is a no-op. return #t or #f according to whether the call succeeds or fails"
	 'prim 1 "wile_setnobuffering")
   (list 'get-host-name "expects no arguments and returns the system's host name as given by gethostname(), or #f if gethostname fails"
	 'prim 0 "wile_gethostname")
   (list 'get-domain-name "expects no arguments and returns the system's domain name as given by getdomainname(), or #f if getdomainname fails"
	 'prim 0 "wile_getdomainname")
   (list 'read-line 'prim 1 "wile_read_line")

   (list 'read-char "expects no arguments or one port argument, attempts to read one character from stdin or the port, and returns the character if the read was successful, #f otherwise"
	 'prim
	 0 (lambda (r)
	     (emit-code
	      "{"
	      "int c = fgetc(stdin);"
	      "@@ = ((c == EOF) ? LVI_BOOL(false) : LVI_CHAR(c));"
	      "}"))
	 1 (lambda (r a1)
	     (emit-code
	      "if (@1.vt == LV_FILE_PORT || @1.vt == LV_PIPE_PORT || @1.vt == LV_SOCK_PORT) {"
	      "int c = fgetc(@1.v.fp);"
	      "@@ = ((c == EOF) ? LVI_BOOL(false) : LVI_CHAR(c));"
	      "} else {"
	      "WILE_EX(\"read-char\", \"input is not a port\");"
	      "}")))

   (list 'newline "expects one optional argument which is an output port, and writes a newline character to that port; the default port if no argument is given is stdout"
	 'prim
	 0 (lambda (r)
	     (emit-code
	      "@@ = LVI_BOOL(true);"
	      "putchar('\\n');"))
	 1 (lambda (r a1)
	     (emit-code
	      "if (@1.vt == LV_FILE_PORT || @1.vt == LV_PIPE_PORT || @1.vt == LV_SOCK_PORT) {"
	      "fputc('\\n', @1.v.fp);"
	      "@@ = LVI_BOOL(true);"
	      "} else {"
	      "WILE_EX(\"newline\", \"input is not a port\");"
	      "}")))

   (list 'write-1str "expects one or two arguments: either just a string or char, in which case the output port defaults to stdout, or an output port and a string or char, and writes the string or char to the output port; returns #t"
	 'prim
	 1 (lambda (r a1)
	     (emit-code
	      "if (@1.vt == LV_CHAR) {"
	      "fputc(@1.v.chr, stdout);"
	      "} else if (@1.vt == LV_STRING) {"
	      "fputs(@1.v.str, stdout);"
	      "} else {"
	      "WILE_EX(\"write-string\", \"input is not a string or char!\");"
	      "}"
	      "@@ = LVI_BOOL(true);"))
	 2 (lambda (r a1 a2)
	     (emit-code
	      "{"
	      "FILE* fp;"
	      "if (@1.vt == LV_FILE_PORT || @1.vt == LV_PIPE_PORT || @1.vt == LV_SOCK_PORT) {"
	      "fp = @1.v.fp;"
	      "} else {"
	      "WILE_EX(\"write-string\", \"first input is not a port!\");"
	      "}"
	      "if (@2.vt == LV_CHAR) {"
	      "fputc(@2.v.chr, fp);"
	      "} else if (@2.vt == LV_STRING) {"
	      "fputs(@2.v.str, fp);"
	      "} else {"
	      "WILE_EX(\"write-string\", \"second input is not a string or char!\");"
	      "}"
	      "@@ = LVI_BOOL(true);"
	      "}")))

   (list 'display 'prim
	 1 (lambda (r a1)
	     (emit-code
	      "display(@1, stdout);"
	      "@@ = @1;"))
	 2 (lambda (r a1 a2)
	     (emit-code
	      "if (@2.vt == LV_FILE_PORT || @2.vt == LV_PIPE_PORT || @2.vt == LV_SOCK_PORT) {"
	      "display(@1, @2.v.fp);"
	      "@@ = @1;"
	      "} else {"
	      "WILE_EX(\"display\", \"expects a scheme value and a port\");"
	      "}")))

   (list 'string->number "expects one string, parses it as a number, and returns the number"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = string2num(@1, __FILE__, __LINE__);")))

   (list 'number->string 'prim
	 ;;; number
	 1 (lambda (r a1)
	     (emit-code
	      "@@ = num2string(@1, 10, INT_MIN, __FILE__, __LINE__);"))
	 ;;; number base
	 2 (lambda (r a1 a2)
	     (emit-code
	      "if (@2.vt == LV_INT) {"
	      "@@ = num2string(@1, @2.v.iv, INT_MIN, __FILE__, __LINE__);"
	      "} else {"
	      "WILE_EX(\"number->string\", \"base is not numeric\");"
	      "}"))
	 ;;; number base precision
	 3 (lambda (r a1 a2 a3)
	     (emit-code
	      "if (@2.vt == LV_INT && @3.vt == LV_INT) {"
	      "@@ = num2string(@1, @2.v.iv, @3.v.iv, __FILE__, __LINE__);"
	      "} else {"
	      "WILE_EX(\"number->string\", \"base or precision is not numeric\");"
	      "}")))

   (list 'cmplx "expects two real-valued arguments X and Y and returns the complex number X+I*Y"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar))
		 (a8 (new-svar)))
	     (promote/real+check "cmplx" a9 a1)
	     (promote/real+check "cmplx" a8 a2)
	     (emit-code "@@ = LVI_CMPLX2(@9.v.rv, @8.v.rv);"))))

   (list 'make-polar "expects two real-valued arguments R and THETA and returns the complex number R*exp(I*THETA)"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar))
		 (a8 (new-svar)))
	     (promote/real+check "cmplx" a9 a1)
	     (promote/real+check "cmplx" a8 a2)
	     (emit-code
	      "@@ = LVI_CMPLX2(@9.v.rv*COS(@8.v.rv), @9.v.rv*SIN(@8.v.rv));"))))

   (list 'angle 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_REAL(ATAN2(CIMAG(@1.v.cv), CREAL(@1.v.cv)));")))

   (list 'cconj "expects one complex argument and returns its complex conjugate"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CMPLX2(CREAL(@1.v.cv), -CIMAG(@1.v.cv));")))

   (list 'even? "expects one integer and returns #t if that integer is even, #f if odd"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL((@1.v.iv)%%2 == 0);")))

   (list 'odd? "expects one integer and returns #t if that integer is odd, #f if even"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL((@1.v.iv)%%2 == 1);")))

   (list 'zero? "expects one number and returns #t if that number is 0. the number may be of any type"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_REAL:"
	    "@@ = LVI_BOOL(@1.v.rv == 0.0);"
	    "break;"
	    "case LV_RAT:"
	    "@@ = LVI_BOOL((@1.v.irv.num == 0 && @1.v.irv.den != 0));"
	    "break;"
	    "case LV_INT:"
	    "@@ = LVI_BOOL(@1.v.iv == 0);"
	    "break;"
	    "case LV_CMPLX:"
	    "@@ = LVI_BOOL(CREAL(@1.v.cv) == 0.0 && CIMAG(@1.v.cv) == 0.0);"
	    "break;"
	    "default:"
	    "WILE_EX(\"zero?\", \"expects a real-valued number\");"
	    "}")))

   (list 'positive? "expects one real-valued number and returns #t if that number is positive. the number may be of any type except complex"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_REAL:"
	    "@@ = LVI_BOOL(@1.v.rv > 0.0);"
	    "break;"
	    "case LV_RAT:"
	    "@@ = LVI_BOOL((@1.v.irv.num > 0 && @1.v.irv.den >= 0) || (@1.v.irv.num < 0 && @1.v.irv.den < 0));"
	    "break;"
	    "case LV_INT:"
	    "@@ = LVI_BOOL(@1.v.iv > 0);"
	    "break;"
	    "default:"
	    "WILE_EX(\"positive?\", \"expects a real-valued number\");"
	    "}")))

   (list 'negative? "expects one real-valued number and returns #t if that number is negative. the number may be of any type except complex"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_REAL:"
	    "@@ = LVI_BOOL(@1.v.rv < 0.0);"
	    "break;"
	    "case LV_RAT:"
	    "@@ = LVI_BOOL((@1.v.irv.num < 0 && @1.v.irv.den >= 0) || (@1.v.irv.num > 0 && @1.v.irv.den < 0));"
	    "break;"
	    "case LV_INT:"
	    "@@ = LVI_BOOL(@1.v.iv < 0);"
	    "break;"
	    "default:"
	    "WILE_EX(\"negative?\", \"expects a real-valued number\");"
	    "}")))

   (list 'null? "expects one argument and returns #t if that value is (), #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_NIL);")))

   (list 'char? "expects one argument and returns #t if that value is a char, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_CHAR);")))

   (list 'boolean? "expects one argument and returns #t if that value is a boolean, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_BOOL);")))

   (list 'string? "expects one argument and returns #t if that value is a string, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_STRING);")))

   (list 'symbol? "expects one argument and returns #t if that value is a symbol, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_SYMBOL);")))

   (list 'integer? "expects one argument and returns #t if that value is an integer by type, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_INT);")))

   (list 'rational? "expects one argument and returns #t if that value is a rational or an integer by type, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_RAT || @1.vt == LV_INT);")))

   (list 'real? "expects one argument and returns #t if that value is a real, a rational, or an integer by type, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_REAL || @1.vt == LV_RAT || @1.vt == LV_INT);")))

   (list 'complex? "expects one argument and returns #t if that value is a number by type, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_CMPLX || @1.vt == LV_REAL || @1.vt == LV_RAT || @1.vt == LV_INT);")))

   (list 'nan? "expects one numeric argument and returns #t if it is a NaN, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_CMPLX:"
	    "@@ = LVI_BOOL(ISNAN(CREAL(@1.v.cv)) || ISNAN(CIMAG(@1.v.cv)));"
	    "break;"
	    "case LV_REAL:"
	    "@@ = LVI_BOOL(ISNAN(@1.v.rv));"
	    "break;"
	    "case LV_RAT:"
	    "@@ = LVI_BOOL(@1.v.irv.num == 0 && @1.v.irv.den == 0);"
	    "break;"
	    "default:"
	    "@@ = LVI_BOOL(false);"
	    "break;"
	    "}")))

   (list 'infinite? "expects one numeric argument and returns #t if it is infinite, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_CMPLX:"
	    "@@ = LVI_BOOL(ISINF(CREAL(@1.v.cv)) || ISINF(CIMAG(@1.v.cv)));"
	    "break;"
	    "case LV_REAL:"
	    "@@ = LVI_BOOL(ISINF(@1.v.rv));"
	    "break;"
	    "case LV_RAT:"
	    "@@ = LVI_BOOL(@1.v.irv.num != 0 && @1.v.irv.den == 0);"
	    "break;"
	    "default:"
	    "@@ = LVI_BOOL(false);"
	    "break;"
	    "}")))

   ;;; everything is #t except #f so only #f becomes #t,
   ;;; everything else becomes #f
   (list 'not
	 "expects one argument and returns #t if that value is #f, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(LV_IS_FALSE(@1));")))

   (list 'finite? "expects one numeric argument and returns #t if it is finite, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_CMPLX:"
	    "@@ = LVI_BOOL(ISFINITE(CREAL(@1.v.cv)) && ISFINITE(CIMAG(@1.v.cv)));"
	    "break;"
	    "case LV_REAL:"
	    "@@ = LVI_BOOL(ISFINITE(@1.v.rv));"
	    "break;"
	    "case LV_RAT:"
	    "@@ = LVI_BOOL(@1.v.irv.den != 0);"
	    "break;"
	    "default:"
	    "@@ = LVI_BOOL(true);"
	    "break;"
	    "}")))

   (list 'abs "expects one numeric argument and returns its absolute value"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_INT:"
	    "@@ = LVI_INT(WILE_ABS(@1.v.iv));"
	    "break;"
	    "case LV_RAT:"
	    "@@ = LVI_RAT(WILE_ABS(@1.v.irv.num), WILE_ABS(@1.v.irv.den));"
	    "break;"
	    "case LV_REAL:"
	    "@@ = LVI_REAL(WILE_ABS(@1.v.rv));"
	    "break;"
	    "case LV_CMPLX:"
	    "@@ = LVI_REAL(CABS(@1.v.cv));"
	    "break;"
	    "default:"
	    "WILE_EX(\"abs\", \"got a non-numeric argument\");"
	    "}")))

   (list 'sign "expects one real-valued argument and returns its sign, -1,0,+1 according to whether the value is negative, zero, or positive"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_INT:"
	    "@@ = LVI_INT(WILE_SIGN(@1.v.iv));"
	    "break;"
	    "case LV_RAT:"
	    "@@ = LVI_INT(WILE_SIGN(@1.v.irv.num));"
	    "if (@1.v.irv.den < 0) {"
	    "@@.v.iv = -@@.v.iv;"
	    "}"
	    "break;"
	    "case LV_REAL:"
	    "@@ = LVI_INT(WILE_SIGN(@1.v.rv));"
	    "break;"
	    "default:"
	    "WILE_EX(\"sign\", \"got a non-real-valued argument\");"
	    "}")))

   (list 'numerator "expects one rational value and returns its numerator"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_INT(@1.v.irv.num);")))

   (list 'denominator "expects one rational value and returns its denominator"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_INT(@1.v.irv.den);")))

   (list 'real-part "expects one complex number and returns its real part"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_REAL(CREAL(@1.v.cv));")))

   (list 'imag-part "expects one complex number and returns its imaginary part"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_REAL(CIMAG(@1.v.cv));")))

   (list 'negative "expects one number and returns its negative"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_INT:"
	    "@@ = LVI_INT(-@1.v.iv);"
	    "break;"
	    "case LV_RAT:"
	    "if (@1.v.irv.den >= 0) {"
	    "@@ = LVI_RAT(-@1.v.irv.num, @1.v.irv.den);"
	    "} else {"
	    "@@ = LVI_RAT(@1.v.irv.num, -@1.v.irv.den);"
	    "}"
	    "break;"
	    "case LV_REAL:"
	    "@@ = LVI_REAL(-@1.v.rv);"
	    "break;"
	    "case LV_CMPLX:"
	    "@@ = LVI_CMPLX2(-CREAL(@1.v.cv), -CIMAG(@1.v.cv));"
	    "break;"
	    "default:"
	    "WILE_EX(\"negative\", \"got a non-numeric argument\");"
	    "}")))

   (list 'reciprocal  "expects one number and returns its reciprocal"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_INT:"
	    "if (@1.v.iv < 0) {"
	    "@@ = LVI_RAT(-1, -@1.v.iv);"
	    "} else {"
	    "@@ = LVI_RAT(1, @1.v.iv);"
	    "}"
	    "break;"
	    "case LV_RAT:"
	    "if (@1.v.irv.num < 0) {"
	    "@@ = LVI_RAT(-@1.v.irv.den, -@1.v.irv.num);"
	    "} else {"
	    "@@ = LVI_RAT(@1.v.irv.den, @1.v.irv.num);"
	    "}"
	    "break;"
	    "case LV_REAL:"
	    "@@ = LVI_REAL(1.0/@1.v.rv);"
	    "break;"
	    "case LV_CMPLX:"
	    "@@ = LVI_CMPLX1(1.0/@1.v.cv);"
	    "break;"
	    "default:"
	    "WILE_EX(\"reciprocal\", \"got a non-numeric argument\");"
	    "}")))

   (list 'i+ "expects two integer-typed numbers and returns their sum, also as an integer-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT(@1.v.iv + @2.v.iv);")))

   (list 'i++ 'prim -3
	 (lambda (r a1 a2 . as)
	   (emit-decl r)
	   (emit-fstr "%s = LVI_INT(%s.v.iv + %s.v.iv" r a1 a2)
	   (for-each (lambda (a) (emit-fstr " + %s.v.iv" a)) as)
	   (emit-fstr ");\n")
	   r))

   (list 'i- "expects two integer-typed numbers and returns their difference, also as an integer-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT(@1.v.iv - @2.v.iv);")))

   (list 'i-- 'prim -3
	 (lambda (r a1 a2 . as)
	   (emit-decl r)
	   (emit-fstr "%s = LVI_INT(%s.v.iv - %s.v.iv" r a1 a2)
	   (for-each (lambda (a) (emit-fstr " - %s.v.iv" a)) as)
	   (emit-fstr ");\n")
	   r))

   (list 'i* "expects two integer-typed numbers and returns their product, also as an integer-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT(@1.v.iv * @2.v.iv);")))

   (list 'i/ "expects two integer-typed numbers and returns their ratio, as a rational-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "if (@2.v.iv < 0) {"
	    "@@ = LVI_RAT(-@1.v.iv, -@2.v.iv);"
	    "} else {"
	    "@@ = LVI_RAT(@1.v.iv, @2.v.iv);"
	    "}")))

   (list 'q+ "expects two rational-typed numbers and returns their sum, also as a rational-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lisp_int_t n, d, g;"
	    "n = @1.v.irv.num * @2.v.irv.den + @2.v.irv.num * @1.v.irv.den;"
	    "d = @1.v.irv.den * @2.v.irv.den;"
	    "g = lgcd(n, d);"
	    "n /= g;"
	    "d /= g;"
	    "@@ = LVI_RAT(n, d);"
	    "}")))

   (list 'q- "expects two rational-typed numbers and returns their difference, also as a rational-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lisp_int_t n, d, g;"
	    "n = @1.v.irv.num * @2.v.irv.den - @2.v.irv.num * @1.v.irv.den;"
	    "d = @1.v.irv.den * @2.v.irv.den;"
	    "g = lgcd(n, d);"
	    "n /= g;"
	    "d /= g;"
	    "@@ = LVI_RAT(n, d);"
	    "}")))

   (list 'q* "expects two rational-typed numbers and returns their product, also as a rational-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lisp_int_t n, d, g;"
	    "n = @1.v.irv.num * @2.v.irv.num;"
	    "d = @1.v.irv.den * @2.v.irv.den;"
	    "g = lgcd(n, d);"
	    "n /= g;"
	    "d /= g;"
	    "@@ = LVI_RAT(n, d);"
	    "}")))

   (list 'q/ "expects two rational-typed numbers and returns their ratio, also as a rational-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lisp_int_t n, d, g;"
	    "n = @1.v.irv.num * @2.v.irv.den;"
	    "d = @1.v.irv.den * @2.v.irv.num;"
	    "g = lgcd(n, d);"
	    "if (d < 0) {"
	    "g = -g;"
	    "}"
	    "n /= g;"
	    "d /= g;"
	    "@@ = LVI_RAT(n, d);"
	    "}")))

   (list 'r+ "expects two real-typed numbers and returns their sum, also as a real-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL(@1.v.rv + @2.v.rv);")))

   (list 'r- "expects two real-typed numbers and returns their difference, also as a real-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL(@1.v.rv - @2.v.rv);")))

   (list 'r* "expects two real-typed numbers and returns their product, also as a real-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL(@1.v.rv * @2.v.rv);")))

   (list 'r/ "expects two real-typed numbers and returns their ratio, also as a real-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL(@1.v.rv / @2.v.rv);")))

   (list 'c+ "expects two complex-typed numbers and returns their sum, also as a complex-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_CMPLX1(@1.v.cv + @2.v.cv);")))

   (list 'c- "expects two complex-typed numbers and returns their difference, also as a complex-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_CMPLX1(@1.v.cv - @2.v.cv);")))

   (list 'c* "expects two complex-typed numbers and returns their product, also as a complex-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_CMPLX1(@1.v.cv * @2.v.cv);")))

   (list 'c/ "expects two complex-typed numbers and returns their ratio, also as a complex-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_CMPLX1(@1.v.cv / @2.v.cv);")))

   (list 'min/i "expects two integer-typed values, and returns the smaller of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@1.v.iv < @2.v.iv) ? @1.v.iv : @2.v.iv);")))

   (list 'max/i "expects two integer-typed values, and returns the larger of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@1.v.iv > @2.v.iv) ? @1.v.iv : @2.v.iv);")))

   (list 'min/q "expects two rational-typed values, and returns the smaller of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "if (@1.v.irv.num * @2.v.irv.den < @2.v.irv.num * @1.v.irv.den) {"
	    "@@ = @1;"
	    "} else {"
	    "@@ = @2;"
	    "}")))

   (list 'max/q "expects two rational-typed values, and returns the larger of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "if (@1.v.irv.num * @2.v.irv.den > @2.v.irv.num * @1.v.irv.den) {"
	    "@@ = @1;"
	    "} else {"
	    "@@ = @2;"
	    "}")))

   (list 'min/r "expects two real-typed values, and returns the smaller of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL((@1.v.rv < @2.v.rv) ? @1.v.rv : @2.v.rv);")))

   (list 'max/r "expects two real-typed values, and returns the larger of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL((@1.v.rv > @2.v.rv) ? @1.v.rv : @2.v.rv);")))

   ;;; no such thing as min/c or max/c

   ;;; return the number-tower type of a number:
   ;;; int 0, rat 1, real 2, complex 3, all other values 4

   (list 'number/type "expects one value: if {integer,rational,real,complex} type respectively, return {0,1,2,3}; for all others, return 4"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_INT:"
	    "@@ = LVI_INT(0);"
	    "break;"
	    "case LV_RAT:"
	    "@@ = LVI_INT(1);"
	    "break;"
	    "case LV_REAL:"
	    "@@ = LVI_INT(2);"
	    "break;"
	    "case LV_CMPLX:"
	    "@@ = LVI_INT(3);"
	    "break;"
	    "default:"
	    "@@ = LVI_INT(4);"
	    "break;"
	    "}")))

   ;;; ditto for vector

   (list 'vector-number/type 'prim 1
	 (lambda (r a1)
	   (let ((a9 (new-svar 'lbl)))
	     (emit-code
	      "{"
	      "if (@1.vt != LV_VECTOR) {"
	      "WILE_EX(\"vector-number/type\", \"input is not a vector\");"
	      "}"
	      "int ty = 0;"
	      "size_t i;"
	      "for (i = 0; i < @1.v.vec.capa; ++i) {"
	      "if (@1.v.vec.arr[i]) {"
	      "switch (@1.v.vec.arr[i]->vt) {"
	      "case LV_INT:"
	      "break;"
	      "case LV_RAT:"
	      "if (ty < 1) {"
	      "ty = 1;"
	      "}"
	      "break;"
	      "case LV_REAL:"
	      "if (ty < 2) {"
	      "ty = 2;"
	      "}"
	      "break;"
	      "case LV_CMPLX:"
	      "if (ty < 3) {"
	      "ty = 3;"
	      "}"
	      "break;"
	      "default:"
	      "ty = 4;"
	      "goto @9;"
	      "}"
	      "} else {"
	      "ty = 4;"
	      "goto @9;"
	      "}"
	      "}"
	      "@9:"
	      "@@ = LVI_INT(ty);"
	      "}"))))

   ;;; promote ints to rationals, leave all else untouched

   (list 'promote/rat "expects one value; integers are promoted to rational type, all others are returned unchanged"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "if (@1.vt == LV_INT) {"
	    "@@ = LVI_RAT(@1.v.iv, 1);"
	    "} else {"
	    "@@ = @1;"
	    "}")))

   ;;; ditto for vector

   (list 'vector-promote/rat! 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_VECTOR) {"
	    "WILE_EX(\"vector-promote/rat!\", \"input is not a vector\");"
	    "}"
	    "size_t i;"
	    "for (i = 0; i < @1.v.vec.capa; ++i) {"
	    "if (@1.v.vec.arr[i] && @1.v.vec.arr[i]->vt == LV_INT) {"
	    "*(@1.v.vec.arr[i]) = LVI_RAT(@1.v.vec.arr[i]->v.iv, 1);"
	    "}"
	    "}"
	    "@@ = @1;"
	    "}")))

   ;;; promote ints and rationals to reals, leave all else untouched

   (list 'promote/real "expects one value; integers and rationals are promoted to real type, all others are returned unchanged"
	 'prim 1 promote/real)

   ;;; ditto for vector

   (list 'vector-promote/real! 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_VECTOR) {"
	    "WILE_EX(\"vector-promote/real!\", \"input is not a vector\");"
	    "}"
	    "size_t i;"
	    "for (i = 0; i < @1.v.vec.capa; ++i) {"
	    "if (@1.v.vec.arr[i]) {"
	    "if (@1.v.vec.arr[i]->vt == LV_INT) {"
	    "*(@1.v.vec.arr[i]) = LVI_REAL((lisp_real_t) (@1.v.vec.arr[i]->v.iv));"
	    "} else if (@1.v.vec.arr[i]->vt == LV_RAT) {"
	    "*(@1.v.vec.arr[i]) = LVI_REAL(LV_RAT2REAL(*(@1.v.vec.arr[i])));"
	    "}"
	    "}"
	    "@@ = @1;"
	    "}"
	    "}")))

   ;;; promote ints rationals and reals to complex, leave all else untouched

   (list 'promote/cmplx "expects one value; integers, rationals, and reals are promoted to complex type, all others are returned unchanged"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "switch (@1.vt) {"
	    "case LV_INT:"
	    "@@ = LVI_CMPLX2((lisp_real_t) @1.v.iv, 0);"
	    "break;"
	    "case LV_RAT:"
	    "@@ = LVI_CMPLX2(LV_RAT2REAL(@1), 0);"
	    "break;"
	    "case LV_REAL:"
	    "@@ = LVI_CMPLX2(@1.v.rv, 0);"
	    "break;"
	    "default:"
	    "@@ = @1;"
	    "break;"
	    "}")))

   ;;; ditto for vector

   (list 'vector-promote/cmplx! 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_VECTOR) {"
	    "WILE_EX(\"vector-promote/cmplx!\", \"input is not a vector\");"
	    "}"
	    "size_t i;"
	    "for (i = 0; i < @1.v.vec.capa; ++i) {"
	    "if (@1.v.vec.arr[i]) {"
	    "if (@1.v.vec.arr[i]->vt == LV_INT) {"
	    "*(@1.v.vec.arr[i]) = LVI_CMPLX2((lisp_real_t) (@1.v.vec.arr[i]->v.iv), 0.0);"
	    "} else if (@1.v.vec.arr[i]->vt == LV_RAT) {"
	    "*(@1.v.vec.arr[i]) = LVI_CMPLX2(LV_RAT2REAL(*(@1.v.vec.arr[i])), 0.0);"
	    "} else if (@1.v.vec.arr[i]->vt == LV_REAL) {"
	    "*(@1.v.vec.arr[i]) = LVI_CMPLX2(@1.v.vec.arr[i]->v.rv, 0.0);"
	    "}"
	    "}"
	    "@@ = @1;"
	    "}"
	    "}")))

   ;;; constructor to make numbers in rational format; it is UNSAFE!
   ;;; it can create numbers in non-canonical formats, 3/6, and also
   ;;; 0/1, 0/0 aka NaN, 1/0 and -1/0 aka infinities. use with care!

   (list 'make-rational "expects two integers N and D and returns the rational number N/D; note that this can construct non-canonical numbers such as 3/6, 0/1, 1/0, etc"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_RAT(@1.v.iv, @2.v.iv);")))

   (list 'gcd "expects two integer arguments and returns their greatest common divisor"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_INT(lgcd(@1.v.iv, @2.v.iv));")))

   (list 'lcm "expects two integer arguments and returns their least common multiple"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_INT(@1.v.iv*(@2.v.iv/lgcd(@1.v.iv, @2.v.iv)));")))

   (list 'ilog 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "lisp_int_t v, c;"
	    "v = @1.v.iv;"
	    "if (v < 0) {"
	    "v = -v;"
	    "}"
	    "if (v == 0) {"
	    "c = 0;"
	    "} else {"
	    "c = -1;"
	    "while (v > 0) {"
	    "v /= 2;"
	    "++c;"
	    "}"
	    "}"
	    "@@ = LVI_INT(c);"
	    "}")))

   (list '< "expects two real-valued numbers and returns #t if the first is less than the second, #f otherwise"
	 'prim 2
;;;	 (curry build-real-cmp "<")
	 (lambda (r a1 a2) (build-real-cmp "<" r a1 a2)))

   (list '<= "expects two real-valued numbers and returns #t if the first is less than or equal to the second, #f otherwise"
	 'prim 2
;;;	 (curry build-real-cmp "<=")
	 (lambda (r a1 a2) (build-real-cmp "<=" r a1 a2)))

   (list '= "expects two real-valued numbers and returns #t if the first is equal to the second, #f otherwise"
	 'prim 2
;;;	 (curry build-real-cmp "==")
	 (lambda (r a1 a2) (build-real-cmp "==" r a1 a2)))

   (list '>= "expects two real-valued numbers and returns #t if the first is greater than or equal to the second, #f otherwise"
	 'prim 2
;;;	 (curry build-real-cmp ">=")
	 (lambda (r a1 a2) (build-real-cmp ">=" r a1 a2)))

   (list '> "expects two real-valued numbers and returns #t if the first is greater than the second, #f otherwise"
	 'prim 2
;;;	 (curry build-real-cmp ">")
	 (lambda (r a1 a2) (build-real-cmp ">" r a1 a2)))

   (list '/= "expects two real-valued numbers and returns #t if the first is unequal to the second, #f otherwise"
	 'prim 2
;;;	 (curry build-real-cmp "!=")
	 (lambda (r a1 a2) (build-real-cmp "!=" r a1 a2)))

   (list 'char<? "expects two character inputs and returns #t if the first is lexicographically less than the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(@1.v.chr < @2.v.chr);")))

   (list 'char<=? "expects two character inputs and returns #t if the first is lexicographically less than or equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(@1.v.chr <= @2.v.chr);")))

   (list 'char=?  "expects two character inputs and returns #t if the first is equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(@1.v.chr == @2.v.chr);")))

   (list 'char>=? "expects two character inputs and returns #t if the first is lexicographically greater than or equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(@1.v.chr >= @2.v.chr);")))

   (list 'char>? "expects two character inputs and returns #t if the first is lexicographically greater than the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(@1.v.chr > @2.v.chr);")))

   (list 'char/=? "expects two character inputs and returns #t if the first is unequal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(@1.v.chr != @2.v.chr);")))

   (list 'char-ci<? "expects two character inputs and returns #t if, ignoring case, the first is lexicographically less than the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(tolower(@1.v.chr) < tolower(@2.v.chr));")))

   (list 'char-ci<=? "expects two character inputs and returns #t if, ignoring case, the first is lexicographically less than or equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(tolower(@1.v.chr) <= tolower(@2.v.chr));")))

   (list 'char-ci=? "expects two character inputs and returns #t if, ignoring case, the first is lexicographically equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(tolower(@1.v.chr) == tolower(@2.v.chr));")))

   (list 'char-ci>=? "expects two character inputs and returns #t if, ignoring case, the first is lexicographically greater than or equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(tolower(@1.v.chr) >= tolower(@2.v.chr));")))

   (list 'char-ci>? "expects two character inputs and returns #t if, ignoring case, the first is lexicographically greater than the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(tolower(@1.v.chr) > tolower(@2.v.chr));")))

   (list 'char-ci/=? "expects two character inputs and returns #t if, ignoring case, the first is unequal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(tolower(@1.v.chr) != tolower(@2.v.chr));")))

   (list 'string<? "expects two string inputs and returns #t if the first is lexicographically less than the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcmp(@1.v.str, @2.v.str) < 0);")))

   (list 'string<=? "expects two string inputs and returns #t if the first is lexicographically less than or equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcmp(@1.v.str, @2.v.str) <= 0);")))

   (list 'string=? "expects two string inputs and returns #t if the first is lexicographically equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcmp(@1.v.str, @2.v.str) == 0);")))

   (list 'string>=? "expects two string inputs and returns #t if the first is lexicographically greater than or equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcmp(@1.v.str, @2.v.str) >= 0);")))

   (list 'string>? "expects two string inputs and returns #t if the first is lexicographically greater than the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcmp(@1.v.str, @2.v.str) > 0);")))

   (list 'string/=? "expects two string inputs and returns #t if the first is unequal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcmp(@1.v.str, @2.v.str) != 0);")))

   (list 'string-ci<? "expects two string inputs and returns #t if, ignoring case, the first is lexicographically less than the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcasecmp(@1.v.str, @2.v.str) < 0);")))

   (list 'string-ci<=? "expects two string inputs and returns #t if, ignoring case, the first is lexicographically less than or equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcasecmp(@1.v.str, @2.v.str) <= 0);")))

   (list 'string-ci=? "expects two string inputs and returns #t if, ignoring case, the first is equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcasecmp(@1.v.str, @2.v.str) == 0);")))

   (list 'string-ci>=? "expects two string inputs and returns #t if, ignoring case, the first is lexicographically greater than or equal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcasecmp(@1.v.str, @2.v.str) >= 0);")))

   (list 'string-ci>? "expects two string inputs and returns #t if, ignoring case, the first is lexicographically greater than the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcasecmp(@1.v.str, @2.v.str) > 0);")))

   (list 'string-ci/=? "expects two string inputs and returns #t if, ignoring case, the first is unequal to the second, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcasecmp(@1.v.str, @2.v.str) != 0);")))

   (list 'char-alphabetic? "expects one char argument and returns #t if that char is alphabetic, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(isalpha(@1.v.chr));")))

   (list 'char-numeric? "expects one char argument and returns #t if that char is numeric, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(isdigit(@1.v.chr));")))

   (list 'char-alphanumeric? "expects one char argument and returns #t if that char is alphanumeric, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(isalnum(@1.v.chr));")))

   (list 'char-oct-digit? "expects one char argument and returns #t if that char is a valid octal digit, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_BOOL(isdigit(@1.v.chr) && @1.v.chr != '8' && @1.v.chr != '9');")))

   (list 'char-hex-digit? "expects one char argument and returns #t if that char is a valid hexadecimal digit, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(isxdigit(@1.v.chr));")))

   (list 'char-whitespace? "expects one char argument and returns #t if that char is whitespace, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(isspace(@1.v.chr));")))

   (list 'char-lowercase? "expects one char argument and returns #t if that char is lowercase, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(islower(@1.v.chr));")))

   (list 'char-uppercase? "expects one char argument and returns #t if that char is uppercase, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(isupper(@1.v.chr));")))

   (list 'char-printable? "expects one char argument and returns #t if that char is printable, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(isprint(@1.v.chr));")))

   (list 'char-control? "expects one char argument and returns #t if that char is a control character, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(iscntrl(@1.v.chr));")))

   (list 'char-downcase "expects one char argument and returns the lowercase version"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CHAR(tolower(@1.v.chr));")))

   (list 'char-upcase "expects one char argument and returns the uppercase version"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CHAR(toupper(@1.v.chr));")))

   (list 'string-create 'prim
	 1 (lambda (r a1)
	     (emit-code
	      "if (@1.vt != LV_INT || @1.v.iv < 0) {"
	      "WILE_EX(\"string-create\", \"input is not a non-negative integer\");"
	      "}"
	      "@@.vt = LV_STRING;"
	      "@@.v.str = LISP_ALLOC(char, 1 + @1.v.iv);"
	      "LISP_ASSERT(@@.v.str != NULL);"
	      "memset(@@.v.str, 'X', @1.v.iv);"
	      "@@.v.str[@1.v.iv] = '\\0';"))
	 2 (lambda (r a1 a2)
	     (emit-code
	      "if (@1.vt != LV_INT || @1.v.iv < 0) {"
	      "WILE_EX(\"string-create\", \"first input is not a non-negative integer\");"
	      "}"
	      "if (@2.vt != LV_CHAR || @2.v.chr == '\\0') {"
	      "WILE_EX(\"string-create\", \"second input is not a valid character\");"
	      "}"
	      "@@.vt = LV_STRING;"
	      "@@.v.str = LISP_ALLOC(char, 1 + @1.v.iv);"
	      "LISP_ASSERT(@@.v.str != NULL);"
	      "memset(@@.v.str, @2.v.chr, @1.v.iv);"
	      "@@.v.str[@1.v.iv] = '\\0';")))

   (list 'string-downcase "expects one string argument and returns a newly-allocated lower-cased version of the input"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_STRING(@1.v.str);"
	    "{"
	    "char* sp = @@.v.str;"
	    "while (*sp) {"
	    "*sp = tolower(*sp);"
	    "++sp;"
	    "}"
	    "}")))

   (list 'string-upcase "expects one string argument and returns a newly-allocated upper-cased version of the input"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_STRING(@1.v.str);"
	    "{"
	    "char* sp = @@.v.str;"
	    "while (*sp) {"
	    "*sp = toupper(*sp);"
	    "++sp;"
	    "}"
	    "}")))

   (list 'string-copy 'prim
	 1 (lambda (r a1)
	     (emit-code
	      "if (@1.vt != LV_STRING) {"
	      "WILE_EX(\"string-copy\", \"expects a string input\");"
	      "}"
	      "@@ = LVI_STRING(@1.v.str);"))
	 2 (lambda (r a1 a2)
	     (emit-code
	      "if (@1.vt != LV_STRING) {"
	      "WILE_EX(\"string-copy\", \"expects a string input\");"
	      "}"
	      "{"
	      "size_t len = strlen(@1.v.str);"
	      "if (@2.v.iv < 0 || (size_t) @2.v.iv >= len) {"
	      "WILE_EX(\"string-copy\", \"start index is out of range\");"
	      "}"
	      "@@ = LVI_STRING(@1.v.str + @2.v.iv);"
	      "}"))
	 3 (lambda (r a1 a2 a3)
	     (emit-code
	      "if (@1.vt != LV_STRING) {"
	      "WILE_EX(\"string-copy\", \"expects a string input\");"
	      "}"
	      "{"
	      "size_t len = strlen(@1.v.str);"
	      "if (@2.v.iv < 0 || (size_t) @2.v.iv >= len) {"
	      "WILE_EX(\"string-copy\", \"start index is out of range\");"
	      "}"
	      "if (@3.v.iv < @2.v.iv || (size_t) @3.v.iv >= len) {"
	      "WILE_EX(\"string-copy\", \"end index is out of range\");"
	      "}"
	      "@@.vt = LV_STRING;"
	      "@@.v.str = LISP_ALLOC(char, 1 + @3.v.iv - @2.v.iv);"
	      "LISP_ASSERT(@@.v.str != NULL);"
	      "memcpy(@@.v.str, @1.v.str + @2.v.iv, @3.v.iv - @2.v.iv);"
	      "@@.v.str[@3.v.iv - @2.v.iv] = '\\0';"
	      "}")))

   (list 'string-length "expects one string argument and returns its length"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_INT(strlen(@1.v.str));")))

   (list 'string-ref "expects one string and one integer index, and returns the character at that position in the string"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "if (@1.vt != LV_STRING) {"
	    "WILE_EX(\"string-ref\", \"expects a string input\");"
	    "}"
	    "if (@2.v.iv < 0 || (size_t) @2.v.iv >= strlen(@1.v.str)) {"
	    "WILE_EX(\"string-ref\", \"index is out of range\");"
	    "}"
	    "@@ = LVI_CHAR(@1.v.str[@2.v.iv]);")))

   (list 'string-find-first-char "expects one string and one character, and returns the index of the left-most occurrence of that character in the string, or #f if the character does not occur in the string"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "if (@1.vt != LV_STRING || @2.vt != LV_CHAR) {"
	    "WILE_EX(\"string-find-first-char\", \"expects a string and a character input\");"
	    "}"
	    "{"
	    "char* pos = strchr(@1.v.str, @2.v.chr);"
	    "if (pos) {"
	    "@@ = LVI_INT(pos - @1.v.str);"
	    "} else {"
	    "@@ = LVI_BOOL(false);"
	    "}"
	    "}")))

   (list 'string-find-last-char "expects one string and one character, and returns the index of the right-most occurrence of that character in the string, or #f if the character does not occur in the string"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "if (@1.vt != LV_STRING || @2.vt != LV_CHAR) {"
	    "WILE_EX(\"string-find-last-char\", \"expects a string and a character input\");"
	    "}"
	    "{"
	    "char* pos = strrchr(@1.v.str, @2.v.chr);"
	    "if (pos) {"
	    "@@ = LVI_INT(pos - @1.v.str);"
	    "} else {"
	    "@@ = LVI_BOOL(false);"
	    "}"
	    "}")))

   (list 'string-set! 'prim 3
	 (lambda (r a1 a2 a3)
	   (emit-code
	    "if (@1.vt != LV_STRING || @2.vt != LV_INT || @3.vt != LV_CHAR) {"
	    "WILE_EX(\"string-set!\", \"expects a string, an integer, and a character\");"
	    "}"
	    "if (@2.v.iv < 0 || (size_t) @2.v.iv >= strlen(@1.v.str)) {"
	    "WILE_EX(\"string-set!\", \"index is out of range\");"
	    "}"
	    "@1.v.str[@2.v.iv] = @3.v.chr;"
	    "@@ = @1;")))

   (list 'string-reverse "expects one string and returns the front-to-back reverse of it"
	 'prim 1 "wile_string_reverse")

   (list 'string-hash-32 "expects one string and returns the 32-bit FNV hash of it"
	 'prim 1 "wile_string_hash_32")
   (list 'string-ci-hash-32 "expects one string and returns the 32-bit FNV hash of the lowercase version of it"
	 'prim 1 "wile_string_ci_hash_32")
   (list 'string-hash-64 "expects one string and returns the 64-bit FNV hash of it"
	 'prim 1 "wile_string_hash_64")
   (list 'string-ci-hash-64 "expects one string and returns the 64-bit FNV hash of the lowercase version of it it"
	 'prim 1 "wile_string_ci_hash_64")

   (list 'char->integer "expects one character and returns its integer code equivalent"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_INT((unsigned char) @1.v.chr);")))

   (list 'integer->char "expects one integer and returns its character equivalent"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CHAR((unsigned char) @1.v.iv);")))

   (list 'char->string 'prim -1 "wile_char2string")

   (list 'symbol->string "expects one symbol argument and returns the text of it as a string"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_STRING(@1.v.str);")))

   (list 'string->symbol "expects one string argument and converts it into a symbol"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_SYMBOL(@1.v.str);")))

   (list 'symbol=? "expects two symbol arguments and returns #t if they are the same, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(strcmp(@1.v.str, @2.v.str) == 0);")))

   (list 'bits-and "expects two integers and returns their bitwise AND"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT(@1.v.iv & @2.v.iv);")))

   (list 'bits-or "expects two integers and returns their bitwise OR"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT(@1.v.iv | @2.v.iv);")))

   (list 'bits-xor "expects two integers and returns their bitwise XOR"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT(@1.v.iv ^ @2.v.iv);")))

   (list 'bits-not "expects one integer and returns its bitwise NOT"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_INT(~@1.v.iv);")))

   (list 'bits-shift 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@2.v.iv >= 0) ? (@1.v.iv << @2.v.iv) : (@1.v.iv >> -@2.v.iv));")))

   (list 'bits-get 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@2.v.iv >= 0) ? (@1.v.iv & ( 1 << @2.v.iv)) : 0);")))

   (list 'bits-set 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@2.v.iv >= 0) ? (@1.v.iv | (1 << @2.v.iv)) : @1.v.iv);")))

   (list 'bits-set? 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL((@2.v.iv >= 0) ? ((@1.v.iv & ( 1 << @2.v.iv)) != 0) : false);")))

   (list 'bits-clear 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@2.v.iv >= 0) ? (@1.v.iv & ~(1 << @2.v.iv)) : @1.v.iv);")))

   (list 'bits-flip 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_INT((@2.v.iv >= 0) ? (@1.v.iv ^ (1 << @2.v.iv)) : @1.v.iv);")))

   (list 'sleep "expects one real-valued number, which is used to delay or sleep the process for the given number of seconds"
	 'prim 1
	 (lambda (r a1)
	   (let ((a9 (new-svar)))
	     (promote/real+check "sleep" a9 a1)
	     (emit-code
	      "@@ = LVI_BOOL(usleep(1000000*@9.v.rv) == 0);"))))

   (list 'exit "expects one integer, the normal exit status, which is reported to the calling process; this call does not return"
	 'prim 1
	 (lambda (r a1) (emit-code "exit(@1.v.iv);")))

   (list 'emergency-exit "expects one integer, the emergency exit status, which is reported to the calling process; this call does not return"
	 'prim 1
	 (lambda (r a1) (emit-code "_exit(@1.v.iv);")))

   (list 'floor "expects one real-valued number and returns its floor. if the input is integer- or rational-typed, the output is integer-typed; otherwise, it is real"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "if (@1.vt == LV_REAL) {"
	    "@@ = LVI_REAL(FLOOR(@1.v.rv));"
	    "} else if (@1.vt == LV_RAT) {"
	    ;;; TODO: keep this as an integer division & return quotient?
	    "@@ = LVI_INT(FLOOR(LV_RAT2REAL(@1)));"
	    "} else if (@1.vt == LV_INT) {"
	    "@@ = LVI_INT(@1.v.iv);"
	    "} else {"
	    "WILE_EX(\"floor\", \"expects one real-valued argument\");"
	    "}")))

   (list 'ceiling "expects one real-valued number and returns its ceiling. if the input is integer- or rational-typed, the output is integer-typed; otherwise, it is real"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "if (@1.vt == LV_REAL) {"
	    "@@ = LVI_REAL(CEIL(@1.v.rv));"
	    "} else if (@1.vt == LV_RAT) {"
	    ;;; TODO: keep this as an integer division & return quotient?
	    "@@ = LVI_INT(CEIL(LV_RAT2REAL(@1)));"
	    "} else if (@1.vt == LV_INT) {"
	    "@@ = LVI_INT(@1.v.iv);"
	    "} else {"
	    "WILE_EX(\"ceiling\", \"expects one real-valued argument\");"
	    "}")))

   (list 'round 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "if (@1.vt == LV_REAL) {"
	    "@@ = LVI_REAL(FLOOR(0.5 + @1.v.rv));"
	    "} else if (@1.vt == LV_RAT) {"
	    ;;; TODO: keep this as an integer division & return quotient?
	    "@@ = LVI_INT(FLOOR(0.5 + LV_RAT2REAL(@1)));"
	    "} else if (@1.vt == LV_INT) {"
	    "@@ = LVI_INT(@1.v.iv);"
	    "} else {"
	    "WILE_EX(\"round\", \"expects one real-valued argument\");"
	    "}")))

   (list 'truncate 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_INT(@1.v.rv >= 0.0 ? FLOOR(@1.v.rv) : CEIL(@1.v.rv));")))

   (list 'sqrt "expects one number and returns its square root. positive real-valued inputs return a real-typed result, others return a complex-typed result"
	 'prim 1
	 (lambda (r a1)
	   (let ((a9 (new-svar)))
	     (promote/real a9 a1)
	     (emit-code
	      "if (@9.vt == LV_REAL) {"
	      "if (@9.v.rv < 0.0) {"
	      "@@ = LVI_CMPLX2(0.0, SQRT(-@9.v.rv));"
	      "} else {"
	      "@@ = LVI_REAL(SQRT(@9.v.rv));"
	      "}"
	      "} else if (@9.vt == LV_CMPLX) {"
	      "@@ = LVI_CMPLX1(CSQRT(@9.v.cv));"
	      "} else {"
	      "WILE_EX(\"sqrt\", \"expects one numeric argument\");"
	      "}"))))

   (list 'cbrt 'prim 1
	 (lambda (r a1)
	   (let ((a9 (new-svar)))
	     (promote/real a9 a1)
	     (emit-code
	      "if (@9.vt == LV_REAL) {"
	      "@@ = LVI_REAL(CBRT(@9.v.rv));"
	      "} else if (@9.vt == LV_CMPLX) {"
	      "@@ = LVI_CMPLX1(CPOW(@9.v.cv, 1.0/3.0));"
	      "} else {"
	      "WILE_EX(\"cbrt\", \"expects one numeric argument\");"
	      "}"))))

   (list 'exp 'prim 1
	 (lambda (r a1) (build-special-math-rc "exp" "EXP" "CEXP" r a1)))

   (list 'log 'prim 1
	 (lambda (r a1)
	   (let ((a9 (new-svar)))
	     (promote/real a9 a1)
	     (emit-code
	      "if (@9.vt == LV_REAL) {"
	      "if (@9.v.rv >= 0.0) {"
	      "@@ = LVI_REAL(LOG(@9.v.rv));"
	      "} else {"
	      "@@ = LVI_CMPLX2(LOG(-@9.v.rv), PI_L);"
	      "}"
	      "} else if (@9.vt == LV_CMPLX) {"
	      "@@ = LVI_CMPLX1(CLOG(@9.v.cv));"
	      "} else {"
	      "WILE_EX(\"log\", \"expects one numeric argument\");"
	      "}"))))

   (list 'sin "expects one number and returns the sine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "sin" "SIN" "CSIN" r a1)))

   (list 'cos "expects one number and returns the cosine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "cos" "COS" "CCOS" r a1)))

   (list 'tan "expects one number and returns the tangent of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "tan" "TAN" "CTAN" r a1)))

   (list 'sinh "expects one number and returns the hyperbolic sine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "sinh" "SINH" "CSINH" r a1)))

   (list 'cosh "expects one number and returns the hyperbolic cosine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "cosh" "COSH" "CCOSH" r a1)))

   (list 'tanh "expects one number and returns the hyperbolic tangent of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "tanh" "TANH" "CTANH" r a1)))

   (list 'asin "expects one number and returns the arc-sine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "asin" "ASIN" "CASIN" r a1)))

   (list 'acos "expects one number and returns the arc-cosine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "acos" "ACOS" "CACOS" r a1)))

   (list 'atan 'prim
	 1 (lambda (r a1) (build-special-math-rc "atan" "ATAN" "CATAN" r a1))
	 2 (lambda (r a1 a2)
	     (let ((a9 (new-svar))
		   (a8 (new-svar)))
	       (promote/real+check "atan" a9 a1)
	       (promote/real+check "atan" a8 a2)
	       (emit-code "@@ = LVI_REAL(ATAN2(@9.v.rv, @8.v.rv));"))))

   (list 'asinh "expects one number and returns the hyperbolic arc-sine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "asinh" "ASINH" "CASINH" r a1)))

   (list 'acosh "expects one number and returns the hyperbolic arc-cosine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "acosh" "ACOSH" "CACOSH" r a1)))

   (list 'atanh "expects one number and returns the hyperbolic arc-tangent of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'prim 1
	 (lambda (r a1) (build-special-math-rc "atanh" "ATANH" "CATANH" r a1)))

   (list 'erfc "expects one real-valued argument and returns the complementary error function of that value"
	 'prim 1
	 (lambda (r a1)
	   (let ((a9 (new-svar)))
	     (promote/real+check "erfc" a9 a1)
	     (emit-code "@@ = LVI_REAL(ERFC(@9.v.rv));"))))

   (list 'ldexp "expects a real-valued argument X and an integer N and returns X*2^N"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL(LDEXP(@1.v.rv, @2.v.iv));")))

   (list 'frexp "expects one real-valued argument and returns a 2-list which is decomposition of it into a normalized fraction in [0.5,1) and an exponent; 0, NaN, and Inf are special cases"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "lval vs[2];"
	    "int ex;"
	    "if (ISFINITE(@1.v.rv)) {"
	    "vs[0] = LVI_REAL(FREXP(@1.v.rv, &ex));"
	    "} else {"
	    "vs[0] = LVI_REAL(@1.v.rv);"
	    "ex = 0;"
	    "}"
	    "vs[1] = LVI_INT(ex);"
	    "@@ = gen_list(2, vs, NULL);"
	    "}")))

   (list 'fmod 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL(FMOD(@1.v.rv, @2.v.rv));")))

   (list 'hypot "expects two real-valued arguments and returns the square root of the sum of their squares"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a8 (new-svar))
		 (a9 (new-svar)))
	     (promote/real+check "hypot" a8 a1)
	     (promote/real+check "hypot" a9 a2)
	     (emit-code "@@ = LVI_REAL(HYPOT(@8.v.rv, @9.v.rv));"))))

   (list 'poly-chebyshev1 "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Chebyshev polynomial of the first kind at X"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar)))
	     (promote/real+check "poly-chebyshev1" a9 a2)
	     (emit-code "@@ = LVI_REAL(pcheby1(@1.v.iv, @9.v.rv));"))))

   (list 'poly-chebyshev2 "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Chebyshev polynomial of the second kind at X"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar)))
	     (promote/real+check "poly-chebyshev2" a9 a2)
	     (emit-code "@@ = LVI_REAL(pcheby2(@1.v.iv, @9.v.rv));"))))

   (list 'poly-hermite1 "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Hermite polynomial of the \"physicist\" flavor at X"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar)))
	     (promote/real+check "poly-hermite1" a9 a2)
	     (emit-code "@@ = LVI_REAL(phermite1(@1.v.iv, @9.v.rv));"))))

   (list 'poly-hermite2 "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Hermite polynomial of the \"probabilist\" flavor at X"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar)))
	     (promote/real+check "poly-hermite2" a9 a2)
	     (emit-code "@@ = LVI_REAL(phermite2(@1.v.iv, @9.v.rv));"))))

   (list 'poly-legendre "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Legendre polynomial at X"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar)))
	     (promote/real+check "poly-legendre" a9 a2)
	     (emit-code "@@ = LVI_REAL(plegendre(@1.v.iv, @9.v.rv));"))))

   (list 'poly-laguerre "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Laguerre polynomial at X"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar)))
	     (promote/real+check "poly-laguerre" a9 a2)
	     (emit-code "@@ = LVI_REAL(plaguerre(@1.v.iv, @9.v.rv));"))))

   (list 'bessel-j "expects one integer N and one real-valued argument X and returns the value of the Nth-order Bessel function of the first kind at X"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar)))
	     (promote/real+check "bessel-j" a9 a2)
	     (emit-code
	      "{"
	      "int s, n;"
	      "n = @1.v.iv;"
	      "if (n >= 0) {"
	      "s = 1;"
	      "} else {"
	      "n = -n;"
	      "s = (n%%2 == 0) ? 1 : -1;"
	      "}"
	      "@@ = LVI_REAL(s*JN(n, @9.v.rv));"
	      "}"))))

   (list 'bessel-y "expects one integer N and one positive real-valued argument X and returns the value of the Nth-order Bessel function of the second kind at X"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar)))
	     (promote/real+check "bessel-j" a9 a2)
	     (emit-code
	      "{"
	      "int s, n;"
	      "n = @1.v.iv;"
	      "if (n >= 0) {"
	      "s = 1;"
	      "} else {"
	      "n = -n;"
	      "s = (n%%2 == 0) ? 1 : -1;"
	      "}"
	      "@@ = LVI_REAL((@9.v.rv >= 0.0) ? s*YN(n, @9.v.rv) : REAL_NAN);"
	      "}"))))

   ;;; TODO: maybe stick this into a function, which already exists?
   ;;; This might be a bit too big to inline. On the other hand, it's
   ;;; not likely that there will be very many separate calls to AGM
   ;;; in a program.

   (list 'arithmetic-geometric-mean "expects to numeric values and returns their arithmetic-geometric mean"
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar))
		 (a8 (new-svar)))
	     (promote/real a9 a1)
	     (promote/real a8 a2)
	     (emit-code
	      "{"
	      "lisp_cmplx_t a, g, an;"
	      "if (@9.vt == LV_REAL) {"
	      "a = @9.v.rv;"
	      "} else if (@9.vt == LV_CMPLX) {"
	      "a = @9.v.cv;"
	      "} else {"
	      "WILE_EX(\"arithmetic-geometric-mean\", \"expects numeric arguments\");"
	      "}"
	      "if (@8.vt == LV_REAL) {"
	      "g = @8.v.rv;"
	      "} else if (@8.vt == LV_CMPLX) {"
	      "g = @8.v.cv;"
	      "} else {"
	      "WILE_EX(\"arithmetic-geometric-mean\", \"expects numeric arguments\");"
	      "}"
	      "while (CABS(a - g) > REAL_EPSILON*(CABS(a) + CABS(g))*0.5) {"
	      "an = (a + g)*0.5;"
	      "g = CSQRT(a*g);"
	      "a = an;"
	      "}"
	      "if (CIMAG(a) == 0.0) {"
	      "@@ = LVI_REAL(CREAL(a));"
	      "} else {"
	      "@@ = LVI_CMPLX1(a);"
	      "}"
	      "}"))))

   (list 'integer 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "if (@1.vt == LV_INT) {"
	    "@@ = @1;"
	    "} else if (@1.vt == LV_RAT) {"
	    "@@ = LVI_INT(@1.v.irv.num/@1.v.irv.den);"
	    "} else if (@1.vt == LV_REAL) {"
	    "@@ = LVI_INT((@1.v.rv >= 0.0) ? FLOOR(@1.v.rv) : CEIL(@1.v.rv));"
	    "} else {"
	    "WILE_EX(\"integer\", \"expects one real-valued argument\");"
	    "}")))

   (list 'float "expects one real-valued argument and returns it as a floating-point value"
	 'prim 1
	 (lambda (r a1) (promote/real+check "float" r a1)))

   (list 'expt
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = wile_expt(&@1, &@2);")))

   (list 'random-seed! "expects one optional integer argument, and resets the random number generator using that value as a seed. if no argument is given, a default value is generated from the current time and process id"
	 'prim
	 0 (lambda (r)
	     (emit-code
	      "srand48((time(NULL)) ^ (getpid() << 4));"
	      "@@ = LVI_BOOL(true);"))
	 1 (lambda (r a1)
	     (emit-code
	      "srand48(@1.v.iv);"
	      "@@ = LVI_BOOL(true);")))

   (list 'random-uniform 'prim
	 0 (lambda (r)
	     (emit-code "@@ = LVI_REAL(drand48());"))
	 2 (lambda (r a1 a2)
	     (let ((a9 (new-svar))
		   (a8 (new-svar)))
	       (promote/real+check "random-uniform" a9 a1)
	       (promote/real+check "random-uniform" a8 a2)
	       (emit-code
		"@@ = LVI_REAL(@9.v.rv + (@8.v.rv - @9.v.rv)*drand48());"))))

   (list 'random-exponential 'prim
	 0 (lambda (r)
	     (emit-code "@@ = LVI_REAL(-LOG(1.0 -drand48()));"))
	 ;;; TODO: need to check that rate parameter is positive
	 1 (lambda (r a1)
	     (let ((a9 (new-svar)))
	       (promote/real+check "random-exponential" a9 a1)
	       (emit-code "@@ = LVI_REAL(-LOG(1.0 - drand48())/@9.v.rv);"))))

   ;;; TODO: need to check for positive real lambda
   (list 'random-poisson "expects one positive real-valued argument and returns a Poisson-distributed random variable"
	 'prim 1
	 (lambda (r a1)
	   (let ((a9 (new-svar)))
	     (promote/real+check "random-poisson" a9 a1)
	     (emit-code
	      "{"
	      "lisp_int_t k = 0;"
	      "lisp_real_t l = EXP(-@9.v.rv), p = 1.0;"
	      "do {"
	      "++k;"
	      "p *= drand48();"
	      "} while (p > l);"
	      "@@ = LVI_INT(k - 1);"
	      "}"))))

   (list 'random-normal-pair 'prim
	 0 (lambda (r)
	     (emit-code
	      "@@ = wile_rand_normal_pair(0.0, 1.0);"))
	 2 (lambda (r a1 a2)
	     (let ((a9 (new-svar))
		   (a8 (new-svar)))
	       (promote/real+check "random-normal-pair" a9 a1)
	       (promote/real+check "random-normal-pair" a8 a2)
	       (emit-code
		"@@ = wile_rand_normal_pair(@9.v.rv, @8.v.rv);"))))

   ;;; TODO: need to check for negative n
   (list 'factorial "expects one non-negative integer argument and returns the factorial of the input"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "lisp_int_t i, f = 1;"
	    "for (i = 1; i <= @1.v.iv; ++i) {"
	    "f *= i;"
	    "}"
	    "@@ = LVI_INT(f);"
	    "}")))

   (list 'floor-quotient 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lisp_int_t nq, nr;"
	    "floor_qr(@1.v.iv, @2.v.iv, &nq, &nr);"
	    "@@ = LVI_INT(nq);"
	    "}")))

   (list 'floor-remainder 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lisp_int_t nq, nr;"
	    "floor_qr(@1.v.iv, @2.v.iv, &nq, &nr);"
	    "@@ = LVI_INT(nr);"
	    "}")))

   (list 'truncate-quotient 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lisp_int_t nq, nr;"
	    "trunc_qr(@1.v.iv, @2.v.iv, &nq, &nr);"
	    "@@ = LVI_INT(nq);"
	    "}")))

   (list 'truncate-remainder 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lisp_int_t nq, nr;"
	    "trunc_qr(@1.v.iv, @2.v.iv, &nq, &nr);"
	    "@@ = LVI_INT(nr);"
	    "}")))

   (list 'ceiling-quotient 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lisp_int_t nq, nr;"
	    "ceil_qr(@1.v.iv, @2.v.iv, &nq, &nr);"
	    "@@ = LVI_INT(nq);"
	    "}")))

   (list 'ceiling-remainder 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lisp_int_t nq, nr;"
	    "ceil_qr(@1.v.iv, @2.v.iv, &nq, &nr);"
	    "@@ = LVI_INT(nr);"
	    "}")))

   (list 'floor/ 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lval vs[2];"
	    "lisp_int_t nq, nr;"
	    "floor_qr(@1.v.iv, @2.v.iv, &nq, &nr);"
	    "vs[0] = LVI_INT(nq);"
	    "vs[1] = LVI_INT(nr);"
	    "@@ = gen_list(2, vs, NULL);"
	    "}")))

   (list 'truncate/ 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lval vs[2];"
	    "lisp_int_t nq, nr;"
	    "trunc_qr(@1.v.iv, @2.v.iv, &nq, &nr);"
	    "vs[0] = LVI_INT(nq);"
	    "vs[1] = LVI_INT(nr);"
	    "@@ = gen_list(2, vs, NULL);"
	    "}")))

   (list 'ceiling/ 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "lval vs[2];"
	    "lisp_int_t nq, nr;"
	    "ceil_qr(@1.v.iv, @2.v.iv, &nq, &nr);"
	    "vs[0] = LVI_INT(nq);"
	    "vs[1] = LVI_INT(nr);"
	    "@@ = gen_list(2, vs, NULL);"
	    "}")))

   ;;; A few internal type-check and -conversion functions,
   ;;; to help with implementing numeric tower

   (list '_int? "expects one value and returns #t if the type of that value is integer, #f otherwise; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_INT);")))

   (list '_rat? "expects one value and returns #t if the type of that value is rational, #f otherwise; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_RAT);")))

   (list '_real? "expects one value and returns #t if the type of that value is real, #f otherwise; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_REAL);")))

   (list '_cmplx? "expects one value and returns #t if the type of that value is complex, #f otherwise; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_CMPLX);")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO: do we need these functions? from here...
;;; we need them for test-wile.scm...?

   (list '_int->rat_ "expects one integer-typed value and returns the value converted to type rational; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_RAT(@1.v.iv, 1);")))

   (list '_int->real_ "expects one integer-typed value and returns the value converted to type real; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_REAL((lisp_real_t) @1.v.iv);")))

   (list '_int->cmplx_ "expects one integer-typed value and returns the value converted to type complex; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CMPLX2((lisp_real_t) @1.v.iv, 0.0);")))

   (list '_rat->real_ "expects one rational-typed value and returns the value converted to type real; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_REAL(((lisp_real_t) @1.v.irv.num)/((lisp_real_t) @1.v.irv.den));")))

   (list '_rat->cmplx_ "expects one rational-typed value and returns the value converted to type complex; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CMPLX2(((lisp_real_t) @1.v.irv.num)/((lisp_real_t) @1.v.irv.den), 0.0);")))

   (list '_real->cmplx_ "expects one real-typed value and returns the value converted to type complex; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CMPLX2(@1.v.rv, 0.0);")))

;;; ... to here?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (list 'is-regular-file? "expects one string and returns #t if that is the name of a regular file, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "struct stat sb;"
	    "@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISREG(sb.st_mode));"
	    "}")))

   (list 'is-directory? "expects one string and returns #t if that is the name of a directory, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "struct stat sb;"
	    "@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISDIR(sb.st_mode));"
	    "}")))

   (list 'is-char-device? "expects one string and returns #t if that is the name of a character device, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "struct stat sb;"
	    "@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISCHR(sb.st_mode));"
	    "}")))

   (list 'is-block-device? "expects one string and returns #t if that is the name of a block device, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "struct stat sb;"
	    "@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISBLK(sb.st_mode));"
	    "}")))

   (list 'is-named-pipe? "expects one string and returns #t if that is the name of a named pipe, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "struct stat sb;"
	    "@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISFIFO(sb.st_mode));"
	    "}")))

   (list 'is-symbolic-link? "expects one string and returns #t if that is the name of a symbolic link, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "struct stat sb;"
	    "@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISLNK(sb.st_mode));"
	    "}")))

   (list 'is-socket? "expects one string and returns #t if that is the name of a socket, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "struct stat sb;"
	    "@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISSOCK(sb.st_mode));"
	    "}")))

   (list 'file-exists? "expects one string and returns #t if that is the name of an existing file, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(access(@1.v.str, F_OK) == 0);")))

   (list 'file-readable? "expects one string and returns #t if that is the name of an existing and readable file, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(access(@1.v.str, R_OK) == 0);")))

   (list 'file-writable? "expects one string and returns #t if that is the name of an existing and writable file, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(access(@1.v.str, W_OK) == 0);")))

   (list 'file-executable?  "expects one string and returns #t if that is the name of an existing and executable file, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(access(@1.v.str, X_OK) == 0);")))

   (list 'create-directory "expects a string and optionally an integer, and creates a directory with that name; the integer is the mode, which defaults to octal 0755 if not specified"
	 'prim
	 1 (lambda (r a1)
	     (emit-code
	      "@@ = LVI_BOOL(mkdir(@1.v.str, 0755) == 0);"))
	 2 (lambda (r a1 a2)
	     (emit-code
	      "@@ = LVI_BOOL(mkdir(@1.v.str, @2.v.iv) == 0);")))

   (list 'rename-file "expects two strings: the first is the name of an existing file, and the second is the new name to which it will be renamed"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_BOOL(rename(@1.v.str, @2.v.str) == 0);")))

   (list 'remove-file "expects one string, and removes (using unlink) a file of that name"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_BOOL(unlink(@1.v.str) == 0);")))

   (list 'remove-directory "expects one string, and removes (using rmdir) a directory of that name"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_BOOL(rmdir(@1.v.str) == 0);")))

   (list 'change-root-directory 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_BOOL(chroot(@1.v.str) == 0);")))

   (list 'describe-system-error "expects one integer, a system error code, and returns the corresponding description using strerror"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_STRING(strerror(@1.v.iv));")))

   (list 'unset-environment-variable "expects one string which is the name of an environment variable to be removed from the process environment"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_BOOL(unsetenv(@1.v.str) == 0);")))

   (list 'set-environment-variable "expects two strings which are the name of an environment variable and the value to which it should be set"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_BOOL(setenv(@1.v.str, @2.v.str, 1) == 0);")))

   (list 'get-environment-variable "expects one string which is the name of the environment variable to retrieve, and returns the value to which that environment variable is set, or #f if that environment variable does not exist"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "char* ev = getenv(@1.v.str);"
	    "@@ = (ev ? LVI_STRING(ev) : LVI_BOOL(false));"
	    "}")))

   (list 'send-signal "expects two integers which are a process id and a signal number respectively, and sends the specified signal to the specified process. returns #t if the call succeeded, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_BOOL(kill(@1.v.iv, @2.v.iv) == 0);")))

   (list 'truncate-file 'prim
	 1 (lambda (r a1)
	     (emit-code
	      "{"
	      "if (@1.vt != LV_FILE_PORT) {"
	      "WILE_EX(\"truncate-file\", \"expects a file port\");"
	      "}"
	      "long int pos = ftell(@1.v.fp);"
	      "int fd = fileno(@1.v.fp);"
	      "@@ = LVI_BOOL(pos >= 0 && fd >= 0 && ftruncate(fd, pos) == 0);"
	      "}"))
	 2 (lambda (r a1 a2)
	     (emit-code
	      "{"
	      "if ((@1.vt != LV_FILE_PORT && @1.vt != LV_STRING) || @2.vt != LV_INT) {"
	      "WILE_EX(\"truncate-file\", \"expects a file name or port and an integer\");"
	      "}"
	      "if (@1.vt == LV_FILE_PORT) {"
	      "int fd = fileno(@1.v.fp);"
	      "@@ = LVI_BOOL(@2.v.iv >= 0 && fd >= 0 && ftruncate(fd, @2.v.iv) == 0);"
	      "} else {"
	      "@@ = LVI_BOOL(@2.v.iv >= 0 && truncate(@1.v.str, @2.v.iv) == 0);"
	      "}"
	      "}")))

   (list 'cputime "returns a list containing two floating-point numbers which are the process user and system times, respectively, in seconds" 'prim 0 "wile_cputime")

   (list 'vector-create "expects one integer, the size of the vector to be created, and optionally a second argument which is used to fill all slots of the new vector; returns a new vector of the given size"
	 'prim
	 1 (lambda (r a1)
	     (emit-code
	      "{"
	      "size_t i, capa;"
	      "@@.vt = LV_VECTOR;"
	      "capa = @1.v.iv;"
	      "@@.v.vec.capa = capa;"
	      "@@.v.vec.arr = LISP_ALLOC(lptr, (capa > 0 ? capa : 1));"
	      "if (@@.v.vec.arr == NULL) {"
	      "WILE_EX(\"vector-create\", \"memory allocation failed!\");"
	      "}"
	      "for (i = 0; i < capa; ++i) {"
	      "@@.v.vec.arr[i] = NULL;"
	      "}"
	      "}"))
	 2 (lambda (r a1 a2)
	     (emit-code
	      "{"
	      "size_t i, capa;"
	      "@@.vt = LV_VECTOR;"
	      "capa = @1.v.iv;"
	      "@@.v.vec.capa = capa;"
	      "@@.v.vec.arr = LISP_ALLOC(lptr, (capa > 0 ? capa : 1));"
	      "if (@@.v.vec.arr == NULL) {"
	      "WILE_EX(\"vector-create\", \"memory allocation failed!\");"
	      "}"
	      "@@.v.vec.arr[0] = new_lv(LV_NIL);"
	      "*(@@.v.vec.arr[0]) = @2;"
	      "for (i = 1; i < capa; ++i) {"
	      "@@.v.vec.arr[i] = @@.v.vec.arr[0];"
	      "}"
	      "}")))

   (list 'vector-fill! 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "size_t i, capa;"
	    "if (@1.vt != LV_VECTOR) {"
	    "WILE_EX(\"vector-fill!\", \"first input is not a vector\");"
	    "}"
	    "capa = @1.v.vec.capa;"
	    "lptr pv = new_lv(LV_NIL);"
	    "*pv = @2;"
	    "for (i = 0; i < capa; ++i) {"
	    "@1.v.vec.arr[i] = pv;"
	    "}"
	    "@@ = @1;"
	    "}")))

   (list 'vector-length "expects one vector and returns its length"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_VECTOR) {"
	    "WILE_EX(\"vector-length\", \"input is not a vector\");"
	    "}"
	    "@@ = LVI_INT(@1.v.vec.capa);"
	    "}")))

   (list 'vector-ref "expects one vector and one index, bounds-checks the index, and returns the value stored in the vector at that index"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_VECTOR) {"
	    "WILE_EX(\"vector-ref\", \"input is not a vector\");"
	    "}"
	    "if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.vec.capa) {"
	    "WILE_EX(\"vector-ref\", \"got bad index value\");"
	    "}"
	    "@@ = @1.v.vec.arr[@2.v.iv] ? *(@1.v.vec.arr[@2.v.iv]) : LVI_NIL();"
	    "}")))

   (list 'vector-set! "expects a vector, an index, and a value, bounds-checks the index, and saves the value in the vector at that index; modifies the vector in-place"
	 'prim 3
	 (lambda (r a1 a2 a3)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_VECTOR) {"
	    "WILE_EX(\"vector-set!\", \"input is not a vector\");"
	    "}"
	    "if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.vec.capa) {"
	    "WILE_EX(\"vector-set!\", \"got bad index value\");"
	    "}"
	    "@1.v.vec.arr[@2.v.iv] = new_lv(LV_NIL);"
	    "*(@1.v.vec.arr[@2.v.iv]) = @3;"
	    "@@ = @1;"
	    "}")))

   (list 'vector-swap! "expects one vector and two indices, bounds-checks the indices, and swaps the values stored in the vector at those two indices"
	 'prim 3
	 (lambda (r a1 a2 a3)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_VECTOR) {"
	    "WILE_EX(\"vector-swap!\", \"input is not a vector\");"
	    "}"
	    "if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.vec.capa || @3.vt != LV_INT || @3.v.iv < 0 || (size_t) @3.v.iv >= @1.v.vec.capa) {"
	    "WILE_EX(\"vector-swap!\", \"got bad index value\");"
	    "}"
	    "lptr tmp = @1.v.vec.arr[@2.v.iv];"
	    "@1.v.vec.arr[@2.v.iv] = @1.v.vec.arr[@3.v.iv];"
	    "@1.v.vec.arr[@3.v.iv] = tmp;"
	    "@@ = @1;"
	    "}")))

   (list 'bytevector-create "expects one integer, the size of the bytevector to be created, and optionally a second argument, a char or small integer, which is used to fill all slots of the new bytevector; returns a new bytevector of the given size"
	 'prim
	 1 (lambda (r a1)
	     (emit-code
	      "{"
	      "size_t i, capa;"
	      "@@.vt = LV_BVECTOR;"
	      "capa = @1.v.iv;"
	      "@@.v.bvec.capa = capa;"
	      "@@.v.bvec.arr = LISP_ALLOC(unsigned char, (capa > 0 ? capa : 1));"
	      "LISP_ASSERT(@@.v.bvec.arr != NULL);"
	      "for (i = 0; i < capa; ++i) {"
	      "@@.v.bvec.arr[i] = 0;"
	      "}"
	      "}"))
	 2 (lambda (r a1 a2)
	     (emit-code
	      "{"
	      "size_t i, capa;"
	      "@@.vt = LV_BVECTOR;"
	      "capa = @1.v.iv;"
	      "@@.v.bvec.capa = capa;"
	      "@@.v.bvec.arr = LISP_ALLOC(unsigned char, (capa > 0 ? capa : 1));"
	      "LISP_ASSERT(@@.v.bvec.arr != NULL);"
	      "if (@2.vt == LV_CHAR) {"
	      "@@.v.bvec.arr[0] = @2.v.chr;"
	      "} else if (@2.vt == LV_INT && @2.v.iv >= 0 && @2.v.iv < 256) {"
	      "@@.v.bvec.arr[0] = @2.v.iv & 0xff;"
	      "} else {"
	      "WILE_EX(\"bytevector-create\", \"got bad initializer\");"
	      "}"
	      "for (i = 1; i < capa; ++i) {"
	      "@@.v.bvec.arr[i] = @@.v.bvec.arr[0];"
	      "}"
	      "}")))

   (list 'bytevector-length "expects one bytevector and returns its length"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_BVECTOR) {"
	    "WILE_EX(\"bytevector-length\", \"input is not a bytevector\");"
	    "}"
	    "@@ = LVI_INT(@1.v.bvec.capa);"
	    "}")))

   (list 'bytevector-ref "expects one bytevector and one index, bounds-checks the index, and returns the value stored in the bytevector at that index"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_BVECTOR) {"
	    "WILE_EX(\"bytevector-ref\", \"input is not a bytevector\");"
	    "}"
	    "if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.bvec.capa) {"
	    "WILE_EX(\"bytevector-ref\", \"got bad index value\");"
	    "}"
	    "@@ = LVI_CHAR(@1.v.bvec.arr[@2.v.iv]);"
	    "}")))

   (list 'bytevector-set! "expects a bytevector, an index, and a char or small integer, bounds-checks the index, and saves the char/int in the bytevector at that index; modifies the bytevector in-place"
	 'prim 3
	 (lambda (r a1 a2 a3)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_BVECTOR) {"
	    "WILE_EX(\"bytevector-set!\", \"input is not a bytevector\");"
	    "}"
	    "if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.bvec.capa) {"
	    "WILE_EX(\"bytevector-set!\", \"got bad index value\");"
	    "}"
	    "if (!(@3.vt == LV_CHAR || (@3.vt == LV_INT && @3.v.iv >= 0 && @3.v.iv < 256))) {"
	    "WILE_EX(\"bytevector-set!\", \"got bad input value\");"
	    "}"
	    "@1.v.bvec.arr[@2.v.iv] = (@3.vt == LV_CHAR) ? @3.v.chr : @3.v.iv;"
	    "@@ = @1;"
	    "}")))

   (list 'bytevector-swap! "expects one bytevector and two indices, bounds-checks the indices, and swaps the values stored in the bytevector at those two indices"
	 'prim 3
	 (lambda (r a1 a2 a3)
	   (emit-code
	    "{"
	    "if (@1.vt != LV_BVECTOR) {"
	    "WILE_EX(\"bytevector-swap!\", \"input is not a bytevector\");"
	    "}"
	    "if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.bvec.capa ||"
	    "    @3.vt != LV_INT || @3.v.iv < 0 || (size_t) @3.v.iv >= @1.v.bvec.capa) {"
	    "WILE_EX(\"bytevector-swap!\", \"got bad index value\");"
	    "}"
	    "unsigned char tmp = @1.v.bvec.arr[@2.v.iv];"
	    "@1.v.bvec.arr[@2.v.iv] = @1.v.bvec.arr[@3.v.iv];"
	    "@1.v.bvec.arr[@3.v.iv] = tmp;"
	    "@@ = @1;"
	    "}")))

   (list 'bytevector->string 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "if (@1.vt != LV_BVECTOR) {"
	    "WILE_EX(\"bytevector->string\", \"expects one bytevector argument\");"
	    "}"
	    "@@.vt = LV_STRING;"
	    "@@.v.str = LISP_ALLOC(char, 1 + @1.v.bvec.capa);"
	    "LISP_ASSERT(@@.v.str != NULL);"
	    "memcpy(@@.v.str, @1.v.bvec.arr, @1.v.bvec.capa);"
	    "@@.v.str[@1.v.bvec.capa] = 0;")))

   (list 'UTCtime "returns a 9-element list (Y M D h m s dow doy dst?) corresponding to UTC time 'now'" 'prim 0 "wile_gmtime" 1 "wile_gmtime")
   (list 'localtime "returns a 9-element list (Y M D h m s dow doy dst?) corresponding to local time 'now'" 'prim 0 "wile_localtime" 1 "wile_localtime")
   (list 'get-file-status "expects one string, the name of an existing file, and returns a 13-element list of integers as returned by stat()"
	 'prim 1 "wile_filestat")
   (list 'get-symbolic-link-status "expects one string, the name of an existing file or symbolic link, and returns a 13-element list of integers as returned by lstat()"
	 'prim 1 "wile_symlinkstat")
   (list 'parse-string 'prim 1 "parse_string")
   (list 'parse-file 'prim 1 "parse_file")
   (list 'regex-match 'prim 2 "wile_regex_match")
   ;;; TODO: 0-arg version that returns all users
   (list 'get-user-information 'prim 1 "wile_getuserinfo")
   (list 'read-directory "expects a string argument which is the name of some directory (default if no argument is \".\" which is the current working directory) and returns a list of directory entries; each entry is a list of length 2 containing the name and inode for each directory entry" 'prim 0 "read_directory" 1 "read_directory")
   (list 'listen-on 'prim 1 "wile_listen_port")
   (list 'accept 'prim 1 "wile_accept_connection")
   (list 'connect-to 'prim 2 "wile_connect_to")

   (list 'raise 'prim -1
	 (lambda (r . as)
	   (apply build-basic-list r as)
	   (let ((a1 r)
		 (r #f))
	     (emit-code
	      "if (@1.vt == LV_PAIR && (@1.v.pair.cdr == NULL || @1.v.pair.cdr->vt == LV_NIL)) {"
	      "@1 = (@1.v.pair.car ? *(@1.v.pair.car) : LVI_NIL());"
	      "}"
	      "cachalot->errval = new_lv(LV_NIL);"
	      "*(cachalot->errval) = @1;"
	      "cachalot->l_whence = 0;"
	      "cachalot->c_whence = LISP_WHENCE;"
	      "longjmp(cachalot->cenv, 1);"))))

   (list 'log-gamma "expects one complex-valued argument and returns the complex-valued log of the gamma function of that argument"
	 'prim 1
	 (lambda (r a1)
	   (build-special-math-cmplx "log-gamma" "logamma" r a1)))

   (list 'digamma "expects one complex-valued argument and returns the complex-valued logarithmic derivative of the gamma function of that argument"
	 'prim 1
	 (lambda (r a1)
	   (build-special-math-cmplx "digamma" "digamma" r a1)))

   (list 'elliptic-K "expects one complex-valued argument and returns the complex-valued complete elliptic integral K of that argument"
	 'prim 1
	 (lambda (r a1)
	   (build-special-math-cmplx "elliptic-K" "elliptic_k" r a1)))

   (list 'elliptic-E "expects one complex-valued argument and returns the complex-valued complete elliptic integral E of that argument"
	 'prim 1
	 (lambda (r a1)
	   (build-special-math-cmplx "elliptic-E" "elliptic_e" r a1)))

   (list 'cosine-integral "expects one positive real-valued argument and returns the cosine integral of that value" 'prim 1
	 (lambda (r a1)
	   (build-special-math-real "cosine-integral" "cosine_integral" r a1)))

   (list 'sine-integral "expects one real-valued argument and returns the sine integral of that value" 'prim 1
	 (lambda (r a1)
	   (build-special-math-real "sine-integral" "sine_integral" r a1)))

   (list 'lambert-W+ "expects one real-valued argument > -1/e and returns the positive real branch of the Lambert W function of that argument"
	 'prim 1
	 (lambda (r a1)
	   (build-special-math-real "lambert-W+" "lambert_wp_fn" r a1)))

   (list 'lambert-W- "expects one real-valued argument in (-1/e, 0) and returns the negative real branch of the Lambert W function of that argument"
	 'prim 1
	 (lambda (r a1)
	   (build-special-math-real "lambert-W-" "lambert_wn_fn" r a1)))

   (list 'lambert-W
	 'prim 2
	 (lambda (r a1 a2)
	   (let ((a9 (new-svar)))
	     (promote/real a9 a2)
	     (emit-code
	      "{"
	      "if (@1.vt != LV_INT) {"
	      "WILE_EX(\"lambert-W\", \"expects an integer and a complex number\");"
	      "}"
	      "lisp_cmplx_t z;"
	      "if (@9.vt == LV_REAL) {"
	      "z = lambert_wc_fn(@1.v.iv, @9.v.rv);"
	      "} else if (@9.vt == LV_CMPLX) {"
	      "z = lambert_wc_fn(@1.v.iv, @9.v.cv);"
	      "} else {"
	      "WILE_EX(\"lambert-W\", \"expects an integer and a complex number\");"
	      "}"
	      "if (CIMAG(z) == 0.0) {"
	      "@@ = LVI_REAL(CREAL(z));"
	      "} else {"
	      "@@ = LVI_CMPLX1(z);"
	      "}"
	      "}"))))

   (list 'sqlite-version "returns the version of sqlite against which the program is linked" 'prim 0 "wile_sql_version")

   (list 'sqlite-open 'prim
	 0 (lambda (r)
	     (emit-code "@@ = wile_sql_open(NULL, 1, __FILE__, __LINE__);"))
	 1 (lambda (r a1)
	     (emit-code
	      "if (@1.vt == LV_STRING) {"
	      "@@ = wile_sql_open(@1.v.str, 0, __FILE__, __LINE__);"
	      "} else {"
	      "WILE_EX(\"sqlite-open\", \"expects a filename\");"
	      "}"))
	 2 (lambda (r a1 a2)
	     (emit-code
	      "if (@1.vt == LV_STRING && @2.vt == LV_SYMBOL) {"
	      "int mode;"
	      "if (strcmp(@2.v.str, \"read-only\") == 0) {"
	      "mode = 0;"
	      "} else if (strcmp(@2.v.str, \"read-write\") == 0) {"
	      "mode = 1;"
	      "} else if (strcmp(@2.v.str, \"create\") == 0) {"
	      "mode = 2;"
	      "} else {"
	      "WILE_EX(\"sqlite-open\", \"unknown mode %%s\", @2.v.str);"
	      "}"
	      "@@ = wile_sql_open(@1.v.str, mode, __FILE__, __LINE__);"
	      "} else {"
	      "WILE_EX(\"sqlite-open\", \"expects a filename\");"
	      "}")))

   (list 'sqlite-run "expects an sqlite port and a string, and runs the string as a command. warning! do not use this with user-supplied strings, this is a security hole"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "#ifdef WILE_USES_SQLITE"
	    "if (@1.vt == LV_SQLITE_PORT && @2.vt == LV_STRING) {"
	    "@@ = wile_sql_run(@1.v.sqlite_conn, @2.v.str, __FILE__, __LINE__);"
	    "} else {"
	    "WILE_EX(\"sqlite-run\", \"expects one sqlite-port and one string\");"
	    "}"
	    "#else"
	    "@@ = LVI_BOOL(false);"
	    "#endif // WILE_USES_SQLITE")))
 
   (list 'sqlite-statement-cleanup 'prim 1 "wile_sql_stmt_clean")
   (list 'sqlite-statement-info 'prim 1 "wile_sql_stmt_info")
   (list 'sqlite-statement-prepare 'prim 2 "wile_sql_stmt_prep")
   (list 'sqlite-statement-run 'prim 1 "wile_sql_stmt_run")
   (list 'sqlite-statement-bind 'prim -2 "wile_sql_stmt_bind")

   (list 'cfft-good-n?
	 "expects one integer input, returns #t if that integer is a good size for a vector to be transformed by the vector-cfft! routine, #f otherwise. 'good size' means a number which is a power of only the prime factors (2,3,5,7,11)"
	 'prim 1 "wile_cfft_good_n")

   (list 'vector-cfft! "expects one vector of a \"good\" length (see cfft-good-n?)  containing only numeric values, and computes the Fourier transform of that sequence in-place"
	 'prim 2 "wile_cfft")

   (list 'call/cc "expects one argument which is a procedure of one argument, and returns... well... it's complicated"
	 'prim 1 "wile_call_cc")

   (list 'wile-basic-build-info "expects no arguments and returns an integer which encodes various build flags"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(wile_binfo());")))

   (list 'stack-trace-minimal "expects one optional output file port to which the stack trace is written; the default is stderr. returns nothing useful"
	 'prim
	 0 (lambda (r)
	     (emit-code
	      "wile_stack_trace_minimal(fileno(stderr));"
	      "@@ = LVI_NIL();"))
	 1 (lambda (r a1)
	     (emit-code
	      "wile_stack_trace_minimal(fileno((@1.vt == LV_FILE_PORT) ? @1.v.fp : stderr));"
	      "@@ = LVI_NIL();")))

   (list 'display-object-hook "expects one symbol and one procedure of two arguments and records that procedure as the display method for objects of that type. this allows displaying objects with cycles"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "if (@1.vt == LV_SYMBOL && @2.vt == LV_LAMBDA && @2.v.lambda.arity == 2) {"
	    "@@ = register_display_proc(@1.v.str, @2, __FILE__, __LINE__);"
	    "} else {"
	    "WILE_EX(\"display-object-hook\", \"expects one symbol and one procedure of two arguments\");"
	    "}")))

   (list 'get-errno "expects no arguments and returns the current value of errno"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(errno);")))

   (list 'set-errno! "expects one integer argument and sets errno to that value. returns nothing useful"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "if (@1.vt == LV_INT) {"
	    "errno = @1.v.iv;"
	    "@@ = LVI_BOOL(true);"
	    "} else {"
	    "WILE_EX(\"set-errno!\", \"expects one integer\");"
	    "}")))

   (list 'token-source-line "expects one argument and returns its location in source code"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_STRING(decode_line_loc(get_lisp_loc(&@1)));")))

;;; ################ TODO: implement this stub!!!

   (list 'eval
	 'prim -1
	 (lambda (r . as)
	   (fprintf stderr "WARNING! using stub for eval\n")
	   (emit-code "@@ = LVI_STRING(\"TODO implement eval\");")))

   ))

;;; Add the stuff in wile-rtl2.scm to the primitives list

(define prim-table
  (let* ((wld (get-environment-variable "WILE_LINK_DIRECTORIES"))
	 (wlds (if wld (string-split-by is-colon? wld) (list ".")))
	 (found #f))
    (for-each (lambda (d)
		(unless found
		  (let ((f (string-append d "/wrtl.sch")))
		    (when (file-exists? f)
		      (set! found (read-all f))))))
	      wlds)
    (if found
	(append found prim-table-internal)
	(begin
	  (fprintf stderr "cannot find interface file 'wrtl.sch' in search path, skipping\n    %s\n" wld)
	  prim-table-internal))))

;;; (define-as "wile_day_of_week" (day-of-week v . vs)

(define (show-prims-table)
  (for-each (lambda (pe)
	      (write-string (string-pad-right (symbol->string (car pe))
					      #\space 30))
	      ;;; TODO: this is kind of a hack for dealing with doc-strings
	      ;;; do this better somehow
	      (when (string? (cadr pe))
		(set! pe (cdr pe)))
	      (let ((lp (length pe)))
		(cond ((< lp 3)
		       (write-string "WHAT?!?" #\tab)
		       (display (cdr pe))
		       (newline))
		      ((= 3 lp)
		       (write-string (symbol->string (cadr pe)) #\tab
				     (symbol->string (caddr pe)) #\newline))
		      ((= 4 lp)
		       (write-string (symbol->string (cadr pe)) #\tab
				     (number->string (caddr pe)) #\newline))
		      ((even? lp)
		       (write-string (symbol->string (cadr pe)))
		       (let loop ((p (cddr pe))
				  (s #\tab))
			 (if (null? p)
			     (newline)
			     (begin
			       (write-string s (number->string (car p)))
			       (loop (cddr p) #\space)))))
		      (else (write-string "TODO!\n")))))
	    (list-sort (lambda (a b)
			 (string<? (symbol->string (car a))
				   (symbol->string (car b))))
		       prim-table)))

(define (lookup-doc-string p)
  (let loop ((ps prim-table))
    (if (null? ps)
	"unknown!"
	(let ((entry (car ps)))
	  (if (symbol=? p (car entry))
	      (cond ((string? (cadr entry))
		     (cadr entry))
		    ((symbol=? (cadr entry) 'alias)
		     (lookup-doc-string (caddr entry)))
		    (else "undocumented!"))
	      (loop (cdr ps)))))))

(define (show-undoc)
  (for-each (lambda (entry)
	      (unless (or (string? (cadr entry))
			  (symbol=? (cadr entry) 'alias))
		(write-string (symbol->string (car entry)) #\newline)))
	    prim-table))
