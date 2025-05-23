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
	    (let ((tmp (new-svar)))
	      (emit-fstr "{\nlval %s[%d];\n" tmp ac)
	      (for-each (lambda (i v)
			  (emit-fstr "%s[%d] = %s;\n" tmp (- i 1) v))
			(fromto 1 ac) as)
	      (emit-fstr "%s = wile_gen_list(%d, %s, NULL);\n}\n" r ac tmp)))
	r)))

;;; used here for apply and in wile-comp.scm

(define (compile-runtime-apply r aL . as)
  (apply build-basic-list r as)
  (emit-fstr "%s = wile_apply_function(&(%s), \"%s\");\n" r r aL)
  r)

;;; Promote a number to be at least real type; checking for complex
;;; or non-number is the caller's responsibility

(define (promote/real r a1)
  (emit-code
   #<< HEREDOC
   >if (@1.vt == LV_INT) {
   >@@ = LVI_REAL((lisp_real_t) @1.v.iv);
   >} else if (@1.vt == LV_RAT) {
   >@@ = LVI_REAL(LV_RAT2REAL(@1));
   >} else {
   >@@ = @1;
   >}
   >HEREDOC
   ))

;;; Promote a number to real type, failing if the input is not real-valued

;;; In several of these functions, we could also rename the arguments:
;;; in promote/real+check, change s-name directly to aa, etc.

(define (promote/real+check aA r aL a1)
  (emit-code
   #<< HEREDOC
   >if (@1.vt == LV_INT) {
   >@@ = LVI_REAL((lisp_real_t) @1.v.iv);
   >} else if (@1.vt == LV_RAT) {
   >@@ = LVI_REAL(LV_RAT2REAL(@1));
   >} else if (@1.vt == LV_REAL) {
   >@@ = @1;
   >} else {
   >wile_exception("@A", "@L", "expects a real-valued input");
   >}
   >HEREDOC
   ))

(define (build-special-math-real s-name c-name r aL a1)
  (let ((aa c-name)
	(ab s-name))
    (emit-code
     #<< HEREDOC
     >{
     >lisp_real_t r;
     >switch (@1.vt) {
     >case LV_INT:
     >r = @1.v.iv;
     >break;
     >case LV_RAT:
     >r = LV_RAT2REAL(@1);
     >break;
     >case LV_REAL:
     >r = @1.v.rv;
     >break;
     >case LV_CMPLX:
     >wile_exception("@b", "@L", "is not implemented for complex values");
     >default:
     >wile_exception("@b", "@L", "got a non-numeric argument");
     >}
     >@@ = LVI_REAL(@a(r));
     >}
     >HEREDOC
     )))

(define (build-special-math-cmplx s-name c-name r aL a1)
  (let ((aa c-name)
	(ab s-name))
    (emit-code
     #<< HEREDOC
     >{
     >lisp_cmplx_t z;
     >switch (@1.vt) {
     >case LV_INT:
     >z = @1.v.iv;
     >break;
     >case LV_RAT:
     >z = LV_RAT2REAL(@1);
     >break;
     >case LV_REAL:
     >z = @1.v.rv;
     >break;
     >case LV_CMPLX:
     >z = @1.v.cv;
     >break;
     >default:
     >wile_exception("@b", "@L", "got a non-numeric argument");
     >}
     >z = @a(z);
     >if (CIMAG(z) == 0.0) {
     >@@ = LVI_REAL(CREAL(z));
     >} else {
     >@@ = LVI_CMPLX1(z);
     >}
     >}
     >HEREDOC
     )))

(define (build-special-math-rc s-name cr-name cc-name r aL a1)
  (let ((aa cr-name)
	(ab cc-name)
	(ac s-name))
    (emit-code
     #<< HEREDOC
     >{
     >lisp_real_t r;
     >lisp_cmplx_t z;
     >bool isr;
     >switch (@1.vt) {
     >case LV_INT:
     >r = @1.v.iv;
     >isr = true;
     >break;
     >case LV_RAT:
     >r = LV_RAT2REAL(@1);
     >isr = true;
     >break;
     >case LV_REAL:
     >r = @1.v.rv;
     >isr = true;
     >break;
     >case LV_CMPLX:
     >z = @1.v.cv;
     >isr = false;
     >break;
     >default:
     >wile_exception("@c", "@L", "got a non-numeric argument");
     >}
     >if (isr) {
     >@@ = LVI_REAL(@a(r));
     >} else {
     >z = @b(z);
     >if (CIMAG(z) == 0.0) {
     >@@ = LVI_REAL(CREAL(z));
     >} else {
     >@@ = LVI_CMPLX1(z);
     >}
     >}
     >}
     >HEREDOC
     )))

(define (build-real-cmp aa r aL a1 a2)
  (emit-code
   #<< HEREDOC
   >switch (TYPE_COMBO(@1.vt,@2.vt)) {
   >case TYPE_COMBO(LV_INT,LV_INT):
   >@@ = LVI_BOOL(@1.v.iv @a @2.v.iv);
   >break;
   >case TYPE_COMBO(LV_INT,LV_RAT):
   >@@ = LVI_BOOL(@1.v.iv * @2.v.irv.den @a @2.v.irv.num);
   >break;
   >case TYPE_COMBO(LV_INT,LV_REAL):
   >@@ = LVI_BOOL(@1.v.iv @a @2.v.rv);
   >break;
   >case TYPE_COMBO(LV_RAT,LV_INT):
   >@@ = LVI_BOOL(@1.v.irv.num @a @2.v.iv * @1.v.irv.den);
   >break;
   >case TYPE_COMBO(LV_RAT,LV_RAT):
   >@@ = LVI_BOOL(@1.v.irv.num * @2.v.irv.den @a @2.v.irv.num * @1.v.irv.den);
   >break;
   >case TYPE_COMBO(LV_RAT,LV_REAL):
   >@@ = LVI_BOOL(@1.v.irv.num @a @2.v.rv * @1.v.irv.den);
   >break;
   >case TYPE_COMBO(LV_REAL,LV_INT):
   >@@ = LVI_BOOL(@1.v.rv @a @2.v.iv);
   >break;
   >case TYPE_COMBO(LV_REAL,LV_RAT):
   >@@ = LVI_BOOL(@1.v.rv * @2.v.irv.den @a @2.v.irv.num);
   >break;
   >case TYPE_COMBO(LV_REAL,LV_REAL):
   >@@ = LVI_BOOL(@1.v.rv @a @2.v.rv);
   >break;
   >default:
   >wile_exception("@a", "@L", "inputs are not real-valued numbers");
   >break;
   >}
   >HEREDOC
   ))

;;; arity meaning:
;;; 0 or more	-> a function which accepts exactly that many arguments
;;; negative	-> a function which accepts (- (- arity) 1) or more arguments
;;;		   -1 -> 0 or more, -2 -> 1 or more, -3 -> 2 or more, etc

(define prim-table-internal
  (list

   '(creal alias real-part)
   '(cimag alias imag-part)
   '(last alias list-last)
   '(make-rectangular alias cmplx)
   '(phase alias angle)
   '(complex-conjugate alias cconj)
   '(conj alias cconj)
   '(number? alias complex?)
   '(char-lower-case? alias char-lowercase?)
   '(char-upper-case? alias char-uppercase?)
   '(list->string alias char->string)
   '(magnitude alias abs)
   '(modulo alias floor-remainder)
   '(quotient alias truncate-quotient)
   '(remainder alias truncate-remainder)
   '(quot-rem alias truncate/)
   '(directory-exists? alias file-exists?)
   '(rename-directory alias rename-file)
   '(make-vector alias vector-create)
   '(make-string alias string-create)
   '(make-bytevector alias bytevector-create)
   '(substring alias string-copy)
   '(read-all alias parse-file)
   '(sqlite-close alias close-port)
   '(agm alias arithmetic-geometric-mean)

   '(call-with-current-continuation alias call/cc)

   '(make-iproc alias make-interpreted-procedure)
   '(set-iproc-env! alias set-interpreted-procedure-environment!)
   '(set-iproc-macro! alias set-interpreted-procedure-macro!)
   '(get-iproc-args alias get-interpreted-procedure-arguments)
   '(get-iproc-arity alias get-interpreted-procedure-arity)
   '(get-iproc-env alias get-interpreted-procedure-environment)
   '(get-iproc-body alias get-interpreted-procedure-body)
   '(get-iproc-macro alias get-interpreted-procedure-macro)

   '(sha-224-update alias sha-256-update)
   '(sha-224-finish alias sha-256-finish)

   '(write-1str alias write-string)
   '(write-char alias write-string)

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

   (list 'fluid-let
	 "this is analogous to the various let-forms, except that it does not introduce new bindings, but rather gives temporary values to existing bindings; after all the expressions of the fluid-let form are evaluated, the prior values are reinstated"
	 'macro -2
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

   ;;; if not exactly case-lambda, then a lot like it

   (list 'case-lambic
	 "expects a positive and even number of arguments, alternating argument counts and lambdas which take those numbers of arguments, and constructs a wrapper function which takes any number of arguments and dispatches to the proper specific case; similar to case-lambda, but slightly different syntax"
	 'macro -3
	 (lambda (n lam . fns)
	   (let* ((args (gensym))
		  (group
		   (let loop ((cs (list (list n lam)))
			      (fs fns))
		     (cond ((null? fs)
			    (list-reverse (cons (list #f ()) cs)))
			   ((null? (cdr fs))
			    (list-reverse (cons (list #t (car fs)) cs)))
			   (else
			    (loop (cons (list (car fs) (cadr fs)) cs)
				  (cddr fs))))))
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
		  ,@cases)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; these macros are part of closing the loop for wile to compile itself

   (list 'load-library
	 "expects a file name and looks for that file in any of the directories given in the colon-separated list specified by the environment variable WILE_LIBRARY_PATH; if found, loads the file as though it were specified by its absolute path, thereby making its contents available for compilation or interpretation"
	 'macro 1
	 (lambda (fname)
	   (letrec* ((paths (string-split-by
			     (lambda (c) (eqv? c #\:))
			     (get-environment-variable "WILE_LIBRARY_PATH")))
		     (find (lambda (ps)
			     (if (null? ps)
				 #f
				 (let* ((dir (car ps))
					(fp (if (string=? dir ".")
						fname
						(string-join-by
						 "/" dir fname))))
				   (if (file-exists? fp)
				       fp
				       (find (cdr ps)))))))
		     (filepath (find paths)))
	     (if filepath
		 `(load ,filepath)
		 `(write-string "unable to find file '" ,fname "'\n")))))

;;; Keep this around: the interpreted version that's generated by
;;; defmacro can crash because the interpreter isn't properly
;;; tail-recursive. This version gets compiled and works fine because
;;; of that.

   (list 'emit-code
	 "this macro is used inside wile; do not use"
	 'macro 1
	 (lambda (str)
	   (let ((xform
		  (let loop ((cs (string->list str))
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

   (list 'def-struct
	 "expects a structure name and one or more field names, and defines a set of creation, test, and access functions for that structure. structures are simply vectors that know their own struct type"
	 'macro -3
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

   (list 'begin-breakable
	 "expects a tag symbol and any number of expressions, and evaluates the expressions; if any of them is a (raise) which throws that tag symbol, the evaluations are terminated"
	 'macro -2
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

;;;   (list 'namespace
;;;	 "expects a list of symbols which are functions to make visible, then any number of function definitions, which will be private except for those listed as public. CURRENTLY BROKEN"
;;;	 'macro -2
;;;	 (lambda (syms . defs)
;;;	   `(begin ,@defs)))

   (list 'cons "expects two values A and B and returns the newly-allocated pair (A B)"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lptr p1 = NULL, p2 = NULL;
	    >if (@1.vt != LV_NIL) {
	    >p1 = new_lv(LV_NIL);
	    >*p1 = @1;
	    >}
	    >if (@2.vt != LV_NIL) {
	    >p2 = new_lv(LV_NIL);
	    >*p2 = @2;
	    >}
	    >@@ = LVI_PAIR(p1, p2);
	    >}
	    >HEREDOC
	    )))

   (list 'car "expects a pair (A B) and returns its first element A"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_PAIR) {
	    >wile_exception("car", "@L", "input is not a pair!");
	    >}
	    >@@ = (@1.v.pair.car ? *(@1.v.pair.car) : LVI_NIL());
	    >HEREDOC
	    )))

   (list 'cdr "expects a pair (A B) and returns its second element B"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_PAIR) {
	    >wile_exception("cdr", "@L", "input is not a pair!");
	    >}
	    >@@ = (@1.v.pair.cdr ? *(@1.v.pair.cdr) : LVI_NIL());
	    >HEREDOC
	    )))

   (list 'set-car! "expects one pair and one general value, and sets the pair's CAR to the specified general value. WARNING: THIS IS CURRENTLY BROKEN!"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >if (@1.vt != LV_PAIR) {
	    >wile_exception("set-car!", "@L", "input is not a pair!");
	    >}
	    >lptr p2 = NULL;
	    >if (@2.vt != LV_NIL) {
	    >p2 = new_lv(LV_NIL);
	    >*p2 = @2;
	    >}
	    >@1.v.pair.car = p2;
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   (list 'set-cdr! "expects one pair and one general value, and sets the pair's CDR to the specified general value. WARNING: THIS IS CURRENTLY BROKEN!"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >if (@1.vt != LV_PAIR) {
	    >wile_exception("set-cdr!", "@L", "input is not a pair!");
	    >}
	    >lptr p2 = NULL;
	    >if (@2.vt != LV_NIL) {
	    >p2 = new_lv(LV_NIL);
	    >*p2 = @2;
	    >}
	    >@1.v.pair.cdr = p2;
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   (list 'cxr "expects one control string and one nested list object and returns the appropriate composition of car and cdr as encoded by the control string"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >char* cp = strchr(@1.v.str, 'r');
	    >@@ = @2;
	    >while (*(--cp) != 'c') {
	    >if (@@.vt != LV_PAIR) {
	    >wile_exception("cxr", "@L", "input does not have the right structure!");
	    >}
	    >if (*cp == 'a') {
	    >@@ = (@@.v.pair.car ? *(@@.v.pair.car) : LVI_NIL());
	    >} else if (*cp == 'd') {
	    >@@ = (@@.v.pair.cdr ? *(@@.v.pair.cdr) : LVI_NIL());
	    >} else {
	    >wile_exception("cxr", "@L", "got malformed control string '%%s'", @1.v.str);
	    >}
	    >}
	    >}
	    >HEREDOC
	    )))

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

   (list 'cadddar "expects one nested list object and returns the appropriate composition of car and cdr"
	 'macro 1 (lambda (a1) `(cxr "cadddar" ,a1)))

   (list 'pair?
	 "expects one argument and returns #t if that value is a pair, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_PAIR);")))

   (list 'list
	 "expects any number of values and returns the list containing those values"
	 'prim -1 build-basic-list)

   (list 'list?
	 "expects one argument and returns #t if that value is a proper list, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >@@ = @1;
	    >while (@@.vt == LV_PAIR) {
	    >@@ = (@@.v.pair.cdr ? *(@@.v.pair.cdr) : LVI_NIL());
	    >}
	    >@@ = LVI_BOOL(@@.vt == LV_NIL);
	    >}
	    >HEREDOC
	    )))

   (list 'vector
	 "expects any number of values and returns a vector containing those values"
	 'prim -1 "wile_list2vector")

   (list 'bytevector
	 "expects any number of character or small integer values and returns a bytevector containing those values"
	 'prim -1 "wile_list2bytevector")

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
	    #<< HEREDOC
	    >{
	    >FILE* fp = fopen(@1.v.str, @2.v.str);
	    >if (fp) {
	    >@@ = LVI_FPORT(fp);
	    >} else {
	    >@@ = LVI_BOOL(false);
	    >}
	    >}
	    >HEREDOC
	    )))

   ;;; syntactic sugar, but I expect to find this all the time
   ;;; and get annoyed when it's not there

   (list 'open-input-file "expects one string argument, the name of a file to open for reading. if successful, return a file-port; otherwise return #f"
	 'macro 1 (lambda (a1) `(open-file ,a1 "r")))

   (list 'open-temporary-file "expects one string argument which should be a filename template containing at least six 'X' characters (if it does not, they get appended to a copy of the input string); returns a two-list containing a file port opened in 'w+' mode and the actual name of the file that got opened"
	 'prim 1 "wile_temp_file")

   (list 'close-port "expects one expects one port argument, and closes it; returns #t if the close succeeded, #f otherwise"
	 'prim 1 "wile_closeport")

   (list 'create-link "expects two string arguments, the first one of which should be the name of an existing file, and creates a new hard link under the second name"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_STRING || @2.vt != LV_STRING) {
	    >wile_exception("create-link", "@L", "inputs are not strings");
	    >}
	    >@@ = LVI_BOOL(link(@1.v.str, @2.v.str) == 0);
	    >HEREDOC
	    )))

   (list 'create-symbolic-link "expects two string arguments, the first one of which should be the name of an existing file, and creates a new symbolic link under the second name"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_STRING || @2.vt != LV_STRING) {
	    >wile_exception("create-symbolic-link", "@L", "inputs are not strings");
	    >}
	    >@@ = LVI_BOOL(symlink(@1.v.str, @2.v.str) == 0);
	    >HEREDOC
	    )))

   (list 'eqv?
	 "expects two general values, and returns whether or not they are equivalent"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL(wile_do_eqv(&(@1), &(@2)));")))

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
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_CLAMBDA || @1.vt == LV_ILAMBDA);")))

   (list 'compiled-procedure?
	 "expects one argument and returns #t if that value is a procedure, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_CLAMBDA);")))

   (list 'interpreted-procedure?
	 "expects one argument and returns #t if that value is an interpreted procedure, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_ILAMBDA);")))

   (list 'continuation?
	 "expects one argument and returns #t if that value is a continuation, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_CONT);")))

   (list 'get-process-id
	 "expects no arguments, returns the process id of the running process" 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(getpid());")))

   (list 'get-parent-process-id
	 "expects no arguments, returns the process id of the parent process of the running process" 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(getppid());")))

   (list 'get-user-id
	 "expects no arguments, returns the user id of the running process" 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(getuid());")))

   (list 'set-user-id
	 "expects one integer and sets the user id of the running process to that value; returns #t if the operation succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(setuid(@1.v.iv) == 0);")))

   (list 'get-effective-user-id
	 "expects no arguments, returns the effective user id of the running process"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(geteuid());")))

   (list 'set-effective-user-id
	 "expects one integer and sets the effective user id of the running process to that value; returns #t if the operation succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(seteuid(@1.v.iv) == 0);")))

   (list 'get-group-id
	 "expects no arguments, returns the group id of the running process" 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(getgid());")))

   (list 'set-group-id
	 "expects one integer and sets the group id of the running process to that value; returns #t if the operation succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(setgid(@1.v.iv) == 0);")))

   (list 'get-effective-group-id
	 "expects no arguments, returns the effective group id of the running process"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(getegid());")))

   (list 'set-effective-group-id
	 "expects one integer and sets the effective group id of the running process to that value; returns #t if the operation succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(setegid(@1.v.iv) == 0);")))

   (list 'get-session-id
	 "expects one integer argument and returns the session id of the process with that process id"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_INT(getsid(@1.v.iv));")))

   (list 'set-session-id
	 "expects no arguments and creates a new session"
	 'prim 0
	 (lambda (r)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >pid_t si = setsid();
	    >if (si >= 0) {
	    >@@ = LVI_INT(si);
	    >} else {
	    >@@ = LVI_BOOL(false);
	    >}
	    >}
	    >HEREDOC
	    )))

   (list 'epochtime
	 "expects no arguments, returns the number of seconds since the epoch, 1970-01-01 00:00:00 UTC" 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(time(NULL));")))

   (list 'apply
	 "expects one procedure and any number of arguments, and returns the result of applying the procedure to those arguments"
	 'priml -3 compile-runtime-apply)

   (list 'gensym
	 "expects no arguments, returns one newly-generated symbol which is supposed to be unique unless the user takes hostile measures to defeat the uniqueness"
	 'prim 0 (lambda (r)
		   ;;; TODO: get a better origin than 0
		   (emit-code "@@ = wile_get_gensym(0);")))

   (list 'run-command
	 "expects one string argument, runs that as a separate process, and returns the exit status of that run or #f if the underlying system() call failed"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code "@@ = wile_run_system_command(@1, \"@L\");")))

   (list 'run-read-command
	 "expects one string argument, launches that as a separate process while opening a readable pipe to its stdout, and returns that pipe-handle, or #f if the underlying popen() call failed"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code "@@ = wile_run_pipe_command(@1, \"r\", \"@L\");")))

   (list 'run-write-command
	 "expects one string argument, launches that as a separate process while opening a writable pipe to its stdin, and returns that pipe-handle, or #f if the underlying popen() call failed"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code "@@ = wile_run_pipe_command(@1, \"w\", \"@L\");")))

   (list 'fork-process
	 "expects no arguments and forks the process into parent and child processes; returns 0 in the child process and the child's process id in the parent process, or #f if the underlying fork() call failed"
	 'prim 0
	 (lambda (r)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >int si = fork();
	    >if (si >= 0) {
	    >@@ = LVI_INT(si);
	    >} else {
	    >@@ = LVI_BOOL(false);
	    >}
	    >}
	    >HEREDOC
	    )))

   (list 'wait-process
	 "expects 0, 1, or 2 arguments: the first argument, if present, is an integer process id on which to wait, and the second argument, if present, is an integer representing an OR of possible options. if no arguments are given, waits for any child process. returns a list of two integers, the process id of the terminated child, and its exit status, or #f"
	 'priml
	 0 (lambda (r aL)
	     (emit-code "@@ = wile_waitpid(-1, 0);"))
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt == LV_INT) {
	      >@@ = wile_waitpid(@1.v.iv, 0);
	      >} else {
	      >wile_exception("wait-process", "@L", "input is not an integer");
	      >}
	      >HEREDOC
	      ))
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt == LV_INT && @2.vt == LV_INT) {
	      >@@ = wile_waitpid(@1.v.iv, @2.v.iv);
	      >} else {
	      >wile_exception("wait-process", "@L", "inputs are not both integers");
	      >}
	      >HEREDOC
	      )))

   (list 'get-current-directory
	 "expects no arguments and returns a string which is the name of the current working directory" 'prim 0 "wile_getcwd")

   (list 'set-current-directory
	 "expects one string argument, attempts to change the current working directory to that location, and returns #t if that succeeds or #f if it fails"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(chdir(@1.v.str) == 0);")))

   (list 'get-file-position
	 "expects one file-port argument and returns the current value of its file position indicator"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_FILE_PORT) {
	    >long int offset = ftell(@1.v.fp);
	    >if (offset < 0) {
	    >@@ = LVI_BOOL(false);
	    >} else {
	    >@@ = LVI_INT(offset);
	    >}
	    >} else {
	    >wile_exception("get-file-position", "@L", "input is not a file port");
	    >}
	    >HEREDOC
	    )))

   ;;; offset relative to start for 2-arg
   (list 'set-file-position
	 "expects a file port, an integer offset, and an optional position indicator symbol 'start 'cur or 'end; sets the file position indicator as an offset from the given location. if no location is specified, the offset is from the start of the file"
	 'prim 2 "wile_setfilepos2" 3 "wile_setfilepos3")

   (list 'set-line-buffering!
	 "expects one argument which is a port of some kind: for file, pipe, or socket-type ports, set buffering mode to line buffering; for string or sqlite ports, this is a no-op. return #t or #f according to whether the call succeeds or fails"
	 'prim 1 "wile_setlinebuffering")

   (list 'set-no-buffering!
	 "expects one argument which is a port of some kind: for file, pipe, or socket-type ports, set buffering mode to no buffering; for string or sqlite ports, this is a no-op. return #t or #f according to whether the call succeeds or fails"
	 'prim 1 "wile_setnobuffering")

   (list 'get-host-name
	 "expects no arguments and returns the system's host name as given by gethostname(), or #f if gethostname fails"
	 'prim 0 "wile_gethostname")

   (list 'get-domain-name
	 "expects no arguments and returns the system's domain name as given by getdomainname(), or #f if getdomainname fails"
	 'prim 0 "wile_getdomainname")

   (list 'read-line
	 "expects one input port argument and reads and returns one line, ie, up to the next #\newline character; if the port is positioned at EOF, ie, there are no more characters to be read, returns #f"
	 'prim 1 "wile_read_line")

   (list 'read-line-interactive
	 "expects one string argument which it prints as a prompt, then reads from stdin and returns one line, with nice interactive editing and history recall; on error, returns #f"
	 'prim 1 "wile_read_line_interactive")

   (list 'read-char "expects no arguments or one port argument, attempts to read one character from stdin or the port, and returns the character if the read was successful, #f otherwise"
	 'priml
	 0 (lambda (r aL)
	     (emit-code
	      #<< HEREDOC
	      >{
	      >int c = fgetc(stdin);
	      >@@ = ((c == EOF) ? LVI_BOOL(false) : LVI_CHAR(c));
	      >}
	      >HEREDOC
	      ))
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt == LV_FILE_PORT || @1.vt == LV_PIPE_PORT || @1.vt == LV_SOCK_PORT) {
	      >int c = fgetc(@1.v.fp);
	      >@@ = ((c == EOF) ? LVI_BOOL(false) : LVI_CHAR(c));
	      >} else {
	      >wile_exception("read-char", "@L", "input is not a port");
	      >}
	      >HEREDOC
	      )))

   (list 'newline "expects one optional argument which is an output port, and writes a newline character to that port; the default port if no argument is given is stdout"
	 'priml
	 0 (lambda (r aL)
	     (emit-code
	      #<< HEREDOC
	      >@@ = LVI_BOOL(true);
	      >putchar('\n');
	      >HEREDOC
	      ))
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt == LV_FILE_PORT || @1.vt == LV_PIPE_PORT || @1.vt == LV_SOCK_PORT) {
	      >fputc('\n', @1.v.fp);
	      >@@ = LVI_BOOL(true);
	      >} else {
	      >wile_exception("newline", "@L", "input is not a port");
	      >}
	      >HEREDOC
	      )))

   (list 'write-string "expects one or more arguments: a number of strings or characters (optionally the first argument may be an output port); all string or character arguments are written to the output port, which defaults to stdout if not specified; returns #t"
	 'priml -2
	 (lambda (r aL a1 . as)
	   (fluid-let ((r #f))
	     (if (null? as)
		 (emit-code
		  #<< HEREDOC
		  >if (@1.vt == LV_CHAR) {
		  >fputc(@1.v.chr, stdout);
		  >} else if (@1.vt == LV_STRING) {
		  >fputs(@1.v.str, stdout);
		  >} else {
		  >wile_exception("write-string", "@L", "input is not a string or char!");
		  >}
		  >HEREDOC
		  )
		 (begin
		   (emit-fstr "{\n")
		   (emit-code
		    #<< HEREDOC
		    >FILE* fp;
		    >if (@1.vt == LV_FILE_PORT || @1.vt == LV_PIPE_PORT || @1.vt == LV_SOCK_PORT) {
		    >fp = @1.v.fp;
		    >} else if (@1.vt == LV_CHAR) {
		    >fp = stdout;
		    >fputc(@1.v.chr, fp);
		    >} else if (@1.vt == LV_STRING) {
		    >fp = stdout;
		    >fputs(@1.v.str, fp);
		    >} else {
		    >wile_exception("write-string", "@L", "first input is not a string or char or port!");
		    >}
		    >HEREDOC
		    )
		   (for-each
		    (lambda (aA)
		      (emit-code
		       #<< HEREDOC
		       >if (@A.vt == LV_CHAR) {
		       >fputc(@A.v.chr, fp);
		       >} else if (@A.vt == LV_STRING) {
		       >fputs(@A.v.str, fp);
		       >} else {
		       >wile_exception("write-string", "@L", "input is not a string or char!");
		       >}
		       >HEREDOC
		       ))
		    as)
		   (emit-fstr "}\n"))))
	   (emit-code "@@ = LVI_BOOL(true);")))

   (list 'display
	 "expects one lisp value and optionally one output port (defaults to stdout if not given) and writes some representation of the value to the port; returns nothing useful. warning: this does NOT YET do cycle detection; if given a cyclic datastructure, it will loop indefinitely"
	 'priml
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >wile_print_lisp_val(&(@1), stdout, "@L");
	      >@@ = @1;
	      >
	      >HEREDOC
	      ))
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >if (@2.vt == LV_FILE_PORT || @2.vt == LV_PIPE_PORT || @2.vt == LV_SOCK_PORT) {
	      >wile_print_lisp_val(&(@1), @2.v.fp, "@L");
	      >@@ = @1;
	      >} else {
	      >wile_exception("display", "@L", "expects a scheme value and a port");
	      >}
	      >HEREDOC
	      )))

   (list 'string->number "expects one string, parses it as a number, and returns the number"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code "@@ = wile_string2num(@1, \"@L\");")))

   (list 'number->string
	 "expects a number, an optional base, and an optional precision, and returns a textual representation of the number. for integers, any base from 2 to 36 is allowed; if a precision is specified, the number is formatted as a floating-point number, and only bases 2, 8, 10, and 16 are allowed. if the precision is negative, scientific notation is used, otherwise regular notation is used"
	 'priml
	 ;;; number
	 1 (lambda (r aL a1)
	     (emit-code
	      "@@ = wile_num2string(@1, 10, INT_MIN, \"@L\");"))
	 ;;; number base
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >if (@2.vt == LV_INT) {
	      >@@ = wile_num2string(@1, @2.v.iv, INT_MIN, "@L");
	      >} else {
	      >wile_exception("number->string", "@L", "base is not numeric");
	      >}
	      >HEREDOC
	      ))
	 ;;; number base precision
	 3 (lambda (r aL a1 a2 a3)
	     (emit-code
	      #<< HEREDOC
	      >if (@2.vt == LV_INT && @3.vt == LV_INT) {
	      >@@ = wile_num2string(@1, @2.v.iv, @3.v.iv, "@L");
	      >} else {
	      >wile_exception("number->string", "@L", "base or precision is not numeric");
	      >}
	      >HEREDOC
	      )))

   (list 'cmplx
	 "expects two real-valued arguments X and Y and returns the complex number X+I*Y"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar))
		 (ab (new-svar)))
	     (promote/real+check "cmplx" aa aL a1)
	     (promote/real+check "cmplx" ab aL a2)
	     (emit-code "@@ = LVI_CMPLX2(@a.v.rv, @b.v.rv);"))))

   (list 'make-polar
	 "expects two real-valued arguments R and THETA and returns the complex number R*exp(I*THETA)"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar))
		 (ab (new-svar)))
	     (promote/real+check "cmplx" aa aL a1)
	     (promote/real+check "cmplx" ab aL a2)
	     (emit-code
	      "@@ = LVI_CMPLX2(@a.v.rv*COS(@b.v.rv), @a.v.rv*SIN(@b.v.rv));"))))

   (list 'angle
	 "expects one complex argument and returns its phase angle in radians"
	 'prim 1
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
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_REAL:
	    >@@ = LVI_BOOL(@1.v.rv == 0.0);
	    >break;
	    >case LV_RAT:
	    >@@ = LVI_BOOL((@1.v.irv.num == 0 && @1.v.irv.den != 0));
	    >break;
	    >case LV_INT:
	    >@@ = LVI_BOOL(@1.v.iv == 0);
	    >break;
	    >case LV_CMPLX:
	    >@@ = LVI_BOOL(CREAL(@1.v.cv) == 0.0 && CIMAG(@1.v.cv) == 0.0);
	    >break;
	    >default:
	    >wile_exception("zero?", "@L", "expects a real-valued number");
	    >}
	    >HEREDOC
	    )))

   (list 'positive? "expects one real-valued number and returns #t if that number is positive. the number may be of any type except complex"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_REAL:
	    >@@ = LVI_BOOL(@1.v.rv > 0.0);
	    >break;
	    >case LV_RAT:
	    >@@ = LVI_BOOL((@1.v.irv.num > 0 && @1.v.irv.den >= 0) || (@1.v.irv.num < 0 && @1.v.irv.den < 0));
	    >break;
	    >case LV_INT:
	    >@@ = LVI_BOOL(@1.v.iv > 0);
	    >break;
	    >default:
	    >wile_exception("positive?", "@L", "expects a real-valued number");
	    >}
	    >HEREDOC
	    )))

   (list 'negative? "expects one real-valued number and returns #t if that number is negative. the number may be of any type except complex"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_REAL:
	    >@@ = LVI_BOOL(@1.v.rv < 0.0);
	    >break;
	    >case LV_RAT:
	    >@@ = LVI_BOOL((@1.v.irv.num < 0 && @1.v.irv.den >= 0) || (@1.v.irv.num > 0 && @1.v.irv.den < 0));
	    >break;
	    >case LV_INT:
	    >@@ = LVI_BOOL(@1.v.iv < 0);
	    >break;
	    >default:
	    >wile_exception("negative?", "@L", "expects a real-valued number");
	    >}
	    >HEREDOC
	    )))

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
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_CMPLX:
	    >@@ = LVI_BOOL(ISNAN(CREAL(@1.v.cv)) || ISNAN(CIMAG(@1.v.cv)));
	    >break;
	    >case LV_REAL:
	    >@@ = LVI_BOOL(ISNAN(@1.v.rv));
	    >break;
	    >case LV_RAT:
	    >@@ = LVI_BOOL(@1.v.irv.num == 0 && @1.v.irv.den == 0);
	    >break;
	    >default:
	    >@@ = LVI_BOOL(false);
	    >break;
	    >}
	    >HEREDOC
	    )))

   (list 'infinite? "expects one numeric argument and returns #t if it is infinite, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_CMPLX:
	    >@@ = LVI_BOOL(ISINF(CREAL(@1.v.cv)) || ISINF(CIMAG(@1.v.cv)));
	    >break;
	    >case LV_REAL:
	    >@@ = LVI_BOOL(ISINF(@1.v.rv));
	    >break;
	    >case LV_RAT:
	    >@@ = LVI_BOOL(@1.v.irv.num != 0 && @1.v.irv.den == 0);
	    >break;
	    >default:
	    >@@ = LVI_BOOL(false);
	    >break;
	    >}
	    >HEREDOC
	    )))

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
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_CMPLX:
	    >@@ = LVI_BOOL(ISFINITE(CREAL(@1.v.cv)) && ISFINITE(CIMAG(@1.v.cv)));
	    >break;
	    >case LV_REAL:
	    >@@ = LVI_BOOL(ISFINITE(@1.v.rv));
	    >break;
	    >case LV_RAT:
	    >@@ = LVI_BOOL(@1.v.irv.den != 0);
	    >break;
	    >default:
	    >@@ = LVI_BOOL(true);
	    >break;
	    >}
	    >HEREDOC
	    )))

   (list 'abs "expects one numeric argument and returns its absolute value"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_INT:
	    >@@ = LVI_INT(WILE_ABS(@1.v.iv));
	    >break;
	    >case LV_RAT:
	    >@@ = LVI_RAT(WILE_ABS(@1.v.irv.num), WILE_ABS(@1.v.irv.den));
	    >break;
	    >case LV_REAL:
	    >@@ = LVI_REAL(WILE_ABS(@1.v.rv));
	    >break;
	    >case LV_CMPLX:
	    >@@ = LVI_REAL(CABS(@1.v.cv));
	    >break;
	    >default:
	    >wile_exception("abs", "@L", "got a non-numeric argument");
	    >}
	    >HEREDOC
	    )))

   (list 'sign "expects one real-valued argument and returns its sign, -1,0,+1 according to whether the value is negative, zero, or positive"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_INT:
	    >@@ = LVI_INT(WILE_SIGN(@1.v.iv));
	    >break;
	    >case LV_RAT:
	    >@@ = LVI_INT(WILE_SIGN(@1.v.irv.num));
	    >if (@1.v.irv.den < 0) {
	    >@@.v.iv = -@@.v.iv;
	    >}
	    >break;
	    >case LV_REAL:
	    >@@ = LVI_INT(WILE_SIGN(@1.v.rv));
	    >break;
	    >default:
	    >wile_exception("sign", "@L", "got a non-real-valued argument");
	    >}
	    >HEREDOC
	    )))

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
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_INT:
	    >@@ = LVI_INT(-@1.v.iv);
	    >break;
	    >case LV_RAT:
	    >if (@1.v.irv.den >= 0) {
	    >@@ = LVI_RAT(-@1.v.irv.num, @1.v.irv.den);
	    >} else {
	    >@@ = LVI_RAT(@1.v.irv.num, -@1.v.irv.den);
	    >}
	    >break;
	    >case LV_REAL:
	    >@@ = LVI_REAL(-@1.v.rv);
	    >break;
	    >case LV_CMPLX:
	    >@@ = LVI_CMPLX2(-CREAL(@1.v.cv), -CIMAG(@1.v.cv));
	    >break;
	    >default:
	    >wile_exception("negative", "@L", "got a non-numeric argument");
	    >}
	    >HEREDOC
	    )))

   (list 'reciprocal  "expects one number and returns its reciprocal"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_INT:
	    >if (@1.v.iv < 0) {
	    >@@ = LVI_RAT(-1, -@1.v.iv);
	    >} else {
	    >@@ = LVI_RAT(1, @1.v.iv);
	    >}
	    >break;
	    >case LV_RAT:
	    >if (@1.v.irv.num < 0) {
	    >@@ = LVI_RAT(-@1.v.irv.den, -@1.v.irv.num);
	    >} else {
	    >@@ = LVI_RAT(@1.v.irv.den, @1.v.irv.num);
	    >}
	    >break;
	    >case LV_REAL:
	    >@@ = LVI_REAL(1.0/@1.v.rv);
	    >break;
	    >case LV_CMPLX:
	    >@@ = LVI_CMPLX1(1.0/@1.v.cv);
	    >break;
	    >default:
	    >wile_exception("reciprocal", "@L", "got a non-numeric argument");
	    >}
	    >HEREDOC
	    )))

   (list 'i+ "expects two integer-typed numbers and returns their sum, also as an integer-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT(@1.v.iv + @2.v.iv);")))

   (list 'i++
	 "expects two or more integer-typed numbers and returns their sum, also as an integer-typed number"
	 'prim -3
	 (lambda (r a1 a2 . as)
	   (emit-decl r)
	   (emit-fstr "%s = LVI_INT(%s.v.iv + %s.v.iv" r a1 a2)
	   (for-each (lambda (a) (emit-fstr " + %s.v.iv" a)) as)
	   (emit-fstr ");\n")
	   r))

   (list 'i-
	 "expects two integer-typed numbers and returns their difference, also as an integer-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT(@1.v.iv - @2.v.iv);")))

   (list 'i--
	 "expects two or more integer-typed numbers and returns the difference between the first and the sum of the rest, also as an integer-typed number"
	 'prim -3
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
	    #<< HEREDOC
	    >if (@2.v.iv < 0) {
	    >@@ = LVI_RAT(-@1.v.iv, -@2.v.iv);
	    >} else {
	    >@@ = LVI_RAT(@1.v.iv, @2.v.iv);
	    >}
	    >HEREDOC
	    )))

   (list 'q+ "expects two rational-typed numbers and returns their sum, also as a rational-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t n, d, g;
	    >n = @1.v.irv.num * @2.v.irv.den + @2.v.irv.num * @1.v.irv.den;
	    >d = @1.v.irv.den * @2.v.irv.den;
	    >g = lgcd(n, d);
	    >n /= g;
	    >d /= g;
	    >@@ = LVI_RAT(n, d);
	    >}
	    >HEREDOC
	    )))

   (list 'q- "expects two rational-typed numbers and returns their difference, also as a rational-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t n, d, g;
	    >n = @1.v.irv.num * @2.v.irv.den - @2.v.irv.num * @1.v.irv.den;
	    >d = @1.v.irv.den * @2.v.irv.den;
	    >g = lgcd(n, d);
	    >n /= g;
	    >d /= g;
	    >@@ = LVI_RAT(n, d);
	    >}
	    >HEREDOC
	    )))

   (list 'q* "expects two rational-typed numbers and returns their product, also as a rational-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t n, d, g;
	    >n = @1.v.irv.num * @2.v.irv.num;
	    >d = @1.v.irv.den * @2.v.irv.den;
	    >g = lgcd(n, d);
	    >n /= g;
	    >d /= g;
	    >@@ = LVI_RAT(n, d);
	    >}
	    >HEREDOC
	    )))

   (list 'q/ "expects two rational-typed numbers and returns their ratio, also as a rational-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t n, d, g;
	    >n = @1.v.irv.num * @2.v.irv.den;
	    >d = @1.v.irv.den * @2.v.irv.num;
	    >g = lgcd(n, d);
	    >if (d < 0) {
	    >g = -g;
	    >}
	    >n /= g;
	    >d /= g;
	    >@@ = LVI_RAT(n, d);
	    >}
	    >HEREDOC
	    )))

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

   (list 'c*
	 "expects two complex-typed numbers and returns their product, also as a complex-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_CMPLX1(@1.v.cv * @2.v.cv);")))

   (list 'c/
	 "expects two complex-typed numbers and returns their ratio, also as a complex-typed number"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_CMPLX1(@1.v.cv / @2.v.cv);")))

   (list 'min/i
	 "expects two integer-typed values, and returns the smaller of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@1.v.iv < @2.v.iv) ? @1.v.iv : @2.v.iv);")))

   (list 'max/i
	 "expects two integer-typed values, and returns the larger of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@1.v.iv > @2.v.iv) ? @1.v.iv : @2.v.iv);")))

   (list 'min/q
	 "expects two rational-typed values, and returns the smaller of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.v.irv.num * @2.v.irv.den < @2.v.irv.num * @1.v.irv.den) {
	    >@@ = @1;
	    >} else {
	    >@@ = @2;
	    >}
	    >HEREDOC
	    )))

   (list 'max/q
	 "expects two rational-typed values, and returns the larger of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.v.irv.num * @2.v.irv.den > @2.v.irv.num * @1.v.irv.den) {
	    >@@ = @1;
	    >} else {
	    >@@ = @2;
	    >}
	    >HEREDOC
	    )))

   (list 'min/r
	 "expects two real-typed values, and returns the smaller of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL((@1.v.rv < @2.v.rv) ? @1.v.rv : @2.v.rv);")))

   (list 'max/r
	 "expects two real-typed values, and returns the larger of the two"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL((@1.v.rv > @2.v.rv) ? @1.v.rv : @2.v.rv);")))

   ;;; no such thing as min/c or max/c

   ;;; return the number-tower type of a number:
   ;;; int 0, rat 1, real 2, complex 3, all other values 4

   (list 'number/type
	 "expects one value: if {integer,rational,real,complex} type respectively, return {0,1,2,3}; for all others, return 4"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_INT:
	    >@@ = LVI_INT(0);
	    >break;
	    >case LV_RAT:
	    >@@ = LVI_INT(1);
	    >break;
	    >case LV_REAL:
	    >@@ = LVI_INT(2);
	    >break;
	    >case LV_CMPLX:
	    >@@ = LVI_INT(3);
	    >break;
	    >default:
	    >@@ = LVI_INT(4);
	    >break;
	    >}
	    >HEREDOC
	    )))

   ;;; ditto for vector

   (list 'vector-number/type
	 "expects one vector, and returns the max number-type (see number/type) for all values in the vector"
	 'priml 1
	 (lambda (r aL a1)
	   (let ((aa (new-svar 'lbl)))
	     (emit-code
	      #<< HEREDOC
	      >{
	      >if (@1.vt != LV_VECTOR) {
	      >wile_exception("vector-number/type", "@L", "input is not a vector");
	      >}
	      >int ty = 0;
	      >size_t i;
	      >for (i = 0; i < @1.v.vec.capa; ++i) {
	      >if (@1.v.vec.arr[i]) {
	      >switch (@1.v.vec.arr[i]->vt) {
	      >case LV_INT:
	      >break;
	      >case LV_RAT:
	      >if (ty < 1) {
	      >ty = 1;
	      >}
	      >break;
	      >case LV_REAL:
	      >if (ty < 2) {
	      >ty = 2;
	      >}
	      >break;
	      >case LV_CMPLX:
	      >if (ty < 3) {
	      >ty = 3;
	      >}
	      >break;
	      >default:
	      >ty = 4;
	      >goto @a;
	      >}
	      >} else {
	      >ty = 4;
	      >goto @a;
	      >}
	      >}
	      >@a:
	      >@@ = LVI_INT(ty);
	      >}
	      >HEREDOC
	      ))))

   ;;; promote ints to rationals, leave all else untouched

   (list 'promote/rat
	 "expects one value; integers are promoted to rational type, all others are returned unchanged"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_INT) {
	    >@@ = LVI_RAT(@1.v.iv, 1);
	    >} else {
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   ;;; ditto for vector

   (list 'vector-promote/rat!
	 "expects one vector; all its integer elements are promoted to rational type, all others are preserved"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >if (@1.vt != LV_VECTOR) {
	    >wile_exception("vector-promote/rat!", "@L", "input is not a vector");
	    >}
	    >size_t i;
	    >for (i = 0; i < @1.v.vec.capa; ++i) {
	    >if (@1.v.vec.arr[i] && @1.v.vec.arr[i]->vt == LV_INT) {
	    >*(@1.v.vec.arr[i]) = LVI_RAT(@1.v.vec.arr[i]->v.iv, 1);
	    >}
	    >}
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   ;;; promote ints and rationals to reals, leave all else untouched

   (list 'promote/real
	 "expects one value; integers and rationals are promoted to real type, all others are returned unchanged"
	 'prim 1 promote/real)

   ;;; ditto for vector

   (list 'vector-promote/real!
	 "expects one vector; all its integer and rational elements are promoted to real type, all others are preserved"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >if (@1.vt != LV_VECTOR) {
	    >wile_exception("vector-promote/real!", "@L", "input is not a vector");
	    >}
	    >size_t i;
	    >for (i = 0; i < @1.v.vec.capa; ++i) {
	    >if (@1.v.vec.arr[i]) {
	    >if (@1.v.vec.arr[i]->vt == LV_INT) {
	    >*(@1.v.vec.arr[i]) = LVI_REAL((lisp_real_t) (@1.v.vec.arr[i]->v.iv));
	    >} else if (@1.v.vec.arr[i]->vt == LV_RAT) {
	    >*(@1.v.vec.arr[i]) = LVI_REAL(LV_RAT2REAL(*(@1.v.vec.arr[i])));
	    >}
	    >}
	    >}
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   ;;; promote ints rationals and reals to complex, leave all else untouched

   (list 'promote/cmplx "expects one value; integers, rationals, and reals are promoted to complex type, all others are returned unchanged"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >switch (@1.vt) {
	    >case LV_INT:
	    >@@ = LVI_CMPLX2((lisp_real_t) @1.v.iv, 0);
	    >break;
	    >case LV_RAT:
	    >@@ = LVI_CMPLX2(LV_RAT2REAL(@1), 0);
	    >break;
	    >case LV_REAL:
	    >@@ = LVI_CMPLX2(@1.v.rv, 0);
	    >break;
	    >default:
	    >@@ = @1;
	    >break;
	    >}
	    >HEREDOC
	    )))

   ;;; ditto for vector

   (list 'vector-promote/cmplx!
	 "expects one vector; all its numeric elements are promoted to complex type, all others are preserved"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >if (@1.vt != LV_VECTOR) {
	    >wile_exception("vector-promote/cmplx!", "@L", "input is not a vector");
	    >}
	    >size_t i;
	    >for (i = 0; i < @1.v.vec.capa; ++i) {
	    >if (@1.v.vec.arr[i]) {
	    >if (@1.v.vec.arr[i]->vt == LV_INT) {
	    >*(@1.v.vec.arr[i]) = LVI_CMPLX2((lisp_real_t) (@1.v.vec.arr[i]->v.iv), 0.0);
	    >} else if (@1.v.vec.arr[i]->vt == LV_RAT) {
	    >*(@1.v.vec.arr[i]) = LVI_CMPLX2(LV_RAT2REAL(*(@1.v.vec.arr[i])), 0.0);
	    >} else if (@1.v.vec.arr[i]->vt == LV_REAL) {
	    >*(@1.v.vec.arr[i]) = LVI_CMPLX2(@1.v.vec.arr[i]->v.rv, 0.0);
	    >}
	    >}
	    >}
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   ;;; constructor to make numbers in rational format; it is UNSAFE!
   ;;; it can create numbers in non-canonical formats, 3/6, and also
   ;;; 0/1, 0/0 aka NaN, 1/0 and -1/0 aka infinities. use with care!

   (list 'make-rational
	 "expects two integers N and D and returns the rational number N/D; note that this can construct non-canonical numbers such as 3/6, 0/1, 1/0, etc"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_RAT(@1.v.iv, @2.v.iv);")))

   (list 'gcd
	 "expects two integer arguments and returns their greatest common divisor"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_INT(lgcd(@1.v.iv, @2.v.iv));")))

   (list 'lcm
	 "expects two integer arguments and returns their least common multiple"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_INT(@1.v.iv*(@2.v.iv/lgcd(@1.v.iv, @2.v.iv)));")))

   (list 'ilog
	 "expects one integer, and returns the integer binary logarithm of the absolute value; as a special case, it returns 0 for input 0"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t v, c;
	    >v = @1.v.iv;
	    >if (v < 0) {
	    >v = -v;
	    >}
	    >if (v == 0) {
	    >c = 0;
	    >} else {
	    >c = -1;
	    >while (v > 0) {
	    >v /= 2;
	    >++c;
	    >}
	    >}
	    >@@ = LVI_INT(c);
	    >}
	    >HEREDOC
	    )))

   (list '< "expects two real-valued numbers and returns #t if the first is less than the second, #f otherwise"
	 'priml 2
	 (lambda (r aL a1 a2) (build-real-cmp "<" r aL a1 a2)))

   (list '<= "expects two real-valued numbers and returns #t if the first is less than or equal to the second, #f otherwise"
	 'priml 2
	 (lambda (r aL a1 a2) (build-real-cmp "<=" r aL a1 a2)))

   (list '= "expects two real-valued numbers and returns #t if the first is equal to the second, #f otherwise"
	 'priml 2
	 (lambda (r aL a1 a2) (build-real-cmp "==" r aL a1 a2)))

   (list '>= "expects two real-valued numbers and returns #t if the first is greater than or equal to the second, #f otherwise"
	 'priml 2
	 (lambda (r aL a1 a2) (build-real-cmp ">=" r aL a1 a2)))

   (list '> "expects two real-valued numbers and returns #t if the first is greater than the second, #f otherwise"
	 'priml 2
	 (lambda (r aL a1 a2) (build-real-cmp ">" r aL a1 a2)))

   (list '/= "expects two real-valued numbers and returns #t if the first is unequal to the second, #f otherwise"
	 'priml 2
	 (lambda (r aL a1 a2) (build-real-cmp "!=" r aL a1 a2)))

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

   (list 'string-create "expects one non-negative integer N and optionally a fill character C, and returns a string composed of N copies of the character C; if no fill character is specified, 'X' is used"
	 'priml
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_INT || @1.v.iv < 0) {
	      >wile_exception("string-create", "@L", "input is not a non-negative integer");
	      >}
	      >@@.vt = LV_STRING;
	      >@@.origin = @1.origin;
	      >@@.v.str = LISP_ALLOC(char, 1 + @1.v.iv);
	      >memset(@@.v.str, 'X', @1.v.iv);
	      >@@.v.str[@1.v.iv] = '\0';
	      >HEREDOC
	      ))
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_INT || @1.v.iv < 0) {
	      >wile_exception("string-create", "@L", "first input is not a non-negative integer");
	      >}
	      >if (@2.vt != LV_CHAR || @2.v.chr == '\0') {
	      >wile_exception("string-create", "@L", "second input is not a valid character");
	      >}
	      >@@.vt = LV_STRING;
	      >@@.origin = @1.origin;
	      >@@.v.str = LISP_ALLOC(char, 1 + @1.v.iv);
	      >memset(@@.v.str, @2.v.chr, @1.v.iv);
	      >@@.v.str[@1.v.iv] = '\0';
	      >HEREDOC
	      )))

   (list 'string-downcase "expects one string argument and returns a newly-allocated lower-cased version of the input"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >@@ = LVI_STRING(@1.v.str);
	    >{
	    >char* sp = @@.v.str;
	    >while (*sp) {
	    >*sp = tolower(*sp);
	    >++sp;
	    >}
	    >}
	    >HEREDOC
	    )))

   (list 'string-upcase "expects one string argument and returns a newly-allocated upper-cased version of the input"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >@@ = LVI_STRING(@1.v.str);
	    >{
	    >char* sp = @@.v.str;
	    >while (*sp) {
	    >*sp = toupper(*sp);
	    >++sp;
	    >}
	    >}
	    >HEREDOC
	    )))

   (list 'string-copy
	 "expects a string, an optional start index, and an optional end index, and returns a newly-allocated copy of the substring from the start to but not including the end index; if the end index is not specified, it defaults to the end of the string, and if the start index is also not specified, it defaults to the beginning of the string"
	 'priml
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_STRING) {
	      >wile_exception("string-copy", "@L", "expects a string input");
	      >}
	      >@@ = LVI_STRING(@1.v.str);
	      >HEREDOC
	      ))
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_STRING || @2.vt != LV_INT) {
	      >wile_exception("string-copy", "@L", "expects a string and an integer input");
	      >}
	      >{
	      >size_t len = strlen(@1.v.str);
	      >if (@2.v.iv < 0 || (size_t) @2.v.iv >= len) {
	      >wile_exception("string-copy", "@L", "start index is out of range");
	      >}
	      >@@ = LVI_STRING(@1.v.str + @2.v.iv);
	      >}
	      >HEREDOC
	      ))
	 3 (lambda (r aL a1 a2 a3)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_STRING || @2.vt != LV_INT || @3.vt != LV_INT) {
	      >wile_exception("string-copy", "@L", "expects a string and two integer inputs");
	      >}
	      >{
	      >size_t len = strlen(@1.v.str);
	      >if (@2.v.iv < 0 || (size_t) @2.v.iv >= len) {
	      >wile_exception("string-copy", "@L", "start index is out of range");
	      >}
	      >if (@3.v.iv < @2.v.iv || (size_t) @3.v.iv > len) {
	      >wile_exception("string-copy", "@L", "end index is out of range");
	      >}
	      >@@.vt = LV_STRING;
	      >@@.origin = @1.origin;
	      >@@.v.str = LISP_ALLOC(char, 1 + @3.v.iv - @2.v.iv);
	      >memcpy(@@.v.str, @1.v.str + @2.v.iv, @3.v.iv - @2.v.iv);
	      >@@.v.str[@3.v.iv - @2.v.iv] = '\0';
	      >}
	      >HEREDOC
	      )))

   (list 'string-length "expects one string argument and returns its length"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_INT(strlen(@1.v.str));")))

   (list 'string-ref "expects one string and one integer index, and returns the character at that position in the string"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_STRING || @2.vt != LV_INT) {
	    >wile_exception("string-ref", "@L", "expects a string input");
	    >}
	    >if (@2.v.iv < 0 || (size_t) @2.v.iv >= strlen(@1.v.str)) {
	    >wile_exception("string-ref", "@L", "got bad index value");
	    >}
	    >@@ = LVI_CHAR(@1.v.str[@2.v.iv]);
	    >HEREDOC
	    )))

   (list 'string-find-first-char
	 "expects one string and one character, and returns the index of the left-most occurrence of that character in the string, or #f if the character does not occur in the string"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_STRING || @2.vt != LV_CHAR) {
	    >wile_exception("string-find-first-char", "@L", "expects a string and a character input");
	    >}
	    >{
	    >char* pos = strchr(@1.v.str, @2.v.chr);
	    >if (pos) {
	    >@@ = LVI_INT(pos - @1.v.str);
	    >} else {
	    >@@ = LVI_BOOL(false);
	    >}
	    >}
	    >HEREDOC
	    )))

   (list 'string-find-last-char
	 "expects one string and one character, and returns the index of the right-most occurrence of that character in the string, or #f if the character does not occur in the string"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_STRING || @2.vt != LV_CHAR) {
	    >wile_exception("string-find-last-char", "@L", "expects a string and a character input");
	    >}
	    >{
	    >char* pos = strrchr(@1.v.str, @2.v.chr);
	    >if (pos) {
	    >@@ = LVI_INT(pos - @1.v.str);
	    >} else {
	    >@@ = LVI_BOOL(false);
	    >}
	    >}
	    >HEREDOC
	    )))

   (list 'string-set!
	 "expects a string, an index, and a character, and sets that position of the string to the specified character"
	 'priml 3
	 (lambda (r aL a1 a2 a3)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_STRING || @2.vt != LV_INT || @3.vt != LV_CHAR) {
	    >wile_exception("string-set!", "@L", "expects a string, an integer, and a character");
	    >}
	    >if (@2.v.iv < 0 || (size_t) @2.v.iv >= strlen(@1.v.str)) {
	    >wile_exception("string-set!", "@L", "index is out of range");
	    >}
	    >@1.v.str[@2.v.iv] = @3.v.chr;
	    >@@ = @1;
	    >HEREDOC
	    )))

   (list 'string-reverse
	 "expects one string and returns the front-to-back reverse of it"
	 'prim 1 "wile_string_reverse")

   (list 'string-hash-32
	 "expects one string and returns the 32-bit FNV hash of it"
	 'prim 1 "wile_string_hash_32")
   (list 'string-ci-hash-32
	 "expects one string and returns the 32-bit FNV hash of the lowercase version of it"
	 'prim 1 "wile_string_ci_hash_32")
   (list 'string-hash-64
	 "expects one string and returns the 64-bit FNV hash of it"
	 'prim 1 "wile_string_hash_64")
   (list 'string-ci-hash-64
	 "expects one string and returns the 64-bit FNV hash of the lowercase version of it it"
	 'prim 1 "wile_string_ci_hash_64")
   (list 'bytevector-hash-64
	 "expects one bytevector and returns the 64-bit FNV hash of it"
	 'prim 1 "wile_bytevector_hash_64")

   (list 'char->integer
	 "expects one character and returns its integer code equivalent"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_INT((unsigned char) @1.v.chr);")))

   (list 'integer->char
	 "expects one integer and returns its character equivalent"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CHAR((unsigned char) @1.v.iv);")))

   (list 'char->string
	 "expects any number of characters and returns a string composed of those characters"
	 'prim -1 "wile_char2string")

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

   (list 'bits-shift
	 "expects two integers N and S and shifts N left by S bits (effectively right if S < 0); this is an arithmetic shift, negative N stay negative when right-shifted"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@2.v.iv >= 0) ? (@1.v.iv << @2.v.iv) : (@1.v.iv >> -@2.v.iv));")))

   (list 'bits-get
	 "expects two integers; the first one is treated as a bit-vector and the second one is treated as an index. returns the bit of I1 that is in position I2, (I1 & (1 << I2))"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@2.v.iv >= 0) ? (@1.v.iv & ( 1 << @2.v.iv)) : 0);")))

   (list 'bits-set
	 "expects two integers; the first one is treated as a bit-vector and the second one is treated as an index. sets the bit of I1 that is in position I2 to '1'"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@2.v.iv >= 0) ? (@1.v.iv | (1 << @2.v.iv)) : @1.v.iv);")))

   (list 'bits-set?
	 "expects two integers; the first one is treated as a bit-vector and the second one is treated as an index. returns the bit of I1 that is in position I2 as a boolean: 1 is #t and 0 is #f"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_BOOL((@2.v.iv >= 0) ? ((@1.v.iv & ( 1 << @2.v.iv)) != 0) : false);")))

   (list 'bits-clear
	 "expects two integers; the first one is treated as a bit-vector and the second one is treated as an index. sets the bit of I1 that is in position I2 to '0'"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_INT((@2.v.iv >= 0) ? (@1.v.iv & ~(1 << @2.v.iv)) : @1.v.iv);")))

   (list 'bits-flip
	 "expects two integers; the first one is treated as a bit-vector and the second one is treated as an index. flips the bit of I1 that is in position I2"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_INT((@2.v.iv >= 0) ? (@1.v.iv ^ (1 << @2.v.iv)) : @1.v.iv);")))

   (list 'sleep
	 "expects one real-valued number, which is used to delay or sleep the process for the given number of seconds"
	 'priml 1
	 (lambda (r aL a1)
	   (let ((aa (new-svar)))
	     (promote/real+check "sleep" aa aL a1)
	     (emit-code
	      "@@ = LVI_BOOL(usleep(1000000*@a.v.rv) == 0);"))))

   (list 'exit
	 "expects one integer, the normal exit status, which is reported to the calling process; this call does not return"
	 'prim 1
	 (lambda (r a1) (emit-code "exit(@1.v.iv);")))

   (list 'emergency-exit
	 "expects one integer, the emergency exit status, which is reported to the calling process; this call does not return"
	 'prim 1
	 (lambda (r a1) (emit-code "_exit(@1.v.iv);")))

   (list 'floor
	 "expects one real-valued number and returns its floor. if the input is integer- or rational-typed, the output is integer-typed; otherwise, it is real"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_REAL) {
	    >@@ = LVI_REAL(FLOOR(@1.v.rv));
	    >} else if (@1.vt == LV_RAT) {
	    >// TODO: keep this as an integer division & return quotient?
	    >@@ = LVI_INT(FLOOR(LV_RAT2REAL(@1)));
	    >} else if (@1.vt == LV_INT) {
	    >@@ = LVI_INT(@1.v.iv);
	    >} else {
	    >wile_exception("floor", "@L", "expects one real-valued argument");
	    >}
	    >HEREDOC
	    )))

   (list 'ceiling
	 "expects one real-valued number and returns its ceiling. if the input is integer- or rational-typed, the output is integer-typed; otherwise, it is real"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_REAL) {
	    >@@ = LVI_REAL(CEIL(@1.v.rv));
	    >} else if (@1.vt == LV_RAT) {
	    >// TODO: keep this as an integer division & return quotient?
	    >@@ = LVI_INT(CEIL(LV_RAT2REAL(@1)));
	    >} else if (@1.vt == LV_INT) {
	    >@@ = LVI_INT(@1.v.iv);
	    >} else {
	    >wile_exception("ceiling", "@L", "expects one real-valued argument");
	    >}
	    >HEREDOC
	    )))

   (list 'round
	 "expects one real-valued number and returns its value rounded to the nearest integer"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_REAL) {
	    >@@ = LVI_REAL(FLOOR(0.5 + @1.v.rv));
	    >} else if (@1.vt == LV_RAT) {
	    >// TODO: keep this as an integer division & return quotient?
	    >@@ = LVI_INT(FLOOR(0.5 + LV_RAT2REAL(@1)));
	    >} else if (@1.vt == LV_INT) {
	    >@@ = LVI_INT(@1.v.iv);
	    >} else {
	    >wile_exception("round", "@L", "expects one real-valued argument");
	    >}
	    >HEREDOC
	    )))

   ;;; TODO: deal with other types of inputs
   (list 'truncate
	 "expects one real number and returns its value rounded to the nearest integer toward zero"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_INT(@1.v.rv >= 0.0 ? FLOOR(@1.v.rv) : CEIL(@1.v.rv));")))

   (list 'sqrt
	 "expects one number and returns its square root. positive real-valued inputs return a real-typed result, others return a complex-typed result"
	 'priml 1
	 (lambda (r aL a1)
	   (let ((aa (new-svar)))
	     (promote/real aa a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@a.vt == LV_REAL) {
	      >if (@a.v.rv < 0.0) {
	      >@@ = LVI_CMPLX2(0.0, SQRT(-@a.v.rv));
	      >} else {
	      >@@ = LVI_REAL(SQRT(@a.v.rv));
	      >}
	      >} else if (@a.vt == LV_CMPLX) {
	      >@@ = LVI_CMPLX1(CSQRT(@a.v.cv));
	      >} else {
	      >wile_exception("sqrt", "@L", "expects one numeric argument");
	      >}
	      >HEREDOC
	      ))))

   (list 'cbrt
	 "expects one numeric argument and returns its cube root; real-valued inputs return real-valued results, and complex-valued inputs return complex-valued results"
	 'priml 1
	 (lambda (r aL a1)
	   (let ((aa (new-svar)))
	     (promote/real aa a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@a.vt == LV_REAL) {
	      >@@ = LVI_REAL(CBRT(@a.v.rv));
	      >} else if (@a.vt == LV_CMPLX) {
	      >@@ = LVI_CMPLX1(CPOW(@a.v.cv, 1.0/3.0));
	      >} else {
	      >wile_exception("cbrt", "@L", "expects one numeric argument");
	      >}
	      >HEREDOC
	      ))))

   (list 'exp
	 "expects one numeric argument and returns its exponential; real-valued results are return as real-typed, and complex-valued results are complex-typed"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "exp" "EXP" "CEXP" r aL a1)))

   (list 'log
	 "expects one numeric argument and returns its natural logarithm"
	 'priml 1
	 (lambda (r aL a1)
	   (let ((aa (new-svar)))
	     (promote/real aa a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@a.vt == LV_REAL) {
	      >if (@a.v.rv >= 0.0) {
	      >@@ = LVI_REAL(LOG(@a.v.rv));
	      >} else {
	      >@@ = LVI_CMPLX2(LOG(-@a.v.rv), PI_L);
	      >}
	      >} else if (@a.vt == LV_CMPLX) {
	      >@@ = LVI_CMPLX1(CLOG(@a.v.cv));
	      >} else {
	      >wile_exception("log", "@L", "expects one numeric argument");
	      >}
	      >HEREDOC
	      ))))

   (list 'sin
	 "expects one number and returns the sine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "sin" "SIN" "CSIN" r aL a1)))

   (list 'cos
	 "expects one number and returns the cosine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "cos" "COS" "CCOS" r aL a1)))

   (list 'tan
	 "expects one number and returns the tangent of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "tan" "TAN" "CTAN" r aL a1)))

   (list 'sinh
	 "expects one number and returns the hyperbolic sine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "sinh" "SINH" "CSINH" r aL a1)))

   (list 'cosh
	 "expects one number and returns the hyperbolic cosine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "cosh" "COSH" "CCOSH" r aL a1)))

   (list 'tanh
	 "expects one number and returns the hyperbolic tangent of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "tanh" "TANH" "CTANH" r aL a1)))

   (list 'asin
	 "expects one number and returns the arc-sine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "asin" "ASIN" "CASIN" r aL a1)))

   (list 'acos
	 "expects one number and returns the arc-cosine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "acos" "ACOS" "CACOS" r aL a1)))

   (list 'atan
	 "expects one or two real-valued numbers; if one, returns the arc-tangent of that number in the range -pi/2 to +pi/2; if two, the first is interpreted as the Y-coordinate and the second as the X-coordinate of a point, and the return value is the angle between the X-axis and the vector from the origin to the point, in the range -pi to +pi"
	 'priml
	 1 (lambda (r aL a1)
	     (build-special-math-rc "atan" "ATAN" "CATAN" r aL a1))
	 2 (lambda (r aL a1 a2)
	     (let ((aa (new-svar))
		   (ab (new-svar)))
	       (promote/real+check "atan" aa aL a1)
	       (promote/real+check "atan" ab aL a2)
	       (emit-code "@@ = LVI_REAL(ATAN2(@a.v.rv, @b.v.rv));"))))

   (list 'asinh
	 "expects one number and returns the hyperbolic arc-sine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "asinh" "ASINH" "CASINH" r aL a1)))

   (list 'acosh
	 "expects one number and returns the hyperbolic arc-cosine of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "acosh" "ACOSH" "CACOSH" r aL a1)))

   (list 'atanh
	 "expects one number and returns the hyperbolic arc-tangent of that number. real-valued inputs return real-typed results, complex inputs return complex results"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-rc "atanh" "ATANH" "CATANH" r aL a1)))

   (list 'erfc
	 "expects one real-valued argument and returns the complementary error function of that value"
	 'priml 1
	 (lambda (r aL a1)
	   (let ((aa (new-svar)))
	     (promote/real+check "erfc" aa aL a1)
	     (emit-code "@@ = LVI_REAL(ERFC(@a.v.rv));"))))

   (list 'ldexp "expects a real-valued argument X and an integer N and returns X*2^N"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL(LDEXP(@1.v.rv, @2.v.iv));")))

   (list 'frexp
	 "expects one real-valued argument and returns a 2-list which is decomposition of it into a normalized fraction in [0.5,1) and an exponent; 0, NaN, and Inf are special cases"
	 'prim 1
	 (lambda (r a1)
	   (let ((aa (new-svar)))
	     (emit-code
	      #<< HEREDOC
	      >{
	      >lval @a[2];
	      >int ex;
	      >if (ISFINITE(@1.v.rv)) {
	      >@a[0] = LVI_REAL(FREXP(@1.v.rv, &ex));
	      >} else {
	      >@a[0] = LVI_REAL(@1.v.rv);
	      >ex = 0;
	      >}
	      >@a[1] = LVI_INT(ex);
	      >@@ = wile_gen_list(2, @a, NULL);
	      >}
	      >HEREDOC
	      ))))

   (list 'fmod
	 "expects two real-valued arguments X and Y and returns M = X - N*Y for some integer N such that M has the same sign as X and magnitude less than the magnitude of Y"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code "@@ = LVI_REAL(FMOD(@1.v.rv, @2.v.rv));")))

   (list 'hypot
	 "expects two real-valued arguments and returns the square root of the sum of their squares"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar))
		 (ab (new-svar)))
	     (promote/real+check "hypot" aa aL a1)
	     (promote/real+check "hypot" ab aL a2)
	     (emit-code "@@ = LVI_REAL(HYPOT(@a.v.rv, @b.v.rv));"))))

   (list 'poly-chebyshev1
	 "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Chebyshev polynomial of the first kind at X"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (promote/real+check "poly-chebyshev1" aa aL a2)
	     (emit-code "@@ = LVI_REAL(pcheby1(@1.v.iv, @a.v.rv));"))))

   (list 'poly-chebyshev2
	 "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Chebyshev polynomial of the second kind at X"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (promote/real+check "poly-chebyshev2" aa aL a2)
	     (emit-code "@@ = LVI_REAL(pcheby2(@1.v.iv, @a.v.rv));"))))

   (list 'poly-hermite1
	 "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Hermite polynomial of the \"physicist\" flavor at X"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (promote/real+check "poly-hermite1" aa aL a2)
	     (emit-code "@@ = LVI_REAL(phermite1(@1.v.iv, @a.v.rv));"))))

   (list 'poly-hermite2
	 "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Hermite polynomial of the \"probabilist\" flavor at X"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (promote/real+check "poly-hermite2" aa aL a2)
	     (emit-code "@@ = LVI_REAL(phermite2(@1.v.iv, @a.v.rv));"))))

   (list 'poly-legendre
	 "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Legendre polynomial at X"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (promote/real+check "poly-legendre" aa aL a2)
	     (emit-code "@@ = LVI_REAL(plegendre(@1.v.iv, @a.v.rv));"))))

   (list 'poly-laguerre
	 "expects one non-negative integer N and one real-valued argument X and returns the value of the Nth-order Laguerre polynomial at X"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (promote/real+check "poly-laguerre" aa aL a2)
	     (emit-code "@@ = LVI_REAL(plaguerre(@1.v.iv, @a.v.rv));"))))

   (list 'bessel-j
	 "expects one integer N and one real-valued argument X and returns the value of the Nth-order Bessel function of the first kind at X"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (promote/real+check "bessel-j" aa aL a2)
	     (emit-code
	      #<< HEREDOC
	      >{
	      >int s, n;
	      >n = @1.v.iv;
	      >if (n >= 0) {
	      >s = 1;
	      >} else {
	      >n = -n;
	      >s = (n%%2 == 0) ? 1 : -1;
	      >}
	      >@@ = LVI_REAL(s*JN(n, @a.v.rv));
	      >}
	      >HEREDOC
	      ))))

   (list 'bessel-y
	 "expects one integer N and one positive real-valued argument X and returns the value of the Nth-order Bessel function of the second kind at X"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (promote/real+check "bessel-j" aa aL a2)
	     (emit-code
	      #<< HEREDOC
	      >{
	      >int s, n;
	      >n = @1.v.iv;
	      >if (n >= 0) {
	      >s = 1;
	      >} else {
	      >n = -n;
	      >s = (n%%2 == 0) ? 1 : -1;
	      >}
	      >@@ = LVI_REAL((@a.v.rv >= 0.0) ? s*YN(n, @a.v.rv) : REAL_NAN);
	      >}
	      >HEREDOC
	      ))))

   ;;; TODO: maybe stick this into a function, which already exists?
   ;;; This might be a bit too big to inline. On the other hand, it's
   ;;; not likely that there will be very many separate calls to AGM
   ;;; in a program.

   (list 'arithmetic-geometric-mean
	 "expects two numeric values and returns their arithmetic-geometric mean"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar))
		 (ab (new-svar)))
	     (promote/real aa a1)
	     (promote/real ab a2)
	     (emit-code
	      #<< HEREDOC
	      >{
	      >lisp_cmplx_t a, g, an;
	      >if (@a.vt == LV_REAL) {
	      >a = @a.v.rv;
	      >} else if (@a.vt == LV_CMPLX) {
	      >a = @a.v.cv;
	      >} else {
	      >wile_exception("arithmetic-geometric-mean", "@L", "expects numeric arguments");
	      >}
	      >if (@b.vt == LV_REAL) {
	      >g = @b.v.rv;
	      >} else if (@b.vt == LV_CMPLX) {
	      >g = @b.v.cv;
	      >} else {
	      >wile_exception("arithmetic-geometric-mean", "@L", "expects numeric arguments");
	      >}
	      >while (CABS(a - g) > REAL_EPSILON*(CABS(a) + CABS(g))*0.5) {
	      >an = (a + g)*0.5;
	      >g = CSQRT(a*g);
	      >a = an;
	      >}
	      >if (CIMAG(a) == 0.0) {
	      >@@ = LVI_REAL(CREAL(a));
	      >} else {
	      >@@ = LVI_CMPLX1(a);
	      >}
	      >}
	      >HEREDOC
	      ))))

   (list 'integer
	 "expects one real-valued number and returns its integer part as an integer-typed value"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_INT) {
	    >@@ = @1;
	    >} else if (@1.vt == LV_RAT) {
	    >@@ = LVI_INT(@1.v.irv.num/@1.v.irv.den);
	    >} else if (@1.vt == LV_REAL) {
	    >@@ = LVI_INT((@1.v.rv >= 0.0) ? FLOOR(@1.v.rv) : CEIL(@1.v.rv));
	    >} else {
	    >wile_exception("integer", "@L", "expects one real-valued argument");
	    >}
	    >HEREDOC
	    )))

   (list 'float
	 "expects one real-valued argument and returns it as a floating-point value"
	 'priml 1
	 (lambda (r aL a1) (promote/real+check "float" r aL a1)))

   (list 'expt
	 "expects two real-valued numbers A and B, and returns A raised to the power of B"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    "@@ = wile_expt(&@1, &@2, \"@L\");")))

   (list 'random-seed!
	 "expects one optional integer argument, and resets the random number generator using that value as a seed. if no argument is given, a default value is generated from the current time and process id"
	 'prim
	 0 (lambda (r)
	     (emit-code
	      #<< HEREDOC
	      >wile_rand_seed((time(NULL)) ^ (getpid() << 4));
	      >@@ = LVI_BOOL(true);
	      >HEREDOC
	      ))
	 1 (lambda (r a1)
	     (emit-code
	      #<< HEREDOC
	      >wile_rand_seed(@1.v.iv);
	      >@@ = LVI_BOOL(true);
	      >HEREDOC
	      )))

   (list 'random-uniform
	 "expects 0 or 2 real-valued arguments L1 and L2 and returns a uniformly distributed random variable in the range [L1,L2); if no arguments are given, L1 and L2 are assumed to be 0 and 1 respectively"
	 'priml
	 0 (lambda (r aL)
	     (emit-code "@@ = LVI_REAL(wile_rand_dbl());"))
	 2 (lambda (r aL a1 a2)
	     (let ((aa (new-svar))
		   (ab (new-svar)))
	       (promote/real+check "random-uniform" aa aL a1)
	       (promote/real+check "random-uniform" ab aL a2)
	       (emit-code
		"@@ = LVI_REAL(@a.v.rv + (@b.v.rv - @a.v.rv)*wile_rand_dbl());"))))

   (list 'random-exponential
	 "expects no arguments or one positive real-valued argument and returns an exponentially-distributed random variable with the specified rate parameter, or with rate 1 if no rate parameter is specified"
	 'priml
	 0 (lambda (r aL)
	     (emit-code "@@ = LVI_REAL(-LOG(1.0 -wile_rand_dbl()));"))
	 1 (lambda (r aL a1)
	     (let ((aa (new-svar)))
	       (promote/real+check "random-exponential" aa aL a1)
	       (emit-code
		#<< HEREDOC
		>if (@a.v.rv <= 0.0) {
		>wile_exception("random-exponential", "@L", "expects a positive rate");
		>}
		>@@ = LVI_REAL(-LOG(1.0 - wile_rand_dbl())/@a.v.rv);
		>HEREDOC
		))))

   (list 'random-poisson
	 "expects one positive real-valued argument and returns a Poisson-distributed random variable"
	 'priml 1
	 (lambda (r aL a1)
	   (let ((aa (new-svar)))
	     (promote/real+check "random-poisson" aa aL a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@a.v.rv <= 0.0) {
	      >wile_exception("random-poisson", "@L", "expects a positive rate");
	      >}
	      >{
	      >lisp_int_t k = 0;
	      >lisp_real_t l = EXP(-@a.v.rv), p = 1.0;
	      >do {
	      >++k;
	      >p *= wile_rand_dbl();
	      >} while (p > l);
	      >@@ = LVI_INT(k - 1);
	      >}
	      >HEREDOC
	      ))))

   (list 'random-normal-pair
	 "expects either 0 or 2 numeric arguments M and S, and returns a pair of random numbers drawn from a normal distribution with mean M and standard deviation S; if no arguments are given, M is assumed to be 0 and S is assumed to be 1"
	 'priml
	 0 (lambda (r aL)
	     (emit-code
	      "@@ = wile_rand_normal_pair(0.0, 1.0);"))
	 2 (lambda (r aL a1 a2)
	     (let ((aa (new-svar))
		   (ab (new-svar)))
	       (promote/real+check "random-normal-pair" aa aL a1)
	       (promote/real+check "random-normal-pair" ab aL a2)
	       (emit-code
		"@@ = wile_rand_normal_pair(@a.v.rv, @b.v.rv);"))))

   (list 'random-cauchy
	 "expects 0 or 2 real-valued arguments X0 and W and returns a Cauchy-distributed random variable; if no arguments are given, X0 and W are assumed to be 0 and 1 respectively"
	 'priml
	 0 (lambda (r aL)
	     (emit-code "@@ = LVI_REAL(TAN(PI_L*(wile_rand_dbl() - 0.5)));"))
	 2 (lambda (r aL a1 a2)
	     (let ((aa (new-svar))
		   (ab (new-svar)))
	       (promote/real+check "random-uniform" aa aL a1)
	       (promote/real+check "random-uniform" ab aL a2)
	       (emit-code "@@ = LVI_REAL(@a.v.rv + @b.v.rv*TAN(PI_L*(wile_rand_dbl() - 0.5)));"))))

   (list 'factorial
	 "expects one non-negative integer argument and returns the factorial of the input"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.v.iv < 0) {
	    >wile_exception("factorial", "@L", "expects a non-negative input");
	    >}
	    >{
	    >lisp_int_t i, f = 1;
	    >for (i = 1; i <= @1.v.iv; ++i) {
	    >f *= i;
	    >}
	    >@@ = LVI_INT(f);
	    >}
	    >HEREDOC
	    )))

   (list 'floor-quotient
	 "expects two integers N1 and N2 and returns their quotient Q_f = floor(N1/N2)"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t nq, nr;
	    >floor_qr(@1.v.iv, @2.v.iv, &nq, &nr, "@L");
	    >@@ = LVI_INT(nq);
	    >}
	    >HEREDOC
	    )))

   (list 'floor-remainder
	 "expects two integers N1 and N2 and returns the remainder R_f = N1 - N2*Q_f; see floor-quotient"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t nq, nr;
	    >floor_qr(@1.v.iv, @2.v.iv, &nq, &nr, "@L");
	    >@@ = LVI_INT(nr);
	    >}
	    >HEREDOC
	    )))

   (list 'truncate-quotient
	 "expects two integers N1 and N2 and returns their quotient Q_t = truncate(N1/N2)"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t nq, nr;
	    >trunc_qr(@1.v.iv, @2.v.iv, &nq, &nr, "@L");
	    >@@ = LVI_INT(nq);
	    >}
	    >HEREDOC
	    )))

   (list 'truncate-remainder
	 "expects two integers N1 and N2 and returns the remainder R_f = N1 - N2*Q_t; see truncate-quotient"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t nq, nr;
	    >trunc_qr(@1.v.iv, @2.v.iv, &nq, &nr, "@L");
	    >@@ = LVI_INT(nr);
	    >}
	    >HEREDOC
	    )))

   (list 'ceiling-quotient
	 "expects two integers N1 and N2 and returns their quotient Q_c = ceiling(N1/N2)"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t nq, nr;
	    >ceil_qr(@1.v.iv, @2.v.iv, &nq, &nr, "@L");
	    >@@ = LVI_INT(nq);
	    >}
	    >HEREDOC
	    )))

   (list 'ceiling-remainder
	 "expects two integers N1 and N2 and returns the remainder R_c = N1 - N2*Q_c; see ceiling-quotient"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >lisp_int_t nq, nr;
	    >ceil_qr(@1.v.iv, @2.v.iv, &nq, &nr, "@L");
	    >@@ = LVI_INT(nr);
	    >}
	    >HEREDOC
	    )))

   (list 'floor/
	 "expects two integers N1 and N2 and returns a two-element list containing their floor-quotient and floor-remainder"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (emit-code
	      #<< HEREDOC
	      >{
	      >lval @a[2];
	      >lisp_int_t nq, nr;
	      >floor_qr(@1.v.iv, @2.v.iv, &nq, &nr, "@L");
	      >@a[0] = LVI_INT(nq);
	      >@a[1] = LVI_INT(nr);
	      >@@ = wile_gen_list(2, @a, NULL);
	      >}
	      >HEREDOC
	      ))))

   (list 'truncate/
	 "expects two integers N1 and N2 and returns a two-element list containing their truncate-quotient and truncate-remainder"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (emit-code
	      #<< HEREDOC
	      >{
	      >lval @a[2];
	      >lisp_int_t nq, nr;
	      >trunc_qr(@1.v.iv, @2.v.iv, &nq, &nr, "@L");
	      >@a[0] = LVI_INT(nq);
	      >@a[1] = LVI_INT(nr);
	      >@@ = wile_gen_list(2, @a, NULL);
	      >}
	      >HEREDOC
	      ))))

   (list 'ceiling/
	 "expects two integers N1 and N2 and returns a two-element list containing their ceiling-quotient and ceiling-remainder"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (emit-code
	      #<< HEREDOC
	      >{
	      >lval @a[2];
	      >lisp_int_t nq, nr;
	      >ceil_qr(@1.v.iv, @2.v.iv, &nq, &nr, "@L");
	      >@a[0] = LVI_INT(nq);
	      >@a[1] = LVI_INT(nr);
	      >@@ = wile_gen_list(2, @a, NULL);
	      >}
	      >HEREDOC
	      ))))

   ;;; A few internal type-check and -conversion functions,
   ;;; to help with implementing numeric tower

   (list '_int?
	 "expects one value and returns #t if the type of that value is integer, #f otherwise; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_INT);")))

   (list '_rat?
	 "expects one value and returns #t if the type of that value is rational, #f otherwise; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_RAT);")))

   (list '_real?
	 "expects one value and returns #t if the type of that value is real, #f otherwise; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_REAL);")))

   (list '_cmplx?
	 "expects one value and returns #t if the type of that value is complex, #f otherwise; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_CMPLX);")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO: do we need these functions? from here...
;;; we need them for test-wile.scm...?

   (list '_int->rat_
	 "expects one integer-typed value and returns the value converted to type rational; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_RAT(@1.v.iv, 1);")))

   (list '_int->real_
	 "expects one integer-typed value and returns the value converted to type real; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_REAL((lisp_real_t) @1.v.iv);")))

   (list '_int->cmplx_
	 "expects one integer-typed value and returns the value converted to type complex; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CMPLX2((lisp_real_t) @1.v.iv, 0.0);")))

   (list '_rat->real_
	 "expects one rational-typed value and returns the value converted to type real; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_REAL(((lisp_real_t) @1.v.irv.num)/((lisp_real_t) @1.v.irv.den));")))

   (list '_rat->cmplx_
	 "expects one rational-typed value and returns the value converted to type complex; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CMPLX2(((lisp_real_t) @1.v.irv.num)/((lisp_real_t) @1.v.irv.den), 0.0);")))

   (list '_real->cmplx_
	 "expects one real-typed value and returns the value converted to type complex; mainly for internal use"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_CMPLX2(@1.v.rv, 0.0);")))

;;; ... to here?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (list 'is-regular-file?
	 "expects one string and returns #t if that is the name of a regular file, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >struct stat sb;
	    >@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISREG(sb.st_mode));
	    >}
	    >HEREDOC
	    )))

   (list 'is-directory?
	 "expects one string and returns #t if that is the name of a directory, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >struct stat sb;
	    >@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISDIR(sb.st_mode));
	    >}
	    >HEREDOC
	    )))

   (list 'is-char-device?
	 "expects one string and returns #t if that is the name of a character device, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >struct stat sb;
	    >@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISCHR(sb.st_mode));
	    >}
	    >HEREDOC
	    )))

   (list 'is-block-device?
	 "expects one string and returns #t if that is the name of a block device, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >struct stat sb;
	    >@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISBLK(sb.st_mode));
	    >}
	    >HEREDOC
	    )))

   (list 'is-named-pipe?
	 "expects one string and returns #t if that is the name of a named pipe, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >struct stat sb;
	    >@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISFIFO(sb.st_mode));
	    >}
	    >HEREDOC
	    )))

   (list 'is-symbolic-link?
	 "expects one string and returns #t if that is the name of a symbolic link, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >struct stat sb;
	    >@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISLNK(sb.st_mode));
	    >}
	    >HEREDOC
	    )))

   (list 'is-socket?
	 "expects one string and returns #t if that is the name of a socket, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >struct stat sb;
	    >@@ = LVI_BOOL(stat(@1.v.str, &sb) == 0 && S_ISSOCK(sb.st_mode));
	    >}
	    >HEREDOC
	    )))

   (list 'file-exists?
	 "expects one string and returns #t if that is the name of an existing file, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(access(@1.v.str, F_OK) == 0);")))

   (list 'file-readable?
	 "expects one string and returns #t if that is the name of an existing and readable file, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(access(@1.v.str, R_OK) == 0);")))

   (list 'file-writable?
	 "expects one string and returns #t if that is the name of an existing and writable file, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(access(@1.v.str, W_OK) == 0);")))

   (list 'file-executable?
	 "expects one string and returns #t if that is the name of an existing and executable file, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(access(@1.v.str, X_OK) == 0);")))

   (list 'create-directory
	 "expects a string and optionally an integer, and creates a directory with that name; the integer is the mode, which defaults to octal 0755 if not specified"
	 'prim
	 1 (lambda (r a1)
	     (emit-code
	      "@@ = LVI_BOOL(mkdir(@1.v.str, 0755) == 0);"))
	 2 (lambda (r a1 a2)
	     (emit-code
	      "@@ = LVI_BOOL(mkdir(@1.v.str, @2.v.iv) == 0);")))

   (list 'rename-file
	 "expects two strings: the first is the name of an existing file, and the second is the new name to which it will be renamed; returns #t if the call succeeded, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_BOOL(rename(@1.v.str, @2.v.str) == 0);")))

   (list 'remove-file
	 "expects one string, and removes (using unlink) a file of that name; returns #t if the call succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_BOOL(unlink(@1.v.str) == 0);")))

   (list 'remove-directory
	 "expects one string and removes a directory of that name; returns #t if the call succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_BOOL(rmdir(@1.v.str) == 0);")))

   (list 'change-root-directory
	 "expects one string and changes the process root to a directory of that name; returns #t if the call succeeded, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code
	    "@@ = LVI_BOOL(chroot(@1.v.str) == 0);")))

   (list 'change-file-owner
	 "expects one string or file-port and two integers or #f and changes the ownership or default group associated with that filename or file-port"
	 'priml 3
	 (lambda (r aL a1 a2 a3)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >uid_t uid;
	    >gid_t gid;
	    >if (@2.vt == LV_INT) {
	    >uid = @2.v.iv;
	    >} else if (@2.vt == LV_BOOL && @2.v.bv == false) {
	    >uid = -1;
	    >} else {
	    >wile_exception("change-file-owner", "@L", "expects a file name or port, a user id or #f, and a group id or #f");
	    >}
	    >if (@3.vt == LV_INT) {
	    >gid = @3.v.iv;
	    >} else if (@3.vt == LV_BOOL && @3.v.bv == false) {
	    >gid = -1;
	    >} else {
	    >wile_exception("change-file-owner", "@L", "expects a file name or port, a user id or #f, and a group id or #f");
	    >}
	    >if (@1.vt == LV_STRING) {
	    >@@ = LVI_BOOL(chown(@1.v.str, uid, gid) == 0);
	    >} else if (@1.vt == LV_FILE_PORT) {
	    >@@ = LVI_BOOL(fchown(fileno(@1.v.fp), uid, gid) == 0);
	    >} else {
	    >wile_exception("change-file-owner", "@L", "expects a file name or port, a user id or #f, and a group id or #f");
	    >}
	    >}
	    >HEREDOC
	    )))

   (list 'change-symbolic-link-owner
	 "expects one string and two integers or #f and changes the ownership or default group associated with that symbolic link or file name"
	 'priml 3
	 (lambda (r aL a1 a2 a3)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >uid_t uid;
	    >gid_t gid;
	    >if (@2.vt == LV_INT) {
	    >uid = @2.v.iv;
	    >} else if (@2.vt == LV_BOOL && @2.v.bv == false) {
	    >uid = -1;
	    >} else {
	    >wile_exception("change-symbolic-link-owner", "@L", "expects a file name, a user id or #f, and a group id or #f");
	    >}
	    >if (@3.vt == LV_INT) {
	    >gid = @3.v.iv;
	    >} else if (@3.vt == LV_BOOL && @3.v.bv == false) {
	    >gid = -1;
	    >} else {
	    >wile_exception("change-symbolic-link-owner", "@L", "expects a file name, a user id or #f, and a group id or #f");
	    >}
	    >if (@1.vt == LV_STRING) {
	    >@@ = LVI_BOOL(lchown(@1.v.str, uid, gid) == 0);
	    >} else {
	    >wile_exception("change-symbolic-link-owner", "@L", "expects a file name, a user id or #f, and a group id or #f");
	    >}
	    >}
	    >HEREDOC
	    )))

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
	    #<< HEREDOC
	    >{
	    >char* ev = getenv(@1.v.str);
	    >@@ = (ev ? LVI_STRING(ev) : LVI_BOOL(false));
	    >}
	    >HEREDOC
	    )))

   (list 'send-signal "expects two integers which are a process id and a signal number respectively, and sends the specified signal to the specified process. returns #t if the call succeeded, #f otherwise"
	 'prim 2
	 (lambda (r a1 a2)
	   (emit-code
	    "@@ = LVI_BOOL(kill(@1.v.iv, @2.v.iv) == 0);")))

   (list 'truncate-file
	 "expects a file port and optionally an integer position; truncates the file at the specified position, or at the current file position indicator if no position was specified. returns #t if the truncation was successful, #f otherwise"
	 'priml
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >{
	      >if (@1.vt != LV_FILE_PORT) {
	      >wile_exception("truncate-file", "@L", "expects a file port");
	      >}
	      >long int pos = ftell(@1.v.fp);
	      >int fd = fileno(@1.v.fp);
	      >@@ = LVI_BOOL(pos >= 0 && fd >= 0 && ftruncate(fd, pos) == 0);
	      >}
	      >HEREDOC
	      ))
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >if ((@1.vt != LV_FILE_PORT && @1.vt != LV_STRING) || @2.vt != LV_INT) {
	      >wile_exception("truncate-file", "@L", "expects a file name or port and an integer");
	      >}
	      >if (@1.vt == LV_FILE_PORT) {
	      >int fd = fileno(@1.v.fp);
	      >@@ = LVI_BOOL(@2.v.iv >= 0 && fd >= 0 && ftruncate(fd, @2.v.iv) == 0);
	      >} else {
	      >@@ = LVI_BOOL(@2.v.iv >= 0 && truncate(@1.v.str, @2.v.iv) == 0);
	      >}
	      >HEREDOC
	      )))

   (list 'cputime "returns a list containing two floating-point numbers which are the process user and system times, respectively, in seconds" 'prim 0 "wile_cputime")

   (list 'vector-create "expects one integer, the size of the vector to be created, and optionally a second argument which is used to fill all slots of the new vector; returns a new vector of the given size"
	 'priml
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >{
	      >size_t i, capa;
	      >if (@1.vt != LV_INT || @1.v.iv < 0) {
	      >wile_exception("vector-create", "@L", "expects a non-negative integer");
	      >}
	      >@@.vt = LV_VECTOR;
	      >@@.origin = @1.origin;
	      >capa = @1.v.iv;
	      >@@.v.vec.capa = capa;
	      >@@.v.vec.arr = LISP_ALLOC(lptr, (capa > 0 ? capa : 1));
	      >for (i = 0; i < capa; ++i) {
	      >@@.v.vec.arr[i] = NULL;
	      >}
	      >}
	      >HEREDOC
	      ))
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >{
	      >size_t i, capa;
	      >if (@1.vt != LV_INT || @1.v.iv < 0) {
	      >wile_exception("vector-create", "@L", "expects a non-negative integer");
	      >}
	      >@@.vt = LV_VECTOR;
	      >@@.origin = @1.origin;
	      >capa = @1.v.iv;
	      >@@.v.vec.capa = capa;
	      >@@.v.vec.arr = LISP_ALLOC(lptr, (capa > 0 ? capa : 1));
	      >for (i = 0; i < capa; ++i) {
	      >@@.v.vec.arr[i] = new_lv(LV_NIL);
	      >*(@@.v.vec.arr[i]) = @2;
	      >}
	      >}
	      >HEREDOC
	      )))

   (list 'vector-fill!
	 "expects one vector and one general value and fills all slots of the vector with that value"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >size_t i, capa;
	    >if (@1.vt != LV_VECTOR) {
	    >wile_exception("vector-fill!", "@L", "first input is not a vector");
	    >}
	    >capa = @1.v.vec.capa;
	    >for (i = 0; i < capa; ++i) {
	    >@1.v.vec.arr[i] = new_lv(LV_NIL);
	    >*(@1.v.vec.arr[i]) = @2;
	    >}
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   (list 'vector-length
	 "expects one vector and returns its length"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >if (@1.vt == LV_VECTOR) {
	    >@@ = LVI_INT(@1.v.vec.capa);
	    >} else if (@1.vt == LV_BVECTOR) {
	    >@@ = LVI_INT(@1.v.bvec.capa);
	    >} else {
	    >wile_exception("vector-length", "@L", "input is not a vector");
	    >}
	    >}
	    >HEREDOC
	    )))

   (list 'vector-ref
	 "expects one vector and one index, bounds-checks the index, and returns the value stored in the vector at that index"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >if (@1.vt != LV_VECTOR) {
	    >wile_exception("vector-ref", "@L", "input is not a vector");
	    >}
	    >if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.vec.capa) {
	    >wile_exception("vector-ref", "@L", "got bad index value");
	    >}
	    >@@ = @1.v.vec.arr[@2.v.iv] ? *(@1.v.vec.arr[@2.v.iv]) : LVI_NIL();
	    >}
	    >HEREDOC
	    )))

   (list 'vector-set!
	 "expects a vector, an index, and a value, bounds-checks the index, and saves the value in the vector at that index; modifies the vector in-place"
	 'priml 3
	 (lambda (r aL a1 a2 a3)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >if (@1.vt != LV_VECTOR) {
	    >wile_exception("vector-set!", "@L", "input is not a vector");
	    >}
	    >if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.vec.capa) {
	    >wile_exception("vector-set!", "@L", "got bad index value");
	    >}
	    >@1.v.vec.arr[@2.v.iv] = new_lv(LV_NIL);
	    >*(@1.v.vec.arr[@2.v.iv]) = @3;
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   (list 'vector-swap!
	 "expects one vector and two indices, bounds-checks the indices, and swaps the values stored in the vector at those two indices"
	 'priml 3
	 (lambda (r aL a1 a2 a3)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >if (@1.vt != LV_VECTOR) {
	    >wile_exception("vector-swap!", "@L", "input is not a vector");
	    >}
	    >if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.vec.capa || @3.vt != LV_INT || @3.v.iv < 0 || (size_t) @3.v.iv >= @1.v.vec.capa) {
	    >wile_exception("vector-swap!", "@L", "got bad index value");
	    >}
	    >lptr tmp = @1.v.vec.arr[@2.v.iv];
	    >@1.v.vec.arr[@2.v.iv] = @1.v.vec.arr[@3.v.iv];
	    >@1.v.vec.arr[@3.v.iv] = tmp;
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   (list 'vector-copy
	 "expects a vector, an optional start index, and an optional end index, and returns a newly-allocated copy of the subvector from the start to but not including the end index; if the end index is not specified, it defaults to the end of the vector, and if the start index is also not specified, it defaults to the beginning of the vector"
	 'priml
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_VECTOR) {
	      >wile_exception("vector-copy", "@L", "expects a vector input");
	      >} else {
	      >size_t i, capa;
	      >@@.vt = LV_VECTOR;
	      >@@.origin = @1.origin;
	      >capa = @1.v.vec.capa;
	      >@@.v.vec.capa = capa;
	      >@@.v.vec.arr = LISP_ALLOC(lptr, (capa > 0 ? capa : 1));
	      >for (i = 0; i < capa; ++i) {
	      >if (@1.v.vec.arr[i]) {
	      >@@.v.vec.arr[i] = new_lv(LV_NIL);
	      >*(@@.v.vec.arr[i]) = *(@1.v.vec.arr[i]);
	      >} else {
	      >@@.v.vec.arr[i] = NULL;
	      >}
	      >}
	      >}
	      >HEREDOC
	      ))
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_VECTOR || @2.vt != LV_INT) {
	      >wile_exception("vector-copy", "@L", "expects a vector and an integer input");
	      >} else if (@2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.vec.capa) {
	      >wile_exception("vector-copy", "@L", "start index is out of range");
	      >} else {
	      >size_t i, capa;
	      >@@.vt = LV_VECTOR;
	      >@@.origin = @1.origin;
	      >capa = @1.v.vec.capa - @2.v.iv;
	      >@@.v.vec.capa = capa;
	      >@@.v.vec.arr = LISP_ALLOC(lptr, (capa > 0 ? capa : 1));
	      >for (i = 0; i < capa; ++i) {
	      >if (@1.v.vec.arr[i + @2.v.iv]) {
	      >@@.v.vec.arr[i] = new_lv(LV_NIL);
	      >*(@@.v.vec.arr[i]) = *(@1.v.vec.arr[i + @2.v.iv]);
	      >} else {
	      >@@.v.vec.arr[i] = NULL;
	      >}
	      >}
	      >}
	      >HEREDOC
	      ))
	 3 (lambda (r aL a1 a2 a3)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_VECTOR || @2.vt != LV_INT || @3.vt != LV_INT) {
	      >wile_exception("vector-copy", "@L", "expects a vector and two integer inputs");
	      >} else if (@2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.vec.capa) {
	      >wile_exception("vector-copy", "@L", "start index is out of range");
	      >} else if (@3.v.iv < @2.v.iv || (size_t) @3.v.iv >= @1.v.vec.capa) {
	      >wile_exception("vector-copy", "@L", "end index is out of range");
	      >} else {
	      >size_t i, capa;
	      >@@.vt = LV_VECTOR;
	      >@@.origin = @1.origin;
	      >capa = @3.v.iv - @2.v.iv;
	      >@@.v.vec.capa = capa;
	      >@@.v.vec.arr = LISP_ALLOC(lptr, (capa > 0 ? capa : 1));
	      >for (i = 0; i < capa; ++i) {
	      >if (@1.v.vec.arr[i + @2.v.iv]) {
	      >@@.v.vec.arr[i] = new_lv(LV_NIL);
	      >*(@@.v.vec.arr[i]) = *(@1.v.vec.arr[i + @2.v.iv]);
	      >} else {
	      >@@.v.vec.arr[i] = NULL;
	      >}
	      >}
	      >}
	      >HEREDOC
	      )))

   (list 'bytevector-create
	 "expects one integer, the size of the bytevector to be created, and optionally a second argument, a char or small integer, which is used to fill all slots of the new bytevector; returns a new bytevector of the given size"
	 'priml
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >{
	      >size_t i, capa;
	      >if (@1.vt != LV_INT || @1.v.iv < 0) {
	      >wile_exception("bytevector-create", "@L", "expects a non-negative integer");
	      >}
	      >@@.vt = LV_BVECTOR;
	      >@@.origin = @1.origin;
	      >capa = @1.v.iv;
	      >@@.v.bvec.capa = capa;
	      >@@.v.bvec.arr = LISP_ALLOC(unsigned char, (capa > 0 ? capa : 1));
	      >for (i = 0; i < capa; ++i) {
	      >@@.v.bvec.arr[i] = 0;
	      >}
	      >}
	      >HEREDOC
	      ))
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >{
	      >size_t i, capa;
	      >if (@1.vt != LV_INT || @1.v.iv < 0) {
	      >wile_exception("bytevector-create", "@L", "expects a non-negative integer");
	      >}
	      >@@.vt = LV_BVECTOR;
	      >@@.origin = @1.origin;
	      >capa = @1.v.iv;
	      >@@.v.bvec.capa = capa;
	      >@@.v.bvec.arr = LISP_ALLOC(unsigned char, (capa > 0 ? capa : 1));
	      >if (@2.vt == LV_CHAR) {
	      >@@.v.bvec.arr[0] = @2.v.chr;
	      >} else if (@2.vt == LV_INT && @2.v.iv >= 0 && @2.v.iv < 256) {
	      >@@.v.bvec.arr[0] = @2.v.iv & 0xff;
	      >} else {
	      >wile_exception("bytevector-create", "@L", "got bad initializer");
	      >}
	      >for (i = 1; i < capa; ++i) {
	      >@@.v.bvec.arr[i] = @@.v.bvec.arr[0];
	      >}
	      >}
	      >HEREDOC
	      )))

   (list 'bytevector-length
	 "expects one bytevector and returns its length"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_BVECTOR) {
	    >wile_exception("bytevector-length", "@L", "input is not a bytevector");
	    >}
	    >@@ = LVI_INT(@1.v.bvec.capa);
	    >HEREDOC
	    )))

   (list 'bytevector-ref
	 "expects one bytevector and one index, bounds-checks the index, and returns the value stored in the bytevector at that index"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_BVECTOR) {
	    >wile_exception("bytevector-ref", "@L", "input is not a bytevector");
	    >}
	    >if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.bvec.capa) {
	    >wile_exception("bytevector-ref", "@L", "got bad index value");
	    >}
	    >@@ = LVI_INT(@1.v.bvec.arr[@2.v.iv]);
	    >HEREDOC
	    )))

   (list 'bytevector-set!
	 "expects a bytevector, an index, and a char or small integer, bounds-checks the index, and saves the char/int in the bytevector at that index; modifies the bytevector in-place"
	 'priml 3
	 (lambda (r aL a1 a2 a3)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_BVECTOR) {
	    >wile_exception("bytevector-set!", "@L", "input is not a bytevector");
	    >}
	    >if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.bvec.capa) {
	    >wile_exception("bytevector-set!", "@L", "got bad index value");
	    >}
	    >if (!(@3.vt == LV_CHAR || (@3.vt == LV_INT && @3.v.iv >= 0 && @3.v.iv < 256))) {
	    >wile_exception("bytevector-set!", "@L", "got bad input value");
	    >}
	    >@1.v.bvec.arr[@2.v.iv] = (@3.vt == LV_CHAR) ? @3.v.chr : (unsigned char) @3.v.iv;
	    >@@ = @1;
	    >HEREDOC
	    )))

   (list 'bytevector-swap!
	 "expects one bytevector and two indices, bounds-checks the indices, and swaps the values stored in the bytevector at those two indices"
	 'priml 3
	 (lambda (r aL a1 a2 a3)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_BVECTOR) {
	    >wile_exception("bytevector-swap!", "@L", "input is not a bytevector");
	    >}
	    >if (@2.vt != LV_INT || @2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.bvec.capa ||
	    >@3.vt != LV_INT || @3.v.iv < 0 || (size_t) @3.v.iv >= @1.v.bvec.capa) {
	    >wile_exception("bytevector-swap!", "@L", "got bad index value");
	    >}
	    >{
	    >unsigned char tmp = @1.v.bvec.arr[@2.v.iv];
	    >@1.v.bvec.arr[@2.v.iv] = @1.v.bvec.arr[@3.v.iv];
	    >@1.v.bvec.arr[@3.v.iv] = tmp;
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   (list 'bytevector->string
	 "expects one bytevector and returns a string containing the bytes stored in the bytevector. note that if any bytes are zero, it is not possible to access any part of the string that is located after the zero bytes"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_BVECTOR) {
	    >wile_exception("bytevector->string", "@L", "expects one bytevector argument");
	    >}
	    >@@.vt = LV_STRING;
	    >@@.origin = @1.origin;
	    >@@.v.str = LISP_ALLOC(char, 1 + @1.v.bvec.capa);
	    >memcpy(@@.v.str, @1.v.bvec.arr, @1.v.bvec.capa);
	    >@@.v.str[@1.v.bvec.capa] = 0;
	    >HEREDOC
	    )))

   (list 'bytevector-fill!
	 "expects one bytevector and one character or small integer and fills all slots of the bytevector with that value"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >{
	    >size_t i, capa;
	    >if (@1.vt != LV_BVECTOR) {
	    >wile_exception("bytevector-fill!", "@L", "first input is not a bytevector");
	    >}
	    >if (!(@2.vt == LV_CHAR || (@2.vt == LV_INT && @2.v.iv >= 0 && @2.v.iv < 256))) {
	    >wile_exception("bytevector-fill!", "@L", "got bad input value");
	    >}
	    >unsigned char pv = (@2.vt == LV_CHAR) ? @2.v.chr : (unsigned char) @2.v.iv;
	    >capa = @1.v.vec.capa;
	    >for (i = 0; i < capa; ++i) {
	    >@1.v.bvec.arr[i] = pv;
	    >}
	    >@@ = @1;
	    >}
	    >HEREDOC
	    )))

   (list 'bytevector-copy
	 "expects a bytevector, an optional start index, and an optional end index, and returns a newly-allocated copy of the sub-bytevector from the start to but not including the end index; if the end index is not specified, it defaults to the end of the bytevector, and if the start index is also not specified, it defaults to the beginning of the bytevector"
	 'priml
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_BVECTOR) {
	      >wile_exception("bytevector-copy", "@L", "expects a bytevector input");
	      >} else {
	      >size_t i, capa;
	      >@@.vt = LV_BVECTOR;
	      >@@.origin = @1.origin;
	      >capa = @1.v.bvec.capa;
	      >@@.v.bvec.capa = capa;
	      >@@.v.bvec.arr = LISP_ALLOC(unsigned char, (capa > 0 ? capa : 1));
	      >for (i = 0; i < capa; ++i) {
	      >@@.v.bvec.arr[i] = @1.v.bvec.arr[i];
	      >}
	      >}
	      >HEREDOC
	      ))
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_BVECTOR || @2.vt != LV_INT) {
	      >wile_exception("bytevector-copy", "@L", "expects a bytevector and an integer input");
	      >} else if (@2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.bvec.capa) {
	      >wile_exception("bytevector-copy", "@L", "start index is out of range");
	      >} else {
	      >size_t i, capa;
	      >@@.vt = LV_BVECTOR;
	      >@@.origin = @1.origin;
	      >capa = @1.v.bvec.capa - @2.v.iv;
	      >@@.v.bvec.capa = capa;
	      >@@.v.bvec.arr = LISP_ALLOC(unsigned char, (capa > 0 ? capa : 1));
	      >for (i = 0; i < capa; ++i) {
	      >@@.v.bvec.arr[i] = @1.v.bvec.arr[i + @2.v.iv];
	      >}
	      >}
	      >HEREDOC
	      ))
	 3 (lambda (r aL a1 a2 a3)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_BVECTOR || @2.vt != LV_INT || @3.vt != LV_INT) {
	      >wile_exception("bytevector-copy", "@L", "expects a bytevector and two integer inputs");
	      >} else if (@2.v.iv < 0 || (size_t) @2.v.iv >= @1.v.bvec.capa) {
	      >wile_exception("bytevector-copy", "@L", "start index is out of range");
	      >} else if (@3.v.iv < @2.v.iv || (size_t) @3.v.iv >= @1.v.bvec.capa) {
	      >wile_exception("bytevector-copy", "@L", "end index is out of range");
	      >} else {
	      >size_t i, capa;
	      >@@.vt = LV_BVECTOR;
	      >@@.origin = @1.origin;
	      >capa = @3.v.iv - @2.v.iv;
	      >@@.v.bvec.capa = capa;
	      >@@.v.bvec.arr = LISP_ALLOC(unsigned char, (capa > 0 ? capa : 1));
	      >for (i = 0; i < capa; ++i) {
	      >@@.v.bvec.arr[i] = @1.v.bvec.arr[i + @2.v.iv];
	      >}
	      >}
	      >HEREDOC
	      )))

   ;;; TODO: make a general version that allows for a comparison test

   (list 'bytevector-sort!
	 "expects a bytevector and sorts it in-place in ascending order; returns the bytevector"
	 'priml
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt != LV_BVECTOR) {
	      >wile_exception("bytevector-sort!", "@L", "expects a bytevector input");
	      >} else {
	      >size_t i, j, k, capa, hist[256];
	      >capa = @1.v.bvec.capa;
	      >for (i = 0; i < 256; ++i) {
	      >hist[i] = 0;
	      >}
	      >for (i = 0; i < capa; ++i) {
	      >hist[@1.v.bvec.arr[i]] += 1;
	      >}
	      >i = 0;
	      >for (j = 0; j < 256; ++j) {
	      >for (k = 0; k < hist[j]; ++k) {
	      >@1.v.bvec.arr[i++] = j;
	      >}
	      >}
	      >if (i != capa) {
	      >wile_exception("bytevector-sort!", "@L", "internal failure! size mismatch %%zu vs %%zu", i, capa);
	      >}
	      >@@ = @1;
	      >}
	      >HEREDOC
	      )))

   (list 'UTCtime
	 "returns a 9-element list (Y M D h m s dow doy dst?) corresponding to UTC time 'now'"
	 'prim 0 "wile_gmtime" 1 "wile_gmtime")

   (list 'localtime
	 "returns a 9-element list (Y M D h m s dow doy dst?) corresponding to local time 'now'"
	 'prim 0 "wile_localtime" 1 "wile_localtime")

   (list 'get-file-status
	 "expects one string, the name of an existing file, and returns a 13-element list of integers as returned by stat()"
	 'prim 1 "wile_filestat")

   (list 'get-symbolic-link-status
	 "expects one string, the name of an existing file or symbolic link, and returns a 13-element list of integers as returned by lstat()"
	 'prim 1 "wile_symlinkstat")

   (list 'parse-string
	 "expects one string, parses it into a list of s-expressions, and returns that list"
	 'prim 1 "wile_parse_string")

   (list 'parse-file
	 "expects one string, the name of an existing file, parses the contents of that file into a list of s-expressions, and returns that list"
	 'prim 1 "wile_parse_file")

   (list 'regex-match
	 "expects two strings: a regular expression and a string to match on. returns a 3-list of strings: the pre-match, match, and post-match pieces; if no match, returns #f"
	 'prim 2 "wile_regex_match")

   (list 'get-user-information
	 "expects no arguments or one integer or one string: if no arguments are given, returns user information for all users; if a string is given, it is assumed to be a user name; if an integer is given, it is assumed to be a user id. in either of these cases, the user information belonging to that user is returned"
	 'prim 0 "wile_getalluserinfo" 1 "wile_getuserinfo")

   (list 'get-group-information
	 "expects no arguments or one integer or one string: if no arguments are given, returns group information for all groups; if a string is given, it is assumed to be a group name; if an integer is given, it is assumed to be a group id. in either of these cases, the group information belonging to that group is returned"
	 'prim 0 "wile_getallgroupinfo" 1 "wile_getgroupinfo")

   (list 'read-directory
	 "expects a string argument which is the name of some directory (default if no argument is \".\" which is the current working directory) and returns a list of directory entries; each entry is a list of length 2 containing the name and inode for each directory entry"
	 'prim 0 "wile_read_directory" 1 "wile_read_directory")

   (list 'listen-on
	 "expects one integer argument in the range 0 to 65535, creates a TCP socket on that port, and sets up that socket to accept connections; returns the socket-port if successful, or #f if unsuccessful"
	 'prim 1 "wile_listen_port")
   (list 'accept
	 "expects one socket port argument and accepts a connection on that port; if successful, returns a 3-list consisting of the new connected port, the remote peer's IP address in text form, and the remote peer's port, otherwise #f"
	 'prim 1 "wile_accept_connection")
   (list 'connect-to
	 "expects a host name string (which may be in textual dotted-quad form) and a port number, and connects to the specified host and port; returns a socket port if successful, or #f if unsuccessful"
	 'prim 2 "wile_connect_to")

   (list 'raise
	 "expects any number of arguments, combines them into a list, and throws that list as an exception"
	 'priml -1
	 (lambda (r aL . as)
	   (apply build-basic-list r as)
	   (let ((a1 r)
		 (r #f))
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt == LV_PAIR && (@1.v.pair.cdr == NULL || @1.v.pair.cdr->vt == LV_NIL)) {
	      >@1 = (@1.v.pair.car ? *(@1.v.pair.car) : LVI_NIL());
	      >}
	      >cachalot->errval = new_lv(LV_NIL);
	      >*(cachalot->errval) = @1;
	      >cachalot->whence = "@L";
	      >longjmp(cachalot->cenv, 1);
	      >HEREDOC
	      ))))

   (list 'log-gamma "expects one complex-valued argument and returns the complex-valued log of the gamma function of that argument"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-cmplx "log-gamma" "logamma" r aL a1)))

   (list 'digamma "expects one complex-valued argument and returns the complex-valued logarithmic derivative of the gamma function of that argument"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-cmplx "digamma" "digamma" r aL a1)))

   (list 'elliptic-K "expects one complex-valued argument and returns the complex-valued complete elliptic integral K of that argument"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-cmplx "elliptic-K" "elliptic_k" r aL a1)))

   (list 'elliptic-E "expects one complex-valued argument and returns the complex-valued complete elliptic integral E of that argument"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-cmplx "elliptic-E" "elliptic_e" r aL a1)))

   (list 'cosine-integral "expects one positive real-valued argument and returns the cosine integral of that value"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-real "cosine-integral" "cosine_integral" r aL a1)))

   (list 'sine-integral "expects one real-valued argument and returns the sine integral of that value"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-real "sine-integral" "sine_integral" r aL a1)))

   (list 'lambert-W+ "expects one real-valued argument > -1/e and returns the positive real branch of the Lambert W function of that argument"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-real "lambert-W+" "lambert_wp_fn" r aL a1)))

   (list 'lambert-W- "expects one real-valued argument in (-1/e, 0) and returns the negative real branch of the Lambert W function of that argument"
	 'priml 1
	 (lambda (r aL a1)
	   (build-special-math-real "lambert-W-" "lambert_wn_fn" r aL a1)))

   (list 'lambert-W
	 "expects one complex argument and one integer, and returns the complex value of the Lambert W function of that argument on that branch"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (let ((aa (new-svar)))
	     (promote/real aa a2)
	     (emit-code
	      #<< HEREDOC
	      >{
	      >if (@1.vt != LV_INT) {
	      >wile_exception("lambert-W", "@L", "expects an integer and a complex number");
	      >}
	      >lisp_cmplx_t z;
	      >if (@a.vt == LV_REAL) {
	      >z = lambert_wc_fn(@1.v.iv, @a.v.rv);
	      >} else if (@a.vt == LV_CMPLX) {
	      >z = lambert_wc_fn(@1.v.iv, @a.v.cv);
	      >} else {
	      >wile_exception("lambert-W", "@L", "expects an integer and a complex number");
	      >}
	      >if (CIMAG(z) == 0.0) {
	      >@@ = LVI_REAL(CREAL(z));
	      >} else {
	      >@@ = LVI_CMPLX1(z);
	      >}
	      >}
	      >HEREDOC
	      ))))

   (list 'sqlite-version "returns the version of sqlite against which the program is linked" 'prim 0 "wile_sql_version")
   (list 'gc-version "returns the version of the Boehm GC against which the program is linked" 'prim 0 "wile_gc_version")

   (list 'sqlite-open
	 "expects an optional file name and an optional mode: if no arguments are specified, an in-memory database is opened read-write; if only a file name is specified, that is taken to be the name of an existing database which is opened read-only; if the mode is also specified, it must be one of 'read-only 'read-write or 'create. if 'read-only or 'read-write, the database must already exist; if 'create, it is created if it does not already exist, and it is opened read-write"
	 'priml
	 0 (lambda (r aL)
	     (emit-code "@@ = wile_sql_open(NULL, 1, \"@L\");"))
	 1 (lambda (r aL a1)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt == LV_STRING) {
	      >@@ = wile_sql_open(@1.v.str, 0, "@L");
	      >} else {
	      >wile_exception("sqlite-open", "@L", "expects a filename");
	      >}
	      >HEREDOC
	      ))
	 2 (lambda (r aL a1 a2)
	     (emit-code
	      #<< HEREDOC
	      >if (@1.vt == LV_STRING && @2.vt == LV_SYMBOL) {
	      >int mode;
	      >if (strcmp(@2.v.str, "read-only") == 0) {
	      >mode = 0;
	      >} else if (strcmp(@2.v.str, "read-write") == 0) {
	      >mode = 1;
	      >} else if (strcmp(@2.v.str, "create") == 0) {
	      >mode = 2;
	      >} else {
	      >wile_exception("sqlite-open", "@L", "unknown mode %%s", @2.v.str);
	      >}
	      >@@ = wile_sql_open(@1.v.str, mode, "@L");
	      >} else {
	      >wile_exception("sqlite-open", "@L", "expects a filename");
	      >}
	      >HEREDOC
	      )))

   (list 'sqlite-run
	 "expects an sqlite port and a string, and runs the string as a command. warning! do not use this with user-supplied strings, this is a security hole"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >#ifdef WILE_USES_SQLITE
	    >if (@1.vt == LV_SQLITE_PORT && @2.vt == LV_STRING) {
	    >@@ = wile_sql_run(@1.v.sqlite_conn, @2.v.str, "@L");
	    >} else {
	    >wile_exception("sqlite-run", "@L", "expects one sqlite-port and one string");
	    >}
	    >#else
	    >@@ = LVI_BOOL(false);
	    >#endif // WILE_USES_SQLITE
	    >HEREDOC
	    )))
   
   (list 'sqlite-statement-cleanup
	 "expects one sqlite-statement object and performs any necessary cleanup, rendering it no longer usable"
	 'prim 1 "wile_sql_stmt_clean")

   (list 'sqlite-statement-info
	 "expects one sqlite-statement object and returns a list of the parameters which can be bound to values"
	 'prim 1 "wile_sql_stmt_info")

   (list 'sqlite-statement-prepare
	 "expects an sqlite database handle and a string containing an SQL statement, and returns an sqlite-statement object"
	 'prim 2 "wile_sql_stmt_prep")

   (list 'sqlite-statement-run
	 "expects an sqlite-statement object with all parameters bound to values, runs it, and returns the results"
	 'prim 1 "wile_sql_stmt_run")

   (list 'sqlite-statement-bind
	 "expects an sqlite-statement object and any number of values and binds the values to the corresponding parameters"
	 'prim -2 "wile_sql_stmt_bind")

   (list 'cfft-good-n?
	 "expects one integer input, returns #t if that integer is a good size for a vector to be transformed by the vector-cfft! routine, #f otherwise. 'good size' means a number which is a power of only the prime factors (2,3,5,7,11)"
	 'prim 1 "wile_cfft_good_n")

   (list 'vector-cfft!
	 "expects an integer indicating the direction of the transform and a vector of a \"good\" length (see cfft-good-n?)  containing only numeric values, and computes the Fourier transform of that sequence in-place"
	 'prim 2 "wile_cfft")

   (list 'call/cc
	 "expects one argument which is a procedure of one argument, and returns... well... it's complicated"
	 'prim 1 "wile_call_cc")

   (list 'wile-basic-build-info
	 "expects no arguments and returns an integer which encodes various build flags"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(wile_binfo());")))

   (list 'implementation-name
	 "expects no arguments and returns a string which names this compiler"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_STRING(\"wile\");")))

   (list 'wile-os-name
	 "expects no arguments and returns a string which describes the OS"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = wile_os_name();")))

   (list 'wile-architecture-name
	 "expects no arguments and returns a string which describes the machine architecture"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = wile_arch_name();")))

   (list 'wile-config-file
	 "expects no arguments and returns the absolute path to the compiler config file if known, or #f"
	 'prim 0
	 (lambda (r)
	   (emit-code
	    #<< HEREDOC
	    >#ifdef WILE_CONFIG_FILE
	    >@@ = LVI_STRING(LISP_STRING(WILE_CONFIG_FILE));
	    >#else
	    >@@ = LVI_BOOL(false);
	    >#endif //  WILE_LIBDIR
	    >HEREDOC
	    )))

   (list 'stack-trace-minimal
	 "expects one optional output file port to which the stack trace is written; the default is stderr. returns nothing useful"
	 'prim
	 0 (lambda (r)
	     (emit-code
	      #<< HEREDOC
	      >wile_stack_trace_minimal(fileno(stderr));
	      >@@ = LVI_NIL();
	      >HEREDOC
	      ))
	 1 (lambda (r a1)
	     (emit-code
	      #<< HEREDOC
	      >wile_stack_trace_minimal(fileno((@1.vt == LV_FILE_PORT) ? @1.v.fp : stderr));
	      >@@ = LVI_NIL();
	      >HEREDOC
	      )))

   (list 'wile-xlat-fn-name
	 "for wile internal use only"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = wile_translate_fn_name(@1.v.str, 0);")))

   (list 'display-object-hook
	 "expects one symbol and one procedure of two arguments and records that procedure as the display method for objects of that type. this allows displaying objects with cycles"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_SYMBOL &&
	    >((@2.vt == LV_CLAMBDA && @2.v.clambda.arity == 2) ||
	    >(@2.vt == LV_ILAMBDA && @2.v.ilambda->arity == 2))) {
	    >@@ = wile_register_display_proc(@1.v.str, @2, "@L");
	    >} else {
	    >wile_exception("display-object-hook", "@L", "expects one symbol and one procedure of two arguments");
	    >}
	    >HEREDOC
	    )))

   (list 'get-errno
	 "expects no arguments and returns the current value of errno"
	 'prim 0
	 (lambda (r)
	   (emit-code "@@ = LVI_INT(errno);")))

   (list 'set-errno!
	 "expects one integer argument and sets errno to that value. returns nothing useful"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_INT) {
	    >errno = @1.v.iv;
	    >@@ = LVI_BOOL(true);
	    >} else {
	    >wile_exception("set-errno!", "@L", "expects one integer");
	    >}
	    >HEREDOC
	    )))

   (list 'token-source-line
	 "expects one argument and returns its location in source code"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_STRING(wile_decode_line_loc(wile_get_lisp_loc(&@1)));")))

   (list 'make-interpreted-procedure
	 "expects a list of arguments, an integer arity, a list of body expressions, an environment list, and a macro boolean"
	 'priml 5
	 (lambda (r aL a1 a2 a3 a4 a5)
	   (emit-code
	    #<< HEREDOC
	    >if ((@1.vt == LV_PAIR || @1.vt == LV_NIL) && @2.vt == LV_INT &&(@3.vt == LV_PAIR || @3.vt == LV_NIL) &&(@4.vt == LV_PAIR || @4.vt == LV_NIL) && @5.vt == LV_BOOL) {
	    >@@.vt = LV_ILAMBDA;
	    >@@.origin = @1.origin;
	    >@@.v.ilambda = LISP_ALLOC(lisp_ifunc_t, 1);
	    >@@.v.ilambda->args = @1;
	    >@@.v.ilambda->arity = @2.v.iv;
	    >@@.v.ilambda->body = @3;
	    >@@.v.ilambda->env = @4;
	    >@@.v.ilambda->macro = @5.v.bv;
	    >} else {
	    >wile_exception("make-interpreted-procedure", "@L", "expects a list of arguments, an integer arity, a list of body expressions, an environment list, and a macro boolean");
	    >}
	    >HEREDOC
	    )))

   (list 'set-interpreted-procedure-environment!
	 "expects an interpreted procedure and an environment list"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_ILAMBDA && @2.vt == LV_PAIR) {
	    >@1.v.ilambda->env = @2;
	    >@@ = @1;
	    >} else {
	    >wile_exception("set-interpreted-procedure-environment!", "@L", "expects an interpreted procedure and an environment list");
	    >}
	    >HEREDOC
	    )))

   (list 'set-interpreted-procedure-macro!
	 "expects an interpreted procedure and a boolean"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_ILAMBDA && @2.vt == LV_BOOL) {
	    >@1.v.ilambda->macro = @2.v.bv;
	    >@@ = @1;
	    >} else {
	    >wile_exception("set-interpreted-procedure-macro!", "@L", "expects an interpreted procedure and a boolean");
	    >}
	    >HEREDOC
	    )))

   (list 'get-interpreted-procedure-arguments
	 "expects an interpreted procedure and returns its arguments list"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_ILAMBDA) {
	    >@@ = @1.v.ilambda->args;
	    >} else {
	    >wile_exception("get-interpreted-procedure-arguments", "@L", "expects an interpreted procedure");
	    >}
	    >HEREDOC
	    )))

   (list 'get-interpreted-procedure-arity
	 "expects an interpreted procedure and returns its arity"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_ILAMBDA) {
	    >@@ = LVI_INT(@1.v.ilambda->arity);
	    >} else {
	    >wile_exception("get-interpreted-procedure-arity", "@L", "expects an interpreted procedure");
	    >}
	    >HEREDOC
	    )))

   (list 'get-interpreted-procedure-environment
	 "expects an interpreted procedure and returns its environment"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_ILAMBDA) {
	    >@@ = @1.v.ilambda->env;
	    >} else {
	    >wile_exception("get-interpreted-procedure-environment", "@L", "expects an interpreted procedure");
	    >}
	    >HEREDOC
	    )))

   (list 'get-interpreted-procedure-body
	 "expects an interpreted procedure and returns its body"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_ILAMBDA) {
	    >@@ = @1.v.ilambda->body;
	    >} else {
	    >wile_exception("get-interpreted-procedure-body", "@L", "expects an interpreted procedure");
	    >}
	    >HEREDOC
	    )))

   (list 'get-interpreted-procedure-macro
	 "expects an interpreted procedure and returns its macro-flag"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt == LV_ILAMBDA) {
	    >@@ = LVI_BOOL(@1.v.ilambda->macro);
	    >} else {
	    >wile_exception("get-interpreted-procedure-macro", "@L", "expects an interpreted procedure");
	    >}
	    >HEREDOC
	    )))

   (list 'get-file-eof
	 "expects one file port and returns #t if its EOF indicator is set, #f otherwise"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_FILE_PORT) {
	    >wile_exception("get-file-eof", "@L", "expects a file port");
	    >}
	    >@@ = LVI_BOOL(feof(@1.v.fp) != 0);
	    >HEREDOC
	    )))

   (list 'get-file-error
	 "expects one file port and returns #t if its error indicator is set, #f otherwise"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_FILE_PORT) {
	    >wile_exception("get-file-error", "@L", "expects a file port");
	    >}
	    >@@ = LVI_BOOL(ferror(@1.v.fp) != 0);
	    >HEREDOC
	    )))

   (list 'clear-file-error
	 "expects one file port and clears its EOF and error indicators; returns nothing useful"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_FILE_PORT) {
	    >wile_exception("get-file-error", "@L", "expects a file port");
	    >}
	    >clearerr(@1.v.fp);
	    >@@ = LVI_BOOL(true);
	    >HEREDOC
	    )))

   (list 'read-bytes
	 "expects a file port and a number of bytes to read and returns a bytevector containing the bytes read"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_FILE_PORT || @2.vt != LV_INT || @2.v.iv <= 0) {
	    >wile_exception("read-bytes", "@L", "expects a file port and number of bytes to read");
	    >}
	    >@@.vt = LV_BVECTOR;
	    >@@.origin = @1.origin;
	    >@@.v.bvec.capa = @2.v.iv;
	    >@@.v.bvec.arr = LISP_ALLOC(unsigned char, @2.v.iv);
	    >@@.v.bvec.capa = fread(@@.v.bvec.arr, 1, @2.v.iv, @1.v.fp);
	    >HEREDOC
	    )))

   (list 'write-bytes
	 "expects a file port and a bytevector to write and returns the number of bytes written"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_FILE_PORT || @2.vt != LV_BVECTOR) {
	    >wile_exception("write-bytes", "@L", "expects a file port and a bytevector to write");
	    >}
	    >@@ = LVI_INT(fwrite(@2.v.bvec.arr, 1, @2.v.bvec.capa, @1.v.fp));
	    >HEREDOC
	    )))

   (list 'sha-256-data?
	 "expects one argument and returns #t if that value is a SHA-256 data structure, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_SHA256_DATA && @1.v.sha256_info->is_256);")))

   (list 'sha-256
	 "expects one string or port and returns the SHA-256 hash of that string or the contents of that port as a 64-character string"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code "@@ = wile_sha256_wrap(true, @1, \"@L\");")))

   (list 'sha-256-init
	 "expects no arguments and returns an initialized SHA-256 hash data structure ready to accept data"
	 'prim 0
	 (lambda (r)
	   (emit-code
	    "@@ = wile_sha256_init(true);")))

   (list 'sha-256-update
	 "expects one SHA-256 hash data structure and one string or bytevector"
	 'prim 2 "wile_sha256_update")

   (list 'sha-256-finish
	 "expects one SHA-256 hash data structure and returns a string containing the final SHA-256 hash in hexadecimal"
	 'prim 1 "wile_sha256_finish")

   (list 'sha-224-data?
	 "expects one argument and returns #t if that value is a SHA-224 data structure, #f otherwise"
	 'prim 1
	 (lambda (r a1)
	   (emit-code "@@ = LVI_BOOL(@1.vt == LV_SHA256_DATA && !@1.v.sha256_info->is_256);")))

   (list 'sha-224
	 "expects one string or port and returns the SHA-224 hash of that string or the contents of that port as a 64-character string"
	 'priml 1
	 (lambda (r aL a1)
	   (emit-code "@@ = wile_sha256_wrap(false, @1, \"@L\");")))

   (list 'sha-224-init
	 "expects no arguments and returns an initialized SHA-224 hash data structure ready to accept data"
	 'prim 0
	 (lambda (r)
	   (emit-code
	    "@@ = wile_sha256_init(false);")))

   (list 'matrix-matrix-multiply
	 "expects vector mat1, integer nr1, integer nc1, bool tr1, vector mat2, integer nr2, integer nc2, bool tr2, and bool tr3, and returns vector mat3 of size nr1*nc2 stored either transposed or not depending on tr3"
	 'priml 9
	 (lambda (r aL a1 a2 a3 a4 a5 a6 a7 a8 a9)
	   (emit-code "@@ = wile_mat_mat_mul(@1, @2, @3, @4, @5, @6, @7, @8, @9, \"@L\");")))

   ;;; experimental, for regexp.scm to speed up DFA generation
   (list 'string-or! "expects two string inputs and ORs the characters of the second into the first"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_STRING || @2.vt != LV_STRING) {
	    >wile_exception("string-or!", "@L", "expects two strings");
	    >}
	    >{
	    >char *s1 = @1.v.str, *s2 = @2.v.str;
	    >while (*s1 && *s2) {
	    >*s1 |= *s2;
	    >++s1;
	    >++s2;
	    >}
	    >}
	    >@@ = @1;
	    >HEREDOC
	    )))

   (list 'string-and! "expects two string inputs and ANDs the characters of the second into the first"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_STRING || @2.vt != LV_STRING) {
	    >wile_exception("string-and!", "@L", "expects two strings");
	    >}
	    >{
	    >char *s1 = @1.v.str, *s2 = @2.v.str;
	    >while (*s1 && *s2) {
	    >*s1 &= *s2;
	    >++s1;
	    >++s2;
	    >}
	    >}
	    >@@ = @1;
	    >HEREDOC
	    )))

   (list 'string-xor! "expects two string inputs and XORs the characters of the second into the first"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_STRING || @2.vt != LV_STRING) {
	    >wile_exception("string-xor!", "@L", "expects two strings");
	    >}
	    >{
	    >char *s1 = @1.v.str, *s2 = @2.v.str;
	    >while (*s1 && *s2) {
	    >*s1 ^= *s2;
	    >++s1;
	    >++s2;
	    >}
	    >}
	    >@@ = @1;
	    >HEREDOC
	    )))

   (list 'bytevector-or! "expects two bytevector inputs and ORs the characters of the second into the first"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_BVECTOR || @2.vt != LV_BVECTOR) {
	    >wile_exception("bytevector-or!", "@L", "expects two bytevectors");
	    >}
	    >{
	    >size_t i, capa;
	    >capa = (@1.v.bvec.capa < @2.v.bvec.capa) ? @1.v.bvec.capa : @2.v.bvec.capa;
	    >for (i = 0; i < capa; ++i) {
	    >@1.v.bvec.arr[i] |= @2.v.bvec.arr[i];
	    >}
	    >}
	    >@@ = @1;
	    >HEREDOC
	    )))

   (list 'bytevector-and! "expects two bytevector inputs and ANDs the characters of the second into the first"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_BVECTOR || @2.vt != LV_BVECTOR) {
	    >wile_exception("bytevector-and!", "@L", "expects two bytevectors");
	    >}
	    >{
	    >size_t i, capa;
	    >capa = (@1.v.bvec.capa < @2.v.bvec.capa) ? @1.v.bvec.capa : @2.v.bvec.capa;
	    >for (i = 0; i < capa; ++i) {
	    >@1.v.bvec.arr[i] &= @2.v.bvec.arr[i];
	    >}
	    >}
	    >@@ = @1;
	    >HEREDOC
	    )))

   (list 'bytevector-xor! "expects two bytevector inputs and XORs the characters of the second into the first"
	 'priml 2
	 (lambda (r aL a1 a2)
	   (emit-code
	    #<< HEREDOC
	    >if (@1.vt != LV_BVECTOR || @2.vt != LV_BVECTOR) {
	    >wile_exception("bytevector-xor!", "@L", "expects two bytevectors");
	    >}
	    >{
	    >size_t i, capa;
	    >capa = (@1.v.bvec.capa < @2.v.bvec.capa) ? @1.v.bvec.capa : @2.v.bvec.capa;
	    >for (i = 0; i < capa; ++i) {
	    >@1.v.bvec.arr[i] ^= @2.v.bvec.arr[i];
	    >}
	    >}
	    >@@ = @1;
	    >HEREDOC
	    )))
   ))

;;; Add the stuff in wile-rtl2.scm to the primitives list

(define (prim-table)
  (let* ((wlds (get-config-val 'c-link-directories))
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
	  (fprintf stderr "cannot find interface file 'wrtl.sch', skipping\n")
	  prim-table-internal))))

(define (show-prims-table)
  (for-each (lambda (pe)
	      (write-string (string-pad-right (symbol->string (car pe))
					      #\space 40))
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
		       (prim-table))))

(define (lookup-doc-string p)
  (let loop ((ps (prim-table)))
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
	    (prim-table)))

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
