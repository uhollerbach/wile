;;; -*- mode: scheme; -*-

;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: GPLv3 or later, see file 'LICENSE' for details

(define global-verbose 0)		;;; how chirpy are we?

(define global-library #f)		;;; flag library mode or not

(define global-errors #f)		;;; check if errors seen during compile

(define (ERR fmt . args)
  (flush-port stderr)
  (raise (apply sprintf
		(string-append fmt " at " (token-source-line args))
		args)))

;;; several output ports for various bits of code wrangling
;;; these are no longer real file ports, but rather string-bag structs

(define global-out #f)
(define global-decl #f)
(define global-func #f)
(define global-code #f)

;;; track how many times each function gets called

(define global-funcs-used #f)

(define (increment-func-used-count fn)
  (let ((count (hash-table-ref global-funcs-used fn 0)))
    (hash-table-set! global-funcs-used fn (+ count 1))))

(def-struct string-bag bag)

(define (add-to-bag! bag str)
  (set-string-bag-bag! bag (cons str (get-string-bag-bag bag))))

(display-object-hook
 'string-bag
 (lambda (p port)
   (for-each (lambda (s) (write-string port s))
	     (list-reverse (get-string-bag-bag p)))))

;;; if doing profiling, this gets re-initialized to (), and then a list
;;; of function labels gets generated, each of which will correspond to
;;; a slot in a counts table.

(define global-profile #f)

;;; make every array of arguments be at least this big, so that we can
;;; re-use them for tail calls. 4 is the minimum needed to make the
;;; library compile without tail call generation failures, and compiling
;;; wile itself needs at least 7 (use -T option)
;;;
;;; if this is set to -1, warnings about too many args are suppressed.

(define global-tc-min-args 8)

(define (debug-show-env env n)
  (unless (null? env)
    (for-each (lambda (v)
		(write-string stderr #\tab)
		(display v stderr)
		(newline stderr))
	      (list-head env n))))

(define (debug-trace routine env tcall expr)
  (when (> global-verbose 1)
    (write-string
     stderr "enter routine " (symbol->string routine) " to compile\n    ")
    (display expr stderr)
    (when tcall
      (write-string stderr " [tail call]"))
    (write-string stderr "\n  from " (token-source-line expr))
    (newline stderr)
    (debug-show-env env 5)
    (flush-port stderr)))

(define (format-real val)
  (cond ((nan? val)
	 "REAL_NAN")
	((infinite? val)
	 (if (positive? val) "REAL_INF" "-REAL_INF"))
	(else
	 (let ((suffix "Q")	;;; "L" for long double, "" for double
	       (size -35))	;;; -22? for long double, -17 for double
	   (string-append (number->string val 10 size) suffix)))))

;;; Mangle a name, initially just a filename, into a form that's legal
;;; for the compiler: first part is the approximate name in a
;;; human-recognizable form, second part is the output of string-hash,
;;; so that possible collisions in the first part are avoided.

(define (name-mangle str1)
  (let* ((cs1 (string->list str1))
	 (cs2 (map (lambda (c)
		     (if (char-alphanumeric? c) (char-upcase c) #\_)) cs1))
	 (str2 (list->string cs2))
	 (str3 (number->string (string-hash str1))))
    (string-join-by "_" str2 str3)))

;;; Convert special characters into escape sequences so that the c compiler
;;; doesn't barf on malformed strings.

(define (escapify str)
  (let loop ((cs (list-reverse (string->list str)))
	     (acc ()))
    (if (null? cs)
	(list->string acc)
	(case (car cs)
	  ((#\alarm)		(loop (cdr cs) (cons #\\ (cons #\a acc))))
	  ((#\backspace)	(loop (cdr cs) (cons #\\ (cons #\b acc))))
	  ((#\escape)		(loop (cdr cs) (cons #\\ (cons #\e acc))))
	  ((#\page)		(loop (cdr cs) (cons #\\ (cons #\f acc))))
	  ((#\newline)		(loop (cdr cs) (cons #\\ (cons #\n acc))))
	  ((#\return)		(loop (cdr cs) (cons #\\ (cons #\r acc))))
	  ((#\tab)		(loop (cdr cs) (cons #\\ (cons #\t acc))))
	  ((#\vtab)		(loop (cdr cs) (cons #\\ (cons #\v acc))))
	  ((#\\)		(loop (cdr cs) (cons #\\ (cons #\\ acc))))
	  ((#\')		(loop (cdr cs) (cons #\\ (cons #\' acc))))
	  ((#\")		(loop (cdr cs) (cons #\\ (cons #\" acc))))
	  (else			(loop (cdr cs) (cons (car cs) acc)))))))

;;; Read all lines from a port after resetting the position back to the start;
;;; used for merging output files in merge-files

(define (read-all-lines port)
  (flush-port port)
  (set-file-position port 0 'start)
  (let loop ((acc ()))
    (let ((line (read-line port)))
      (if line
	  (loop (cons line acc))
	  (list-reverse acc)))))

(define (transfer-all-lines from-bag to-bag)
  (let ((fbag (get-string-bag-bag from-bag))
	(tbag (get-string-bag-bag to-bag)))
    (set-string-bag-bag! to-bag (list-append fbag tbag))
    (set-string-bag-bag! from-bag ())))

(defmacro (with-output dest . body)
  (let ((tport (gensym)))
    `(let ((,tport (make-string-bag ())))
       (fluid-let ((global-out ,tport))
	 ,@body
	 (when ,dest
	   (transfer-all-lines ,tport ,dest))))))

(define (args-ref name ix)
  (string-append name "[" (number->string ix) "]"))

;;; Generate fresh variables

(define new-svar-index 0)

(define (new-svar . tag)
  (set! new-svar-index (+ new-svar-index 1))
  (string-append (if (null? tag) "var" (symbol->string (car tag)))
		 "_"
		 (number->string new-svar-index)))

;;; don't care what these are, just that they can't be entered from a program

(define frame-sym (gensym))
(define global-sym (gensym))

;;; list of closure tables, see below, for building closures
(define global-closures ())

;;; list of mirror variables to make closures first-class
(define global-mirrors #f)

(defmacro (with-mirrors . body)
  `(fluid-let ((global-mirrors (hash-table-create string-hash string=?)))
     ,@body))

(define (lookup-frame env)
  (let loop ((env env))
    (if (null? env)
	#f
	(let ((entry (car env)))
	  (if (symbol=? (car entry) frame-sym)
	      (cdr entry)
	      (loop (cdr env)))))))

;;; The lowest-level routines that actually emit bits of code; everything
;;; should go through these emit-* routines.

;;; Emit a generic literal or formatted string

(define (emit-str str)
  (add-to-bag! global-out str))

(define (emit-fstr fstr . args)
  (emit-str (apply sprintf fstr args)))

;;; Emit a declaration of an lval or array of lvals

(define (emit-decl res . len)
  (emit-fstr "lval %s" res)
  (unless (null? len) (emit-fstr "[%d]" (max (car len) global-tc-min-args)))
  (emit-fstr ";\n"))

;;; This macro does limited interpolation of variables into strings:
;;; @@ gets replaced with the value of the variable 'r', @1 to @9
;;; get replaced with the values of the variables 'a1' to 'a9'.

;;; This macro can get pretty deep into recursive calls, which the
;;; interpreter doesn't properly tail-call yet; as a result, the
;;; compiler can crash. Activate this version when it's all proven.
;;; The version in wile-prims gets compiled, so it doesn't have this
;;; problem... compiled code is mostly properly tail-recursive.

;;; (defmacro (emit-code . strs)
;;;   (let ((xform
;;; 	 (let loop ((cs (string->list (apply string-join-by "\n" strs)))
;;; 		    (accs ())
;;; 		    (acca ()))
;;; 	   (cond ((null? cs)
;;; 		  (cons (list->string (list-reverse accs))
;;; 			(map (lambda (c)
;;; 			       (case c
;;; 				 ((#\@) 'r)
;;; 				 ((#\#) 'loc)
;;; 				 (else => (lambda (c)
;;; 					    (string->symbol
;;; 					     (char->string #\a c))))))
;;; 			     (list-reverse acca))))
;;; 		 ((char=? (car cs) #\@)
;;; 		  (loop (cddr cs)
;;; 			(cons #\s (cons #\% accs))
;;; 			(cons (cadr cs) acca)))
;;; 		 (else
;;; 		  (loop (cdr cs) (cons (car cs) accs) acca))))))
;;;     `(begin (when r (emit-decl r))
;;; 	    (emit-str (apply sprintf ,@xform ()))
;;; 	    (emit-str #\newline)
;;; 	    r)))

(define (emit-function-head fn-name top-label visible?
			    clos-name args-name info1 info2)
  ;;; emit a trailing ';' after the label because clang expects
  ;;; an expression after a label; gcc doesn't care
  (emit-fstr
   "\n// @@@ %v @@@ %s @@@ %s @@@\n%slval %s(lptr* %s, lptr %s, const char* cloc)\n{\n%s:;\n"
   info1 info2 fn-name (if visible? "" "static ")
   fn-name clos-name args-name top-label)
  (unless (hash-table-contains? global-funcs-used fn-name)
    (hash-table-set! global-funcs-used fn-name 0))
  (when global-profile
    (emit-fstr "wile_profile[%d].count += 1;\n" (list-length global-profile))
    (set! global-profile
	  (cons (sprintf "%v\\t%v" info2 info1)
		global-profile))))

(define (emit-function-call res fn closure args tcall frame call-loc)
  (when (and closure (not (null? global-closures)))
    (let* ((cl1 (car global-closures))
	   (clist (get-closure-table-clist cl1))
	   (cl-entry (assv closure clist)))
      (when cl-entry
	(set! closure (sprintf "(lptr*) %s" (cdr cl-entry))))))
  (when (string=? (string-copy call-loc 0 2) "./")
    (set! call-loc (string-copy call-loc 2)))
  (increment-func-used-count fn)
  (let ((call (sprintf "%s(%s, %s, \"%s\")"
		       fn (if closure closure "NULL") args call-loc)))
    (if tcall
	(if (and frame (string=? fn (car frame)))
	    (emit-fstr "goto %s;\n" (cadr frame))
	    (emit-fstr "TAIL_CALL %s;\n" call))
	(emit-fstr "%s = %s;\n" res call))))

(load-library "wile-prims.scm")

(define (sym2str s)
  (if (symbol? s) (symbol->string s) s))

(define global-file-head
  "// Generated by wile, the extremely stable scheming genius compiler\n\n#include \"wile-rtl1.h\"\n#include \"wile-rtl2.h\"\n\n// declarations\n\n")

(define (emit-function-tail vname)
  (emit-fstr "return %s;\n}\n" vname))

;;; closure table for a given function consists of:
;;; * name, the name by which the function itself refers to its closure table
;;; * size, the total number of aliases in both lists
;;; * vlist, the list of aliases for variables outside itself
;;; * clist, the list of aliases for other functions' closures

(def-struct closure-table name size vlist clist)

;;; Symbol table entries: lists in the following formats

;;; frame-sym fn-name top-label

;;; s-name 'c-var c-name
;;; s-name 'proc arity c-fn-name c-cl-name	;;; a scheme-proc
;;; s-name 'alias s-name

;;; a primitive
;;; s-name 'prim arity (lambda) arity (lambda) arity (lambda) ...

;;; a primitive that cares about location: source location
;;; gets added as a hidden first argument to the lambda(s)
;;; s-name 'priml arity (lambda) arity (lambda) arity (lambda) ...

;;; a macro
;;; s-name 'macro arity (lambda)

;;; arity meaning:
;;; 0 or more	-> a function which accepts exactly that many arguments
;;; negative	-> a function which accepts (- (- arity) 1) or more arguments
;;;		   -1 -> 0 or more, -2 -> 1 or more, -3 -> 2 or more, etc

(define (update-closure-table get-list set-list! format fc entry)
  (let loop ((cls global-closures)
	     (fc fc))
    (if (zero? fc)
	()
	(begin
	  (loop (cdr cls) (- fc 1))
	  (let* ((cl (car cls))
		 (name (get-closure-table-name cl))
		 (size (get-closure-table-size cl))
		 (the-list (get-list cl))
		 (cl-entry (assv entry the-list)))
	    (if cl-entry
		(cdr cl-entry)
		(let ((cl-entry (sprintf format name size)))
		  (set-closure-table-size! cl (+ size 1))
		  (set-list! cl (cons (cons entry cl-entry) the-list))
		  cl-entry)))))))

(define (update-closure-vlist fc entry)
  (update-closure-table get-closure-table-vlist
			set-closure-table-vlist! "V_CLOS(%s,%d)" fc entry))

(define (update-closure-clist fc entry)
  (update-closure-table get-closure-table-clist
			set-closure-table-clist! "P_CLOS(%s,%d)" fc entry))

(define (lookup-symbol full-env name)
  (let loop ((env full-env)
	     (name name)
	     (fc 0))
    (if (null? env)
	(ERR "unknown symbol '%s'" name)
	(let ((entry (car env)))
	  (cond ((symbol=? (car entry) global-sym)
		 (loop (cdr env) name -1))
		((symbol=? (car entry) frame-sym)
		 (loop (cdr env) name (+ fc 1)))
		((symbol=? (car entry) name)
		 (set! entry ((if (string? (cadr entry)) cddr cdr) entry))
		 (if (symbol=? (car entry) 'alias)
		     (loop (cdr env) (cadr entry) fc)
		     (cond ((and (positive? fc)
				 (symbol=? (car entry) 'c-var))
			    (list 0 'c-var (update-closure-vlist
					    fc (cadr entry))))
			   ((and (> fc 1)
				 (symbol=? (car entry) 'proc))
			    (update-closure-clist (- fc 1) (cadddr entry))
			    (cons fc entry)
			    )
			   (else
			    (cons fc entry)))))
		(else (loop (cdr env) name fc)))))))

;;; If the symbol is unknown, this version doesn't raise an exception
;;; but instead just returns #f

(define (lookup-symbol-nofail env name)
  (guard (err ((and (string? err) (regex-match "unknown symbol" err)) #f))
	 (lookup-symbol env name)))

;;; Make a new symbol table entry for a variable

(define (make-var-def s-name c-name)
  (list s-name 'c-var c-name))

;;; Make a new symbol table entry for a function

(define (make-fn-def s-name c-fn-name c-cl-name arity)
  (list s-name 'proc arity c-fn-name c-cl-name))

;;; Make a new symbol table entry for a macro

(define (make-macro-def s-name fn arity)
  (list s-name 'macro arity fn))

(define (reset-mirrors mt)
  (for-each (lambda (kv) (emit-fstr "*%s = %s;\n" (cdr kv) (car kv)))
	    (hash-table-entries mt))
  (hash-table-clear! mt))

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

;;; Create the code and symbol table entries for a top-level symbol:
;;; stdin, stdout, stderr, pi, Euler gamma number, command-line stuff

(define (special-decl var compile-type)
  (when (symbol=? compile-type 'extern)
    (emit-fstr "extern "))
  (unless (symbol=? compile-type 'singleton)
    (emit-decl var)))

(define (make-top-def compile-type s-var c-name c-def)
  (special-decl c-name compile-type)
  (cond ((symbol=? compile-type 'singleton)
	 (emit-fstr "%s = %s;\n" c-name c-def))
	((symbol=? compile-type 'main)
	 (with-output
	  global-code
	  (emit-fstr "%s = %s;\n" c-name c-def))))
  (make-var-def s-var c-name))

(define (make-top-env compile-type)
  (let ((mk-real (lambda (val)
		   (string-append "LVI_REAL(" (format-real val) ")"))))
    (foldr
     cons (prim-table)
     (list
      (let ((tmp "var_argv")
	    (fstr (string-join-by
		   "\n"
		   "WILE_CONFIG_SYM4();"
		   "if (argc <= 1) {"
		   "%s = LVI_NIL();"
		   "} else {"
		   "int i;"
		   "lval* sas = LISP_ALLOC(lval, argc-1);"
		   "for (i = 1; i < argc; ++i) {"
		   "sas[i-1] = LVI_STRING(argv[i]);"
		   "}"
		   "%s = wile_gen_list(argc - 1, sas, NULL);"
		   "}"
		   "")))
	(special-decl tmp compile-type)
	(cond ((symbol=? compile-type 'singleton)
	       (emit-fstr fstr tmp tmp))
	      ((symbol=? compile-type 'main)
	       (with-output
		global-code
		(emit-fstr fstr tmp tmp))))
	(make-var-def 'command-line-arguments tmp))
      (make-top-def compile-type 'command-name
		    "var_cmd_name" "LVI_STRING(argv[0])")
      (make-top-def compile-type 'stdin "var_stdin" "LVI_FPORT(stdin)")
      (make-top-def compile-type 'stdout "var_stdout" "LVI_FPORT(stdout)")
      (make-top-def compile-type 'stderr "var_stderr" "LVI_FPORT(stderr)")
      (make-top-def compile-type 'pi "var_pi" (mk-real pi))
      (make-top-def compile-type 'euler-gamma
		    "var_euler_gamma" (mk-real euler-gamma))
      (make-top-def compile-type 'default-show-sign
		    "var_show_sign" "LVI_BOOL(false)")
      (make-top-def compile-type 'default-int-base
		    "var_int_base" "LVI_INT(10)")
      (make-top-def compile-type 'default-float-base
		    "var_flt_base" "LVI_INT(10)")
      (make-top-def compile-type 'default-float-precision
		    "var_flt_precision" "LVI_INT(-15)")))))

;;; Make tail flags (#f #f ... #f #t) corresponding to a sequence (exprs)

(define (make-tail-flags exprs tcall)
  (if (null? exprs)
      ()
      (list-reverse (cons tcall (replicate #f (list-length (cdr exprs)))))))

;;; Turn a (defmacro ...) into an actual function that can be used

(define (declare-macro def)
  (debug-trace 'declare-macro () #f def)
  (let* ((s-name (caar def))
	 (args (cdar def))
	 (body (cdr def))
	 (argies (args-list args))
	 (arity (car argies))
	 (formals (cadr argies))
	 (stdenv (wile-environment-with-macros #f)))
    (unless (unique-symbols? formals)
      (ERR "malformed 'defmacro' args list '%v'" args))
    (make-macro-def s-name (make-iproc formals arity body stdenv #t) arity)))

;;; Define what is an immediate value

(define (is-immediate? val)
  (or (null? val)
      (char? val)
      (boolean? val)
      (integer? val)
      (rational? val)
      (real? val)
      (complex? val)
      (string? val)
      (symbol? val)
      (vector? val)
      (bytevector? val)
      (and (pair? val) (symbol? (car val)) (symbol=? (car val) 'quote))))

;;; Compile such immediate values

(define (compile-immediate-pair vname cur-env expr)
  (debug-trace 'compile-immediate-pair cur-env #f expr)
  (let* ((tmp (args-list expr))
	 (alen (car tmp))
	 (aal (abs alen))
	 (vals (cadr tmp)))
    (with-output
     global-decl
     (when global-library
       (emit-fstr "static bool do_init_%s = true;\n" vname))
     (emit-fstr "static lval %s;\n" vname))

    ;;; In no-main mode, the prior output is most likely a function which is
    ;;; getting processed via its own (with-output), so we need to write this
    ;;; output into that function's output, not anything global.

    (let ((wsave global-out))
      (with-output
       (if global-library wsave global-code)
       (when global-library
	 (emit-fstr "if (do_init_%s) " vname))
       (emit-fstr "{\n")
       (cond ((zero? alen)
	      (emit-fstr "%s = LVI_NIL();\n" vname))
	     ((= alen -1)		;;; this case should never happen...?
	      (fprintf stderr "hit impossible case? const list %v\n" expr)
	      (emit-fstr "%s = %s;\n"
			 vname (compile-immediate cur-env (car vals))))
	     (else
	      (let ((tmp (new-svar)))
		(emit-decl tmp aal)
		(for-each (lambda (i v)
			    (emit-fstr "%s[%d] = %s;\n"
				       tmp i (compile-immediate cur-env v)))
			  (upfrom 0 aal) vals)
		(if (positive? alen)
		    (emit-fstr "%s = wile_gen_list(%d, %s, NULL);\n"
			       vname alen tmp)
		    (emit-fstr "%s = wile_gen_list(%d, %s, %s + %d);\n"
			       vname (- aal 1) tmp tmp (- aal 1))))))
       (when global-library
	 (emit-fstr "do_init_%s = false;\n" vname))
       (emit-fstr "}\n")))
    vname))

(define (compile-immediate-vector vname cur-env expr is-bv?)
  (debug-trace 'compile-immediate-vector cur-env #f expr)
  (let ((vlen (vector-length expr)))
    (with-output
     global-decl
     (when global-library
       (emit-fstr "static bool do_init_%s = true;\n" vname))
     (emit-fstr "static lval %s;\n" vname))

    ;;; In no-main mode, the prior output is most likely a function which is
    ;;; getting processed via its own (with-output), so we need to write this
    ;;; output into that function's output, not anything global.

    (let ((wsave global-out))
      (with-output
       (if global-library wsave global-code)
       (when global-library
	 (emit-fstr "if (do_init_%s) " vname))
       (let* ((r #f)
	      (a1 vname)
	      (a2 (number->string vlen))
	      (a3 (if is-bv? "bvec" "vec"))
	      (a4 (if is-bv? "unsigned char" "lptr"))
	      (a5 (string-upcase a3)))
	 (emit-code
	  "{"
	  "@1.vt = LV_@5TOR;"
	  ;;; TODO: find better origin than this
	  "@1.origin = 0;"
	  "@1.v.@3.capa = @2;"
	  "@1.v.@3.arr = LISP_ALLOC(@4, @2);"
	  ""))
       (for-each (lambda (i v)
		   (let ((r #f)
			 (a1 vname)
			 (a2 (number->string i))
			 (a3 (compile-immediate cur-env v)))
		     (if is-bv?
			 (emit-code "@1.v.bvec.arr[@2] = @3.v.iv;")
			 (emit-code "@1.v.vec.arr[@2] = new_lv(LV_NIL);"
				    "*(@1.v.vec.arr[@2]) = @3;"))))
		 (upfrom 0 vlen)
		 ((if is-bv? bytevector->list vector->list) expr))
       (when global-library
	 (emit-fstr "do_init_%s = false;\n" vname))
       (emit-fstr "}\n")))
    vname))

(define (compile-immediate cur-env expr)
  (debug-trace 'compile-immediate cur-env #f expr)
  (when (and (pair? expr) (symbol? (car expr)) (symbol=? (car expr) 'quote))
    (set! expr (cadr expr)))
  (let ((r (new-svar)))
    (cond
     ((null? expr)
      (emit-code "@@ = LVI_NIL();"))
     ((char? expr)
      (let ((a1 (number->string (char->integer expr))))
	(emit-code "@@ = LVI_CHAR(@1);")))
     ((boolean? expr)
      (let ((a1 (if expr "true" "false")))
	(emit-code "@@ = LVI_BOOL(@1);")))
     ((integer? expr)
      (let ((a1 (number->string expr)))
	(emit-code "@@ = LVI_INT(@1);")))
     ((rational? expr)
      (let ((a1 (number->string (numerator expr)))
	    (a2 (number->string (denominator expr))))
	(emit-code
	 "@@.vt = LV_RAT;"
	 ;;; TODO: find better origin than this
	 "@@.origin = 0;"
	 "@@.v.irv.num = @1;"
	 "@@.v.irv.den = @2;")))
     ((real? expr)
      (let ((a1 (format-real expr)))
	(emit-code "@@ = LVI_REAL(@1);")))
     ((complex? expr)
      (let ((a1 (format-real (creal expr)))
	    (a2 (format-real (cimag expr))))
	(emit-code
	 "@@.vt = LV_CMPLX;"
	 ;;; TODO: find better origin than this
	 "@@.origin = 0;"
	 "@@.v.cv = (@1) + (@2)*I;")))
     ((string? expr)
      (let ((a1 (escapify expr)))
	(emit-code "@@ = LVI_STRING(\"@1\");")))
     ((symbol? expr)
      (let ((a1 expr))
	(emit-code "@@ = LVI_SYMBOL(\"@1\");")))
     ((or (vector? expr) (bytevector? expr))
      (compile-immediate-vector r cur-env expr (bytevector? expr)))
     ((pair? expr)
      (compile-immediate-pair r cur-env expr))
     (else (ERR "unknown immediate type '%v'" expr)))))

;;; TODO: comment more from here down

(define (check-dup-def def env)
  (when (pair? def)
    (set! def (car def)))
  (let ((prior (lookup-symbol-nofail env def)))
    (when prior
      (if (zero? (car prior))
	  (ERR "duplicate definition '%s'" def)
	  (fprintf stderr
		   "warning: definition '%s' at %v shadows prior definition\n"
		   def (token-source-line def))))))

(defmacro (transfer-namespace-sym pri pub)
  (let ((pval (gensym)))
    `(let ((,pval (lookup-symbol-nofail wk-env ,pri)))
       (if (and ,pval (zero? (car ,pval)))
	   (set! new-env (cons (cons ,pub (cdr ,pval)) new-env))
	   (ERR "symbol '%s' is not defined in namespace" ,pri)))))

;;; This is a teeny-tiny-trivial "environment" that's just for
;;; features in cond-expand: want to test for just 'wile instead of
;;; (string=? (implementation-name) "wile") for example

(def-struct bbox name value)

(define (wile-feature-environment)
  (map (lambda (p) (make-bbox (car p) (cdr p)))
       (list
	(cons 'wile #t))))

(define (do-cond-expand clauses)
  (let loop ((cs clauses))
    (cond ((null? cs)
	   '(#f))
	  ((guard (err (#t #f)) (eval (wile-feature-environment) (caar cs)))
	   (cdar cs))
	  (else
	   (loop (cdr cs))))))

;;; TODO: this needs to return the last value

(define (compile-special-begin-new cur-env tcall exprs)
  (debug-trace 'compile-special-begin-new cur-env tcall exprs)
  (if (null? exprs)
      (compile-immediate cur-env ())
      (let loop ((exprs exprs)
		 (cur-env cur-env)
		 (res #f))
	(cond ((null? exprs)
;;; TODO: include last value
;;;	       cur-env)
	       res)
	      ((not (pair? (car exprs)))
	       ;;; something?
	       )
	      ((eqv? 'begin (caar exprs))
	       (loop (list-append (cdar exprs) (cdr exprs)) cur-env res))
	      ((eqv? 'define (caar exprs))
	       (let* ((ec (cdar exprs))
		      (_ (check-dup-def (car ec) cur-env))
		      (c-fn (new-svar 'fn))
		      (fny? (pair? (car ec)))
		      (new-ee (if fny?
				  (list (caar ec)
					'proc
					(car (args-list (cdar ec)))
					c-fn "NULL")
				  (list (car ec) 'c-var (new-svar))))
		      (new-env (cons new-ee cur-env)))
		 (loop (cdr exprs) new-env
		       (compile-deffish new-env (car exprs)))))
	      ((eqv? 'define-primitive (caar exprs))
	       (let* ((ec (cdar exprs))
		      (_ (check-dup-def (caddr ec) cur-env))
		      (c-fn (car ec))
		      (fny? (pair? (caddr ec)))
		      (new-ee (if fny?
				  (list (caaddr ec)
					'proc
					(car (args-list (cdaddr ec)))
					c-fn "NULL")
				  (list (caddr ec) 'c-var c-fn)))
		      (new-env (cons new-ee cur-env)))
		 (loop (cdr exprs) new-env
		       (compile-deffish new-env (car exprs)))))
	      ((eqv? 'define-alias (caar exprs))
	       (check-dup-def (cdar exprs) cur-env)
	       (let* ((new-ee (list (cadar exprs) 'alias (caddar exprs)))
		      (new-env (cons new-ee cur-env)))
		 (loop (cdr exprs) new-env res)))
	      ((eqv? 'defmacro (caar exprs))
	       (let ((ec (cdar exprs)))
		 (check-dup-def (car ec) cur-env)
		 (if (symbol? (car ec))
		     (ERR "illegal macro symbol %s" (car ec))
		     (loop (cdr exprs)
			   (cons (declare-macro ec) cur-env) res))))
	      ((eqv? 'load (caar exprs))
	       (loop (list-append (parse-file (cadar exprs)) (cdr exprs))
		     cur-env res))
	      ((eqv? 'load-library (caar exprs))
	       (let* ((fname (cadar exprs))
		      (paths (get-config-val 'scheme-include-directories))
		      (filepath
		       (let loop2 ((ps paths))
			 (if (null? ps)
			     #f
			     (let* ((dir (car ps))
				    (fp (if (string=? dir ".")
					    fname
					    (string-join-by "/" dir fname))))
			       (if (file-exists? fp)
				   fp
				   (loop2 (cdr ps))))))))
		 (if filepath
		     (loop (list-append (parse-file filepath) (cdr exprs))
			   cur-env res)
		     (ERR "cannot find file %s" fname))))
	      ((eqv? 'namespace (caar exprs))
	       (let* ((fsym (gensym))
		      (frame (list fsym 'namespace))
		      (wk-env (list-take-while
			       (lambda (v)
				 (not (eqv? (car v) fsym)))
			       (loop (cddar exprs) (cons frame cur-env) res)))
		      (new-env cur-env))
		 (for-each (lambda (es)
			     (cond ((symbol? es)
				    (transfer-namespace-sym es es))
				   ((and (list-length=? 2 es)
					 (symbol? (car es))
					 (symbol? (cadr es)))
				    (transfer-namespace-sym
				     (car es) (cadr es)))
				   (else
				    (ERR "bad namespace interface %v" es))))
			   (cadar exprs))
		 (loop (cdr exprs) new-env res)))
	      ((eqv? 'cond-expand (caar exprs))
	       (loop (list-append (do-cond-expand (cdar exprs)) (cdr exprs))
		     cur-env res))
	      (else
	       (let ((ator (lookup-symbol-nofail cur-env (caar exprs))))
		 (if (and ator (eqv? 'macro (cadr ator)))
		     (let ((mex (apply-macro
				 (caar exprs) (cddr ator) (cdar exprs))))
		       (loop (cons mex (cdr exprs)) cur-env res))
		     (loop (cdr exprs) cur-env
			   (maybe-compile-expr cur-env #f (car exprs))))))))))

(define (compile-special-begin-old cur-env tcall exprs)
  (debug-trace 'compile-special-begin-old cur-env tcall exprs)
  (if (null? exprs)
      (compile-immediate cur-env ())
      (let* ((tmp1 (expand-toplevel cur-env exprs))
	     (wk-env (car tmp1))
	     (tmp2 (partition is-define? (cadr tmp1)))
	     (defs (car tmp2))
	     (exprs (cadr tmp2))
	     (do-decl (lambda (def)
			(let ((decl (declare-deffish wk-env def #f #f)))
			  (when (list? decl)
			    (set! wk-env (cons decl wk-env)))))))
	(for-each do-decl defs)
	(for-each (lambda (d) (compile-deffish wk-env d)) defs)
	(list-last (map (lambda (e t) (maybe-compile-expr wk-env t e))
			exprs (make-tail-flags exprs tcall))))))

;;; begin-new is still experimental and does not work
;;; completely correctly: DO NOT USE

(define (compile-special-begin cur-env tcall exprs)
  ((if (get-environment-variable "WILE_DO_NOT_USE_BEGIN_NEW")
       compile-special-begin-new
       compile-special-begin-old) cur-env tcall exprs))

(define (compile-special-if cur-env tcall exprs)
  (debug-trace 'compile-special-if cur-env tcall exprs)
  (if (list-length=? 3 exprs)
      (let ((res (new-svar)))
	(emit-decl res)
	;;; do not optimize these by combining them;
	;;; they sequence the operations properly
	(emit-fstr "if (LV_IS_FALSE(%s)) {\n"
		   (maybe-compile-expr cur-env #f (car exprs)))
	(with-mirrors
	 (let ((r2 (maybe-compile-expr cur-env tcall (caddr exprs))))
	   (reset-mirrors global-mirrors)
	   (emit-fstr "%s = %s;\n" res r2)))
	(emit-fstr "} else {\n")
	(with-mirrors
	 (let ((r2 (maybe-compile-expr cur-env tcall (cadr exprs))))
	   (reset-mirrors global-mirrors)
	   (emit-fstr "%s = %s;\n" res r2)))
	(emit-fstr "}\n")
	res)
      (ERR "malformed 'if' expression '%v'" exprs)))

(define (compile-special-do cur-env tcall exprs)
  (debug-trace 'compile-special-do cur-env tcall exprs)
  (let* ((res (new-svar))
	 (var-stuff (car exprs))
	 (vs (map car var-stuff))
	 (ups (map (lambda (v) (not (null? (cddr v)))) var-stuff))
	 (cs1 (map (lambda (v) (new-svar)) vs))
	 (cs2 (map (lambda (v) (new-svar)) vs)))
    (for-each (lambda (c1 c2 i f)
		(emit-decl c1)
		(when f (emit-decl c2))
		(emit-fstr "%s = %s;\n"
			   c1 (maybe-compile-expr cur-env #f i)))
	      cs1 cs2 (map cadr var-stuff) ups)
    (for-each (lambda (v c)
		(set! cur-env (cons (make-var-def v c) cur-env)))
	      vs cs1)
    (emit-decl res)
    (with-mirrors
     (for-each (lambda (v n)
		 (let ((r #f)
		       (a1 (new-svar))
		       (a2 v))
		   (emit-code "lptr @1 = new_lv(VT_UNINIT);"
			      "@1->v.pair.car = &(@2);")
		   (hash-table-set! global-mirrors v a1)))
	       cs1 vs)
     (emit-fstr "do {\n")
     (let ((tst (maybe-compile-expr cur-env #f (caadr exprs))))
       (emit-fstr "if (!LV_IS_FALSE(%s)) {\n" tst)
       (let ((tmp (compile-special-begin cur-env tcall (cdadr exprs))))
	 (emit-fstr "%s = %s;\nbreak;\n}\n" res tmp)))
     (let ((body (cddr exprs)))
       (unless (null? body)
	 (compile-special-begin cur-env #f body)))
     (for-each (lambda (c i f)
		 (when f
		   (emit-fstr "%s = %s;\n"
			      c (maybe-compile-expr cur-env #f (car i)))))
	       cs2 (map cddr var-stuff) ups)
     (for-each (lambda (c1 c2 f)
		 (when f (emit-fstr "%s = %s;\n" c1 c2)))
	       cs1 cs2 ups)
     (emit-fstr "} while (1);\n")
     (reset-mirrors global-mirrors))
    res))

(define (compile-special-andor init cur-env tcall exprs)
  (debug-trace 'compile-special-andor cur-env tcall exprs)
  (let ((res (new-svar))
	(flag (if (symbol=? init 'true) "" "!")))
    (emit-decl res)
    (emit-fstr "%s = LVI_BOOL(%s);\ndo {\n" res init)
    (for-each (lambda (e t)
		(emit-fstr "%s = %s;\n" res (maybe-compile-expr cur-env t e))
		(emit-fstr "if (%sLV_IS_FALSE(%s)) { break; }\n" flag res))
	      exprs (make-tail-flags exprs tcall))
    (emit-fstr "} while (0);\n")
    res))

(define (cond-fail-or-raise res raze loc)
  (if raze
      (let ((r #f)
	    (a1 raze)
	    (aL loc))
	(emit-code
	 "cachalot = @1.next;"
	 "cachalot->errval = @1.errval;"
	 "cachalot->whence = \"@L\";"
	 "fflush(NULL);"
	 "longjmp(cachalot->cenv, 1);"))
      (emit-fstr "%s = LVI_BOOL(false);\n" res)))

(define (compile-special-cond imp-raise cur-env tcall clauses loc)
  (debug-trace 'compile-special-cond cur-env tcall clauses)
  (if (null? clauses)
      (let ((res (new-svar)))
	(emit-decl res)
	(cond-fail-or-raise res imp-raise loc)
	res)
      (let* ((is-else? (lambda (t) (and (symbol? t) (symbol=? t 'else))))
	     (all-ts (list-reverse (map car clauses)))
	     (last-t (car all-ts))
	     (rest-t (cdr all-ts))
	     (res (new-svar))
	     (doit (lambda (clause)
		     ;;; do not optimize these by combining them;
		     ;;; they sequence the operations properly
		     (emit-fstr "if (!LV_IS_FALSE(%s)) {\n"
				(maybe-compile-expr cur-env #f (car clause)))
		     (with-mirrors
		      (emit-fstr "%s = %s;\n"
				 res (compile-special-begin cur-env tcall
							    (cdr clause)))
		      (reset-mirrors global-mirrors))
		     (emit-fstr "break;\n}\n"))))
	(emit-decl res)
	(emit-fstr "do {\n")
	(cond ((any-true? (map is-else? rest-t))
	       (ERR "malformed 'cond' expression '%v'" clauses))
	      ((is-else? last-t)
	       (for-each doit (list-untail clauses 1))
	       (with-mirrors
		(emit-fstr "%s = %s;\n" res
			   (compile-special-begin cur-env tcall
						  (cdr (list-last clauses))))
		(reset-mirrors global-mirrors)))
	      (else
	       (for-each doit clauses)
	       (cond-fail-or-raise res imp-raise loc)))
	(emit-fstr "} while (0);\n")
	res)))

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
      (ERR "malformed '%s' expression '%v'" llist)))

(define (compile-special-lettish advance-in-place cur-env tcall clauses)
  (debug-trace 'compile-special-lettish cur-env tcall clauses)
  (cond ((or (null? clauses) (null? (cdr clauses)))
	 (ERR "malformed 'let' '%v'" clauses))
	((and (list? clauses) (symbol? (car clauses)))		;;; named let
	 (check-let-bindings "named-let" (cdr clauses))
	 (let* ((s-name (car clauses))
		(nargs (list-length (cadr clauses)))
		(formals (map car (cadr clauses)))
		(args (map cadr (cadr clauses)))
		(c-fn-name (new-svar 'fn))
		(c-cl-name (new-svar))
		(lambda-env
		 (cons (make-fn-def s-name c-fn-name c-cl-name nargs) cur-env))
		(lambda-clauses (cons formals (cddr clauses))))
	   ;;; TODO: what about tail calls here?
	   (compile-special-lambda c-fn-name c-cl-name
				   lambda-env #f lambda-clauses)
	   (compile-expr lambda-env #f (cons s-name args))))
	(else					;;; let or let*
	 (check-let-bindings (if advance-in-place "let*" "let") clauses)
	 (let ((ndefs
		(map (lambda (def)
		       (let ((tmp (new-svar)))
			 (emit-decl tmp)
			 (emit-fstr "%s = %s;\n" tmp
				    (maybe-compile-expr
				     cur-env #f (cadr def)))
			 (let ((d (make-var-def (car def) tmp)))
			   (when advance-in-place
			     (set! cur-env (cons d cur-env)))
			   d)))
		     (car clauses))))
	   (unless advance-in-place
	     (for-each (lambda (d) (set! cur-env (cons d cur-env))) ndefs))
	   (compile-special-begin cur-env tcall (cdr clauses))))))

(define (compile-special-letrec cur-env tcall clauses)
  (debug-trace 'compile-special-letrec cur-env tcall clauses)
  (check-let-bindings "letrec" clauses)
  (let ((vars
	 (map (lambda (def)
		(if (symbol? (car def))
		    (let* ((tmp (new-svar))
			   (d (make-var-def (car def) tmp)))
;;; TODO: is this right? or should it write the declaration to global-decl?
		      (with-output global-func (emit-decl tmp))
		      (set! cur-env (cons d cur-env))
		      tmp)
		    (ERR "malformed 'letrec' definition '%s'" def)))
	      (car clauses))))
    (for-each (lambda (var def)
		(if (symbol? (car def))
		    (emit-fstr "%s = %s;\n" var
			       (maybe-compile-expr cur-env #f (cadr def)))
		    (ERR "malformed 'letrec' definition '%s'" def)))
	      vars (car clauses))
    (compile-special-begin cur-env tcall (cdr clauses))))

(define (compile-special-set! cur-env tcall clauses)
  (debug-trace 'compile-special-set! cur-env tcall clauses)
  (if (or (null? clauses) (null? (cdr clauses)) (not (symbol? (car clauses))))
      (ERR "malformed 'set!' expression '%v'" clauses)

      ;;; TODO: here, if we get a proc or prim, that means redefinition of
      ;;; said proc or prim. That would mean modifying the current environment
      ;;; from here on out, and that doesn't quite work in the current flow.
      ;;; Don't implement this yet, needs more thought and work.

      (let* ((tmp (lookup-symbol cur-env (car clauses)))
	     (fc (car tmp))
	     (si (cdr tmp)))
	(if (symbol=? (car si) 'c-var)
	    (emit-fstr "%s = %s;\n"
		       (cadr si) (maybe-compile-expr
				  cur-env #f (cadr clauses)))
	    (ERR "malformed 'set!' symbol '%s' does not resolve to a c-var"
		 (car clauses)))
	(cadr si))))

(define (build-closure)
  (unless (null? global-closures)
    (let* ((cl1 (car global-closures))
	   (name (get-closure-table-name cl1))
	   (size (get-closure-table-size cl1))
	   (vlist1 (get-closure-table-vlist cl1))
	   (clist1 (get-closure-table-clist cl1))
	   (cl2 (if (null? (cdr global-closures)) () (cadr global-closures)))
	   (vlist2 (if (null? cl2) () (get-closure-table-vlist cl2)))
	   (clist2 (if (null? cl2) () (get-closure-table-clist cl2))))
      (emit-fstr "MK_CLOS(%s,%d);\n" name size)
      (for-each (lambda (cle)
		  (let* ((name1 (cdr cle))
			 (var (car cle))
			 (tmp (assv var vlist2))
			 (name2 (if tmp (cdr tmp) var)))
		    (if global-mirrors
			(if (hash-table-contains? global-mirrors name2)
			    (emit-fstr
			     "P%s = %s;\n" (string-copy name1 1)
			     (hash-table-ref global-mirrors name2 ""))
			    (let ((r #f)
				  (a1 (new-svar))
				  (a2 name2)
				  (a3 (string-copy name1 1) a1))
			      (emit-code "lptr @1 = new_lv(VT_UNINIT);"
					 "@1->v.pair.car = &(@2);"
					 "P@3 = @1;")
			      (hash-table-set! global-mirrors name2 a1)))
			(emit-fstr "P%s = &(%s);\n"
				   (string-copy name1 1) name2))))
		vlist1)
      (for-each (lambda (cle)
		  (let* ((name1 (cdr cle))
			 (var (car cle))
			 (tmp (assv var clist2))
			 (name2 (if tmp (cdr tmp) var)))
		    ;;; TODO: how to handle closures here?
		    (emit-fstr "%s = (lptr) (%s);\n" name1 name2)))
		clist1))))

(define (compile-special-lambda c-fn-name c-cl-name cur-env tcall def)
  (debug-trace 'compile-special-lambda cur-env tcall def)
  (let* ((args (car def))
	 (argies (args-list (car def)))
	 (arity (car argies))
	 (sas (cadr argies))
	 (_ (unless (unique-symbols? sas)
	      (ERR "malformed 'lambda' args list '%v'" args)))
	 (tmp-fn (if c-fn-name c-fn-name (new-svar 'fn)))
	 (c-name (if c-cl-name c-cl-name (new-svar)))
	 (a-name (new-svar))
	 (top-label (new-svar 'lbl))
	 (cur-env (cons (list frame-sym tmp-fn top-label) cur-env))
	 (cas (map (lambda (i) (args-ref a-name i))
		   (upfrom 0 (list-length sas)))))
    (with-output
     global-decl
     (emit-fstr "static lval %s(lptr*, lptr, const char*);\n" tmp-fn))
    (fluid-let ((global-closures
		 (cons (make-closure-table c-name 0 () ()) global-closures)))
      (with-output
       global-func
       (with-mirrors
	(emit-function-head tmp-fn top-label #f c-name a-name
			    (sprintf "lambda %v" (car def))
			    (token-source-line def))
	(for-each (lambda (sa ca)
		    (set! cur-env (cons (make-var-def sa ca) cur-env)))
		  sas cas)
	(let ((res (compile-special-begin cur-env a-name (cdr def))))
	  (reset-mirrors global-mirrors)
	  (emit-function-tail res))
	(emit-fstr "// end of lambda %s\n" tmp-fn)))
      (build-closure)
      (increment-func-used-count tmp-fn)
      (string-append "LVI_PROC(" tmp-fn "," c-name ","
		     (number->string arity) ")"))))

(define (finish-one-case res cur-env tcall val clause)
  ;;; do not merge these emit-fstr; sequencing and scoping depends
  ;;; on them being separate
  (emit-fstr "{\n")
  (with-mirrors
   (emit-fstr
    "%s = %s;\n" res
    (if (and (list-length=? 3 clause)
	     (symbol? (cadr clause))
	     (symbol=? (cadr clause) '=>))
	(let ((proc (compile-expr cur-env tcall (caddr clause)))
	      (tmp (new-svar)))
	  (apply build-basic-list tmp (list val))
	  (compile-runtime-apply
	   (new-svar) (token-source-line clause) proc tmp))
	(compile-special-begin cur-env tcall (cdr clause))))
   (reset-mirrors global-mirrors))
  (emit-fstr "break;\n}\n"))

(define (do-one-case1 res to-int cur-env tcall val clause)
  (debug-trace 'do-one-case1 cur-env tcall clause)
  ;;; do not merge these emit-fstr; sequencing and scoping depends
  ;;; on them being separate
  (if (and (symbol? (car clause)) (symbol=? (car clause) 'else))
      (emit-fstr "default:\n")
      (for-each (lambda (c) (emit-fstr "case %d:\n" (to-int c)))
		(car clause)))
  (finish-one-case res cur-env tcall val clause))

(define (do-compile-case1 type member to-int def? cur-env tcall clauses)
  (debug-trace 'do-compile-case1 cur-env tcall clauses)
  (let ((loc (token-source-line (car clauses)))
	(val (maybe-compile-expr cur-env #f (car clauses)))
	(res (new-svar)))
    (emit-decl res)
    (emit-fstr "if (%s.vt != %s) {\nwile_exception(\"case\", \"%s\", \"case-value type does not match case type\");\n}\nswitch (%s%s) {\n" val type loc val member)
    (for-each (lambda (c) (do-one-case1 res to-int cur-env tcall val c))
	      (cdr clauses))
    (unless def?
      (emit-fstr "default:\n%s = LVI_BOOL(false);\n" res))
    (emit-fstr "}\n")
    res))

(define (do-one-case2 res to-str cur-env tcall val vm clause)
  (debug-trace 'do-one-case2 cur-env tcall clause)
  ;;; do not merge these emit-fstr; sequencing and scoping depends
  ;;; on them being separate
  (unless (and (symbol? (car clause)) (symbol=? (car clause) 'else))
    (emit-fstr "if (%v) "
	       (string-join-by
		" ||\n    "
		(map (lambda (s)
		       (string-append
			"(strcmp(" vm ", \"" (to-str s) "\") == 0)"))
		     (car clause)))))
  (finish-one-case res cur-env tcall val clause))

(define (do-compile-case2 type member to-str def? cur-env tcall clauses)
  (debug-trace 'do-compile-case2 cur-env tcall clauses)
  (let* ((loc (token-source-line (car clauses)))
	 (val (maybe-compile-expr cur-env #f (car clauses)))
	 (vm (string-append val member))
	 (res (new-svar)))
    (emit-decl res)
    (emit-fstr "if (%s.vt != %s) {\nwile_exception(\"case\", \"%s\", \"case-value type does not match case type\");\n}\ndo {\n" val type loc)
    (for-each (lambda (c) (do-one-case2 res to-str cur-env tcall val vm c))
	      (cdr clauses))
    (unless def?
      (emit-fstr "%s = LVI_BOOL(false);\n" res))
    (emit-fstr "} while (0);\n")
    res))

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (compile-special-case cur-env tcall clauses)
  (debug-trace 'compile-special-case cur-env tcall clauses)
  (let* ((is-else? (lambda (t) (and (symbol? t) (symbol=? t 'else))))
	 (all-cs (map car (cdr clauses)))
	 (last-c (list-last all-cs))
	 (rest-cs (list-untail all-cs 1))
	 (default? (is-else? last-c))
	 (typed-cs (flatten (if default? rest-cs all-cs)))
	 (check-unique (lambda (lt? eq? vals)
			 (let loop ((vs (list-sort lt? vals)))
			   (cond ((or (null? vs) (null? (cdr vs))) #t)
				 ((eq? (car vs) (cadr vs)) #f)
				 (else (loop (cdr vs))))))))
    (when (any-true? (map is-else? rest-cs))
      (ERR "malformed 'case' expression '%v'" clauses))
    (cond ((all-true? (map char? typed-cs))
	   (if (check-unique char<? char=? typed-cs)
	       (do-compile-case1 "LV_CHAR" ".v.chr" char->integer
				 default? cur-env tcall clauses)
	       (ERR "duplicated cases in 'case' expression '%v'" clauses)))
	  ((all-true? (map integer? typed-cs))
	   (if (check-unique < = typed-cs)
	       (do-compile-case1 "LV_INT" ".v.iv" (lambda (x) x)
				 default? cur-env tcall clauses)
	       (ERR "duplicated cases in 'case' expression '%v'" clauses)))
	  ((all-true? (map string? typed-cs))
	   (if (check-unique string<? string=? typed-cs)
	       (do-compile-case2 "LV_STRING" ".v.str" (lambda (x) x)
				 default? cur-env tcall clauses)
	       (ERR "duplicated cases in 'case' expression '%v'" clauses)))
	  ((all-true? (map symbol? typed-cs))
	   (if (check-unique symbol<? symbol=? typed-cs)
	       (do-compile-case2 "LV_SYMBOL" ".v.str" symbol->string
				 default? cur-env tcall clauses)
	       (ERR "duplicated cases in 'case' expression '%v'" clauses)))
	  ;;; TODO: possibly also support boolean
	  (else (ERR "mixed types are not supported in 'case' expression '%v'" clauses)))))

;;; Note: the stuff near provide-loc is an EXTENSION:
;;;
;;; the standard form catches an exception and provides it in err
;;;
;;;   (guard (err (#t (printf "catched exception %v\n" err)))
;;;          (raise "hurrrling naouw"))
;;;
;;; the extended form catches an exception and provides it in err,
;;  and provides some kind of information about the origin in loc
;;;
;;;   (guard (err loc (#t (printf "cotted exception %v from %v\n" err loc)))
;;;          (raise "Imma throw something now"))

(define (compile-special-guard cur-env tcall clauses)
  (debug-trace 'compile-special-guard cur-env tcall clauses)
  (if (and (pair? clauses)
	   (pair? (car clauses))
	   (symbol? (caar clauses)))
      (let* ((exc (car clauses))
	     (res (new-svar))
	     (errv (new-svar))
	     (provide-loc (symbol? (cadar clauses)))
	     (locv #f)
	     (ecv (new-svar))
	     (new-env (cons (make-var-def (caar clauses) errv) cur-env)))
	(when provide-loc
	  (set! locv (new-svar))
	  (set! new-env (cons (make-var-def (cadar clauses) locv) new-env)))
	;;; do not optimize these by combining them;
	;;; they sequence the operations properly
	(let ((r res)
	      (a1 ecv))
	  (emit-code
	   "{"
	   "struct lisp_escape_info @1;"
	   "@1.errval = NULL;"
	   "@1.next = cachalot;"
	   "cachalot = &@1;"
	   "if (setjmp(@1.cenv) == 0) {"))
	(emit-fstr "%s = %s;\ncachalot = %s.next;\n} else {\n"
		   res (compile-special-begin cur-env #f (cdr clauses)) ecv)
	(emit-decl errv)
	(emit-fstr "%s = (%s.errval ? *(%s.errval) : LVI_NIL());\n"
		   errv ecv ecv)
	(when provide-loc
	  ;;; the commented-out mention of the variable prevents
	  ;;; the not-used optimizer from turning this into
	  ;;;     (void) (cachalot->whence) ? LVI_STRING()...
	  ;;; which at least gcc is unhappy about, and the
	  ;;;     __attribute__((unused))
	  ;;; prevents gcc from complaining if this doesn't get used
	  (emit-fstr "#ifdef __GNUC__\n__attribute__((unused))\n#endif\n")
	  (emit-decl locv)
	  (emit-fstr "%s = (cachalot->whence) ? LVI_STRING(cachalot->whence) : LVI_NIL();\t// %s\n" locv locv))
	(emit-fstr "cachalot = %s.next;\n%s = %s;\n}\n}\n"
		   ecv res (compile-special-cond
			    ecv new-env #f
			    ((if provide-loc cddar cdar) clauses)
			    (token-source-line clauses)))
	res)
      (ERR "malformed 'guard' expression '%v'" clauses)))

(define (compile-qq-ordinary-list level cur-env expr)
  (let* ((lt (compile-qq level cur-env (cdr expr)))
	 (lh (compile-qq level cur-env (car expr)))
	 (splice? (and (pair? (car expr))
		       (symbol? (caar expr))
		       (symbol=? (caar expr) 'unquote-splicing)))
	 (merge-fn (cdr (lookup-symbol cur-env (if splice? 'append 'cons)))))
    (unless (or (symbol=? (car merge-fn) 'prim)
		(symbol=? (car merge-fn) 'priml))
      (ERR "'quasiquote' merge lookup failed!!"))
    (apply-prim "quasiquote merge" (cdr merge-fn)
		(list lh lt) (token-source-line expr)
		(symbol=? (car merge-fn) 'priml))))

(define (compile-qq level cur-env expr)
  (cond ((or (symbol? expr)
	     (boolean? expr)
	     (char? expr)
	     (string? expr)
	     (number? expr)
	     (null? expr))
	 (compile-immediate cur-env expr))
	((pair? expr)
	 (if (symbol? (car expr))
	     (cond ((or (symbol=? (car expr) 'unquote)
			(symbol=? (car expr) 'unquote-splicing))
		    (if (= level 1)
			(maybe-compile-expr cur-env #f (cadr expr))
			(compile-qq-ordinary-list (- level 1) cur-env expr)))
		   ((symbol=? (car expr) 'quasiquote)
		    (compile-qq-ordinary-list (+ level 1) cur-env expr))
		   (else
		    (compile-qq-ordinary-list level cur-env expr)))
	     (compile-qq-ordinary-list level cur-env expr)))
	((vector? expr)
	 (let ((tmp (compile-qq-ordinary-list
		     level cur-env (vector->list expr)))
	       (conv-fn (cdr (lookup-symbol cur-env 'list->vector))))
	   (unless (or (symbol=? (car conv-fn) 'prim)
		       (symbol=? (car conv-fn) 'priml))
	     (ERR "'quasiquote' conversion lookup failed!"))
	   (apply-prim "quasiquote convert" (cdr conv-fn) (list tmp)
		       (token-source-line expr)
		       (symbol=? (car conv-fn) 'priml))))

;;; bytevectors are a little problematic, because they assume the contents
;;; are always bytes, and thus only have space for bytes. so storing some
;;; arbitrary expression in there is not possible, even if it evaluates to
;;; a byte at the end. probably not going to support this for a while if
;;; ever. instead, use a vector, generate only bytes, and then convert it
;;; to a bytevector
;;;	((bytevector? expr)
;;;	 (let ((tmp (compile-qq-ordinary-list
;;;		     level cur-env (bytevector->list expr)))
;;;	       (conv-fn (cdr (lookup-symbol cur-env 'list->bytevector))))
;;;	   (unless (or (symbol=? (car conv-fn) 'prim)
;;;		       (symbol=? (car conv-fn) 'priml))
;;;	     (ERR "'quasiquote' conversion lookup failed!!"))
;;;	   (apply-prim "quasiquote convert" (cdr conv-fn) (list tmp)
;;;		       (token-source-line expr)
;;;		       (symbol=? (car conv-fn) 'priml))))

	(else				;;; unimplemented or impossible cases
	 (ERR "malformed 'quasiquote' expression '%v'" expr))))

(define (compile-special-qq cur-env tcall expr)
  (debug-trace 'compile-special-qq cur-env tcall expr)
  (if (list-length=? 1 expr)
      (compile-qq 1 cur-env (car expr))
      (ERR "malformed 'quasiquote' expression '%v'" expr)))

(define (compile-special-cond-expand cur-env tcall clauses)
  (debug-trace 'compile-special-cond-expand cur-env tcall clauses)
  (compile-special-begin cur-env tcall (do-cond-expand clauses)))

(define (compile-special cur-env tcall expr)
  (debug-trace 'compile-special cur-env tcall expr)
  (cond ((symbol=? (car expr) 'and)
	 (compile-special-andor 'true cur-env tcall (cdr expr)))
	((symbol=? (car expr) 'or)
	 (compile-special-andor 'false cur-env tcall (cdr expr)))
	((symbol=? (car expr) 'begin)
	 (compile-special-begin cur-env tcall (cdr expr)))
((symbol=? (car expr) 'begin-new)
 (compile-special-begin-new cur-env tcall (cdr expr)))
	((symbol=? (car expr) 'if)
	 (compile-special-if cur-env tcall (cdr expr)))
	((symbol=? (car expr) 'cond)
	 (compile-special-cond #f cur-env tcall (cdr expr)
			       (token-source-line expr)))
	((symbol=? (car expr) 'do)
	 (compile-special-do cur-env tcall (cdr expr)))
	((symbol=? (car expr) 'let)
	 (with-mirrors
	  (let ((res (compile-special-lettish #f cur-env tcall (cdr expr))))
	    (reset-mirrors global-mirrors)
	    res)))
	((symbol=? (car expr) 'let*)
	 (with-mirrors
	  (let ((res (compile-special-lettish #t cur-env tcall (cdr expr))))
	    (reset-mirrors global-mirrors)
	    res)))
	((or (symbol=? (car expr) 'letrec)
	     (symbol=? (car expr) 'letrec*))
	 (with-mirrors
	  (let ((res (compile-special-letrec cur-env tcall (cdr expr))))
	    (reset-mirrors global-mirrors)
	    res)))
	((symbol=? (car expr) 'set!)
	 (compile-special-set! cur-env tcall (cdr expr)))
	((symbol=? (car expr) 'lambda)
	 (compile-special-lambda #f #f cur-env tcall (cdr expr)))
	((symbol=? (car expr) 'case)
	 (compile-special-case cur-env tcall (cdr expr)))
	((symbol=? (car expr) 'guard)
	 (compile-special-guard cur-env tcall (cdr expr)))
	((symbol=? (car expr) 'quasiquote)
	 (compile-special-qq cur-env tcall (cdr expr)))
	((symbol=? (car expr) 'unquote)
	 (ERR "naked 'unquote' form '%v'" expr))
	((symbol=? (car expr) 'unquote-splicing)
	 (ERR "naked 'unquote-splicing' form '%v'" expr))
	((symbol=? (car expr) 'cond-expand)
	 (compile-special-cond-expand cur-env tcall (cdr expr)))
	(else (ERR "special form '%v' is not implemented yet" expr))))

(define (is-special-form? val)
  (and (symbol? val)
       (or (symbol=? val 'and)
	   (symbol=? val 'begin)
   (symbol=? val 'begin-new)
	   (symbol=? val 'case)
	   (symbol=? val 'cond)
	   (symbol=? val 'cond-expand)
;;; these get handled differently, they are special specials
;;;	   (symbol=? val 'define)
;;;	   (symbol=? val 'define-primitive)
;;;	   (symbol=? val 'define-alias)
	   (symbol=? val 'delay)		;;; TODO
	   (symbol=? val 'do)
	   (symbol=? val 'guard)
	   (symbol=? val 'if)
	   (symbol=? val 'lambda)
	   (symbol=? val 'let)
	   (symbol=? val 'let*)
	   (symbol=? val 'letrec)
	   (symbol=? val 'letrec*)
	   (symbol=? val 'or)
	   (symbol=? val 'quasiquote)
	   (symbol=? val 'unquote)
	   (symbol=? val 'unquote-splicing)
;;; this gets handled differently now
;;;	   (symbol=? val 'quote)
	   (symbol=? val 'set!))))

(define (build-regular-prim res nargs arity c-fn args call-loc)
  (cond ((negative? arity)
	 (let* ((narity (- arity))
		(req (- narity 1))
		(tmp (new-svar)))
	   (apply build-basic-list res (list-tail args req))
	   (emit-fstr "{\n")
	   (emit-decl tmp narity)
	   (when (positive? req)
	     (for-each (lambda (ix v) (emit-fstr "%s[%d] = %s;\n" tmp ix v))
		       (upfrom 0 req) (list-head args req)))
	   (emit-fstr "%s[%d] = %s;\n" tmp req res)
	   (emit-function-call res c-fn #f tmp #f #f call-loc)
	   (emit-fstr "}\n")))
	((zero? nargs)
	 (emit-decl res)
	 (emit-function-call res c-fn #f "NULL" #f #f call-loc))
	(else
	 (let ((tmp (new-svar)))
	   (emit-decl res)
	   (emit-fstr "{\n")
	   (emit-decl tmp nargs)
	   (for-each (lambda (ix v) (emit-fstr "%s[%d] = %s;\n" tmp ix v))
		     (upfrom 0 nargs) args)
	   (emit-function-call res c-fn #f tmp #f #f call-loc)
	   (emit-fstr "}\n")))))

(define (apply-prim s-name ops args call-loc add-loc?)
  (let* ((res (new-svar))
	 (nargs (list-length args))
	 (aop (let loop ((os ops))
		(if (null? os)
		    (ERR "primitive '%v' does not have a version for %d args"
			 s-name nargs)
		    (let ((arity (car os)))
		      (if (or (and (or (zero? arity)
				       (positive? arity))
				   (= arity nargs))
			      (and (negative? arity)
				   (>= nargs (- (- arity) 1))))
			  (list arity (cadr os))
			  (loop (cddr os)))))))
	 (arity (car aop))
	 (op (cadr aop)))
    (if (string? op)
	(build-regular-prim res nargs arity op args call-loc)
	(apply op (cons res (if add-loc? (cons call-loc args) args))))
    res))

(define global-prims-memoized #f)

(define (wrap-prim s-name codelets call-loc add-loc?)
  (let ((lookup (assv s-name global-prims-memoized)))
    (if (and lookup (not add-loc?))
	(cdr lookup)
	(let ((f-name (new-svar 'fn))
	      (c-name (new-svar))
	      (a-name (new-svar))
	      (top-label (new-svar 'lbl))
	      (res (new-svar))
	      (ncs (/ (list-length codelets) 2))
	      (arity (car codelets)))
	  (with-output
	   global-func
	   (emit-function-head f-name top-label #f c-name a-name s-name
			       (token-source-line s-name))
	   (if (> ncs 1)
	       (ERR "ambiguous primitive '%s': %d choices cannot be wrapped yet"
		    s-name ncs)
	       (let ((cas (map (lambda (i) (args-ref a-name i))
			       (upfrom 0 (abs arity)))))
	    ;;; the build-variadic-bypass is only for variadic functions,
	    ;;; it doesn't do anything for fixed-arity functions; so we
	    ;;; can set it for either case
		 (fluid-let ((build-variadic-bypass
			      (args-ref a-name (- (abs arity) 1))))
		   (let ((op (cadr codelets)))
		     (if (string? op)
			 (build-regular-prim res arity arity op cas call-loc)
			 (apply op (cons res (if add-loc?
						 (cons call-loc cas)
						 cas))) )))))
	   (emit-function-tail res)
	   (emit-fstr "// end of prim %s\n" f-name))
	  (increment-func-used-count f-name)
	  (let ((res (string-append "LVI_PROC(" f-name "," "NULL" ","
				    (number->string arity) ")")))
	    (set! global-prims-memoized
		  (cons (cons s-name res) global-prims-memoized))
	    res)))))

(define (tcall-off s-name nargs variadic?)
  (unless (negative? global-tc-min-args)
    (fprintf stderr
	     "wile alert: tail call generation to %s at line %s failed\n"
	     s-name (token-source-line s-name))
    (fprintf stderr
	     "    too many args: %d%s vs %d; continuing with non-tail call\n    try re-running compilation with '-T %d' option\n\n"
	     nargs (if variadic? "+" "") global-tc-min-args
	     (if variadic? (+ nargs 1) nargs)))
  #f)

(define (apply-proc s-name c-fn-name c-cl-name arity args tcall frame call-loc)
  (let* ((res (new-svar))
	 (sars (map sym2str args))
	 (parr (new-svar))
	 (star (new-svar))
	 (narity (- arity))
	 (req (- narity 1))
	 (nargs (list-length args)))
    (emit-decl res)
    (cond ((zero? arity)
	   (unless (zero? nargs)
	     (ERR "'%v' expects 0 args but was called with %d"
		  s-name nargs))
	   (if tcall
	       (begin
		 (reset-mirrors global-mirrors)
		 (set! parr tcall))
	       (emit-decl parr 1)))
	  ((positive? arity)
	   (unless (= arity nargs)
	     (ERR "'%v' expects %d args but was called with %d"
		  s-name arity nargs))
	   (when tcall
	     (if (<= nargs global-tc-min-args)
		 (set! parr tcall)
		 (set! tcall (tcall-off s-name nargs #f))))
	   (let ((stt (if tcall star parr)))
	     (emit-decl stt arity)
	     (for-each (lambda (i v) (emit-fstr "%s[%d] = %s;\n" stt i v))
		       (upfrom 0 arity) sars))
	   (when tcall
	     (reset-mirrors global-mirrors)
	     (for-each (lambda (i)
			 (emit-fstr "%s[%d] = %s[%d];\n" parr i star i))
		       (upfrom 0 arity))))
	  ((negative? arity)
	   (unless (<= req nargs)
	     (ERR "'%v' expects at least %d args but was called with %d"
		  s-name req nargs))
	   (let* ((rars (if (positive? req) (list-head sars req) ()))
		  (oars (if (positive? req) (list-tail sars req) sars))
		  (noar (list-length oars))
		  (pix 0)
		  (oix 0)
		  (olst (new-svar)))
	     (when tcall
	       (if (< req global-tc-min-args)
		   (set! parr tcall)
		   (set! tcall (tcall-off s-name req #t))))
	     (let ((stt (if tcall star parr)))
	       (emit-decl stt narity)
	       (for-each (lambda (a)
			   (emit-fstr "%s[%d] = %s;\n" stt pix a)
			   (set! pix (+ pix 1)))
			 rars))
	     (if (zero? noar)
		 (emit-fstr "%s[%d] = LVI_NIL();\n" parr pix)
		 (begin (emit-decl olst noar)
			(for-each (lambda (a)
				    (emit-fstr "%s[%d] = %s;\n" olst oix a)
				    (set! oix (+ oix 1)))
				  oars)
			(emit-fstr "%s[%d] = wile_gen_list(%d, %s, NULL);\n"
				   parr pix noar olst)))
	     (when tcall
	       (reset-mirrors global-mirrors)
	       (for-each (lambda (i)
			   (emit-fstr "%s[%d] = %s[%d];\n" parr i star i))
			 (upfrom 0 pix))))))
    (emit-function-call res c-fn-name c-cl-name parr tcall frame call-loc)
    res))

(define (apply-macro s-name mac-op exprs)
  (let* ((arity (car mac-op))
	 (narity (- arity))
	 (req (- narity 1))
	 (nargs (list-length exprs)))
    (if (negative? arity)
	(unless (<= req nargs)
	  (ERR "macro '%v' expects at least %d args but was called with %d"
	       s-name req nargs))
	(unless (= arity nargs)
	  (ERR "macro '%v' expects %d args but was called with %d"
	       s-name arity nargs)))
    (apply (cadr mac-op) exprs)))

(define (wrap-macro mac si)
  (let ((arity (cadr si)))
    (cond ((zero? arity)
	   `(lambda () (,mac)))
	  ((positive? arity)
	   (let ((args (map (lambda (i)
			      (string->symbol
			       (string-append "a" (number->string i))))
			    (fromto 1 arity))))
	     `(lambda (,@args) (,mac ,@args))))
	  ;;; TODO: implement this bit
	  (else (ERR "negative arity macro wrap is not implemented yet")))))

(define (compile-expr cur-env tcall expr)
  (debug-trace 'compile-expr cur-env tcall expr)
  (cond
   ((is-immediate? expr)
    (compile-immediate cur-env expr))
   ((is-special-form? (car expr))
    (compile-special cur-env tcall expr))
   ((symbol? (car expr))
    (let* ((s-name (car expr))
	   (tmp (lookup-symbol cur-env s-name))
	   (fc (car tmp))
	   (op (cdr tmp)))
      (if (symbol=? (car op) 'macro)
	  (compile-expr cur-env tcall (apply-macro s-name (cdr op) (cdr expr)))
	  (let ((args (map (lambda (ex) (maybe-compile-expr cur-env #f ex))
			   (cdr expr)))
		(call-loc (token-source-line expr)))
	    (cond ((symbol=? (car op) 'c-var)
		   (let ((tmp (new-svar)))
		     (apply build-basic-list tmp args)
		     (compile-runtime-apply
		      (new-svar) (token-source-line expr) (cadr op) tmp)))
		  ((or (symbol=? (car op) 'prim)
		       (symbol=? (car op) 'priml))
		   (apply-prim s-name (cdr op) args call-loc
			       (symbol=? (car op) 'priml)))
		  ((symbol=? (car op) 'proc)
		   (apply-proc s-name (caddr op) (cadddr op) (cadr op)
			       args tcall (lookup-frame cur-env) call-loc))
		  (else
		   (ERR "unknown symbol '%s'" s-name)))))))
   (else
    (let ((ator (compile-expr cur-env #f (car expr)))
	  (args (map (lambda (ex) (maybe-compile-expr cur-env #f ex))
		     (cdr expr)))
	  (tmp (new-svar)))
      (apply build-basic-list tmp args)
      (compile-runtime-apply (new-svar) (token-source-line expr) ator tmp)))))

(define (maybe-compile-expr cur-env tcall expr)
  (if (symbol? expr)
      (let* ((tmp (lookup-symbol cur-env expr))
	     (fc (car tmp))
	     (si (cdr tmp)))
	(cond ((symbol=? (car si) 'c-var)
	       (cadr si))
	      ((or (symbol=? (car si) 'prim)
		   (symbol=? (car si) 'priml))
	       (wrap-prim expr (cdr si) (token-source-line expr)
			  (symbol=? (car si) 'priml)))
	      ((symbol=? (car si) 'proc)
	       (let ((r (new-svar))
		     (a1 (caddr si))
		     (a2 (cadddr si))
		     (a3 (number->string (cadr si))))
		 (increment-func-used-count a1)
		 (emit-code "@@ = LVI_PROC(@1,@2,@3);")))
	      ((symbol=? (car si) 'macro)
	       (compile-expr cur-env tcall (wrap-macro expr si)))
	      (else (ERR "unknown symbol '%s'" expr))))
      (compile-expr cur-env tcall expr)))

(define (is-pragma? obj)
  (and (pair? obj) (symbol? (car obj)) (symbol=? (car obj) 'pragma)))

(define (is-define? obj)
  (and (pair? obj)
       (symbol? (car obj))
       (or (symbol=? (car obj) 'define)
	   (symbol=? (car obj) 'define-primitive)
	   (symbol=? (car obj) 'define-alias)
	   (symbol=? (car obj) 'defmacro))))

(define (compile-const c-var-name cur-env def port)
  (debug-trace 'compile-const cur-env #f def)
  (let ((const-name (car def))
	(tmp (if c-var-name c-var-name (new-svar)))
	(do-init #f))
    (with-output
     global-decl
     ;;; statically initialize nil, boolean, char, string,
     ;;; and real-valued numeric LITERAL data
     (cond ((null? (cadr def))
	    (emit-fstr "static lval %s = LVI_NIL();\n" tmp))
	   ((boolean? (cadr def))
	    (emit-fstr "static lval %s = LVI_BOOL(%s);\n"
		       tmp (if (cadr def) "true" "false")))
	   ((char? (cadr def))
	    (emit-fstr "static lval %s = LVI_CHAR(%d);\n"
		       tmp (char->integer (cadr def))))
	   ((integer? (cadr def))
	    (emit-fstr "static lval %s = LVI_INT(%d);\n" tmp (cadr def)))
	   ((rational? (cadr def))
	    (emit-fstr "static lval %s = LVI_RAT(%d,%d);\n"
		       tmp (numerator (cadr def))
		       (denominator (cadr def))))
	   ((real? (cadr def))
	    (emit-fstr "static lval %s = LVI_REAL(%s);\n"
		       tmp (format-real (cadr def))))
	   ;;; we don't have complex literals: (cmplx) is a runtime
	   ;;; constructor. so no way to do complex values here atm
	   ((string? (cadr def))
	    (emit-fstr "static lval %s = LVI_STRING_NOCPY(\"%s\");\n"
		       tmp (escapify (cadr def))))
	   (else
	    (emit-fstr "static lval %s;\n" tmp)
	    (set! do-init #t))))
    (when do-init
      (with-output
       global-code
       (emit-fstr "%s = %s;\n"
		  tmp (maybe-compile-expr cur-env #f (cadr def)))))
    (make-var-def const-name tmp)))

(define (declare-function c-fn-name cur-env def c-port s-port doc-string)
  (debug-trace 'declare-function cur-env #f def)
  (let* ((args (cdar def))
	 (argies (args-list args))
	 (_ (unless (unique-symbols? (cadr argies))
	      (ERR "malformed 'define' args list '%v'" args)))
	 (arity (car argies))
	 (tmp-fn (if c-fn-name c-fn-name (new-svar 'fn))))
    (if c-fn-name
	(begin
	  (when c-port
	    (let ((c-bag (make-string-bag ())))
	      (with-output
	       c-bag
	       (emit-fstr "lval %s(lptr*, lptr, const char*);\n" tmp-fn))
	      (display c-bag c-port)))
	  (when s-port
	    (if (string=? doc-string "")
		(fprintf s-port "(%s prim %d \"%s\")\n"
			 (caar def) arity c-fn-name)
		(fprintf s-port "(%s \"%s\" prim %d \"%s\")\n"
			 (caar def) doc-string arity c-fn-name))))
	(with-output
	 global-decl
	 (emit-fstr "static lval %s(lptr*, lptr, const char*);\n" tmp-fn)))
    (make-fn-def (caar def) tmp-fn "NULL" arity)))

(define (compile-function c-fn-name cur-env def)
  (debug-trace 'compile-function cur-env #f def)
  (let* ((args (car def))
	 (argies (args-list args))
	 (_ (unless (unique-symbols? (cadr argies))
	      (ERR "malformed 'define' args list '%v'" args)))
	 (sas (cdadr argies))
	 (tmp (lookup-symbol cur-env (caar def)))
	 (fc (car tmp))
	 (si (cdr tmp))
	 (tmp-fn (cadddr tmp))
	 (c-name (new-svar))
	 (a-name (new-svar))
	 (top-label (new-svar 'lbl))
	 (cur-env (cons (list frame-sym tmp-fn top-label) cur-env))
	 (cas (map (lambda (i) (args-ref a-name i))
		   (upfrom 0 (list-length sas)))))
    (fluid-let ((global-closures
		 (cons (make-closure-table c-name 0 () ()) global-closures)))
      (with-output
       global-func
       (with-mirrors
	(emit-function-head tmp-fn top-label c-fn-name c-name a-name
			    (car def) (token-source-line def))
	(for-each (lambda (sa ca)
		    (set! cur-env (cons (make-var-def sa ca) cur-env)))
		  sas cas)
	(let ((res (compile-special-begin cur-env a-name (cdr def))))
	  (reset-mirrors global-mirrors)
	  (emit-function-tail res))
	(emit-fstr "// end of function %s\n" tmp-fn))))))

(define (declare-deffish cur-env def c-port s-port)
  (debug-trace 'declare-deffish cur-env #f def)
  (when (> global-verbose 0)
    (write-string stderr "declare ")
    (display (cadr def) stderr)
    (newline stderr))
  (cond
   ((symbol=? (car def) 'define)
    (if (symbol? (cadr def))
	(compile-const #f cur-env (cdr def) c-port)
	(declare-function #f cur-env (cdr def) c-port s-port "")))
   ((symbol=? (car def) 'define-primitive)
    (if (symbol? (cadddr def))
	(compile-const (cadr def) cur-env (cdddr def) c-port)
	(declare-function
	 (cadr def) cur-env (cdddr def) c-port s-port (caddr def))))
   ((symbol=? (car def) 'define-alias)
    (when s-port
      (fprintf s-port "(%s alias %s)\n" (cadr def) (caddr def))))
   ((symbol=? (car def) 'defmacro)
    (if (symbol? (cadr def))
	(ERR "sorry, macro symbols are not supported")
	(declare-macro (cdr def))))
   (else (ERR "unknown toplevel 'define' action '%s'" (car def)))))

(define (compile-deffish cur-env def)
  (debug-trace 'compile-deffish cur-env #f def)
  (when (> global-verbose 0)
    (write-string stderr "compile ")
    (display (cadr def) stderr)
    (newline stderr))
  (guard (err (#t (fprintf stderr "caught exception\n    %v\n" err)
		  (set! global-errors #t)))
	 (cond
	  ((symbol=? (car def) 'define)
	   (unless (symbol? (cadr def))
	     (compile-function #f cur-env (cdr def))))
	  ((symbol=? (car def) 'define-primitive)
	   (unless (symbol? (cadddr def))
	     (compile-function (cadr def) cur-env (cdddr def))))
	  ((symbol=? (car def) 'define-alias) #t)	;;; relax, all good
	  ((symbol=? (car def) 'defmacro) #t)		;;; relax, all good
	  (else (ERR "unknown toplevel 'define' action '%s'" (car def))))))

(define (find-pragma p ps)
  (let loop ((ps ps))
    (cond ((null? ps) #f)
	  ((symbol=? p (caar ps))
	   (if (null? (cdar ps))
	       #t
	       (cdar ps)))
	  (else (loop (cdr ps))))))

;;; TODO:
;;; * what about missing files? -> fail
;;; * what about files in sub-directories of the search path?
;;;   ie, analogous to C header file <sys/types.h> ?

(define (read-recursive search ihash in-file)
  (when (hash-table-contains? ihash in-file)
    (ERR "include loop! already processed file '%s'" in-file))
  (let loop1 ((data (read-all in-file))
	      (acc ()))
    (hash-table-set! ihash in-file #t)
    (cond ((null? data) (list-reverse acc))
	  ((and (pair? (car data))
		(symbol? (caar data))
		(or (symbol=? (caar data) 'load)
		    (symbol=? (caar data) 'load-library)))
	   (let* ((inc-file (cadar data))
		  (cur-path (if (or (symbol=? (caar data) 'load)
				    (string-find-first-char inc-file #\/))
				'("")
				search)))
	     (when (> global-verbose 0)
	       (write-string stderr "include file " inc-file #\linefeed))
	     (let loop2 ((cp cur-path))
	       (if (null? cp)
		   (ERR "cannot find file %s  in search path!" inc-file)
		   (let ((pfile (if (string=? (car cp) "")
				    inc-file
				    (string-append (car cp) "/" inc-file))))
		     (when (> global-verbose 1)
		       (write-string stderr "    seeking " pfile #\newline))
		     (if (file-exists? pfile)
			 (let ((new-data (read-recursive search ihash pfile)))
			   (loop1 (cdr data)
				  (let loop3 ((l1 new-data)
					      (l2 acc))
				    (if (null? l1)
					l2
					(loop3 (cdr l1) (cons (car l1) l2))))))
			 (loop2 (cdr cp))))))))
	  (else (loop1 (cdr data) (cons (car data) acc))))))

;;; This function has to
;;;     a) learn new macros, if defmacro is specified;
;;;     b) expand existing top-level macros, in case they expand into begin2;
;;;     c) expand existing top-level begin2
;;;
;;; Since the expansions in (b) and (c) may expose new defmacro or begin2
;;; or uses of known macros, this function has to loop until there are no
;;; more changes, it has to update its environment, and it has to return
;;; both the updated environment and the new list of top-level exprs.

(define (expand-toplevel cur-env exprs)
  (let loop ((cur-env cur-env)
	     (exprs exprs)
	     (acc ())
	     (cflag #f))
    (if (null? exprs)
	(if cflag
	    (loop cur-env (list-reverse acc) () #f)
	    (list cur-env (list-reverse acc)))
	(let ((cur (car exprs))
	      (rest (cdr exprs)))
	  (if (and (pair? cur)
		   (symbol? (car cur)))
	      (let ((cur-sym (car cur)))
		(cond ((symbol=? cur-sym 'defmacro)
		       (if (symbol? (cadr cur))
			   (ERR "sorry, macro symbols are not supported")
			   (loop (cons (declare-macro (cdr cur)) cur-env)
				 rest acc #t)))
		      ((symbol=? cur-sym 'begin)
		       (if (any-true? (map is-define? (cdr cur)))
			   (loop cur-env rest
				 (append (list-reverse (cdr cur)) acc) #t)
			   (loop cur-env rest (cons cur acc) cflag)))
		      (else (let ((tmp (lookup-symbol-nofail cur-env cur-sym)))
			      (if (and (pair? tmp)
				       (symbol=? (cadr tmp) 'macro))
				  (loop cur-env rest
					(cons (apply-macro cur-sym (cddr tmp)
							   (cdr cur)) acc) #t)
				  (loop cur-env rest (cons cur acc) cflag))))))
	      (loop cur-env rest (cons cur acc) cflag))))))

;;; remove stuff between a return or goto or TAIL_CALL statement
;;; and the next closing brace

(define (remove-dead-code lines)
  (let loop ((ls lines)
	     (out #t)
	     (acc ()))
    (if (null? ls)
	(list-reverse acc)
	(let ((l1 (car ls)))
	  (cond (out
		 (loop (cdr ls)
		       (not (regex-match "^(return|goto|TAIL_CALL)" l1))
		       (cons l1 acc)))
		((regex-match "^}" l1)
		 (loop (cdr ls) #t (cons l1 acc)))
		(else
		 (loop (cdr ls) #f acc)))))))

;;; remove unused functions - still very imperfect: it only catches
;;; the first level of unused functions; if, after removal of these,
;;; more functions become unused, it doesn't catch those. need a
;;; proper intermediate language!

(define (drop-until-fn-end fname lst)
  (let ((str (string-append "// end of function " fname)))
    (until (or (null? lst)
	       (string=? "" (car lst))
	       (string=? str (car lst)))
	   (set! lst (cdr lst)))
    (cons (string-append "// dropped unused function " fname) (cdr lst))))

(define (remove-unused-functions lines)
  (let loop ((ls lines)
	     (acc ()))
    (if (null? ls)
	(list-reverse acc)
	(let* ((l1 (car ls))
	       (rmat1 (regex-match "@@@ +fn_[0-9]+ +@@@$" l1))
	       (rmat2 (regex-match "static lval fn_[0-9]+\\(lptr\\*, lptr, const char\\*\\);" l1)))
	  (if rmat1
	      (let* ((fn1 (cadr rmat1))
		     (fn (string-trim (lambda (c)
					(or (char=? c #\@)
					    (char-whitespace? c))) fn1))
		     (cc (hash-table-ref global-funcs-used fn #f)))
		(if (and cc (zero? cc))
		    (begin
		      (when (> global-verbose 1)
			(write-string stderr l1 "\n"))
		      (loop (drop-until-fn-end fn (cdr ls)) acc))
		    (loop (cdr ls) (cons l1 acc))))
	      (if rmat2
		  (let* ((fn1 (cadr rmat2))
			 (fn2 (string-copy fn1 12))
			 (pix (string-find-first-char fn2 #\())
			 (fn (string-copy fn2 0 pix))
			 (cc (hash-table-ref global-funcs-used fn #f)))
		    (if (and cc (zero? cc))
			(loop (cdr ls) acc)
			(loop (cdr ls) (cons l1 acc))))
		  (loop (cdr ls) (cons l1 acc))))))))

(define (remove-unused-lbls lines)
  (let ((decl (hash-table-create string-hash string=?))
	(used (hash-table-create string-hash string=?))
	(re-dec "^lbl_[0-9]+:")
	(lbl-from-dec (lambda (str)
			(string-copy str 4 (string-find-first-char str #\:))))
	(re-use "^goto lbl_[0-9]+;")
	(lbl-from-use (lambda (str)
			(string-copy str 9 (string-find-first-char str #\;))))
	(multi-dec "warning! lbl_%s appears more than once!\n")
	(output ()))
    (for-each (lambda (l)
		(cond ((regex-match re-dec l)
		       (let ((var (lbl-from-dec l)))
			 (if (hash-table-contains? decl var)
			     (fprintf stderr multi-dec var)
			     (hash-table-set! decl var 1))))
		      ((regex-match re-use l)
		       (let ((var (lbl-from-use l)))
			 (hash-table-set! used var 1)))))
	      lines)
    (for-each (lambda (l)
		(unless (hash-table-contains? decl l)
		  (fprintf stderr "wile error: lbl_%s undeclared\n" l)))
	      (hash-table-keys used))
    (let loop ((ls lines)
	       (acc ()))
      (if (null? ls)
	  (list-reverse acc)
	  (let ((l (car ls)))
	    (loop (cdr ls)
		  (if (and (regex-match re-dec l)
			   (not (hash-table-contains? used (lbl-from-dec l))))
		      acc
		      (cons l acc))))))))

(defmacro (add-output val) `(set! output (cons ,val output)))

(define (strvec-ref bv ix)
  (bytevector-ref bv (string->number ix)))

(define (strvec-set! bv ix)
  (bytevector-set! bv (string->number ix) #xff))

(define (truthy v)
  (> v #x7f))

(define (strvec-foreach proc vec1 vec2)
  (let ((len (bytevector-length vec1)))
    (do ((i 0 (+ i 1)))
	((= i len) #t)
      (proc (bytevector-ref vec1 i)
	    (bytevector-ref vec2 i)))))

(define (remove-unused-vars nvars lines)
  (let ((decl (bytevector-create nvars 0))
	(set (bytevector-create nvars 0))
	(used (bytevector-create nvars 0))
	(re-dec "^((__attribute__\\(\\(unused\\)\\)|static)[ \t]+)?(lval|lptr|lptr\\*) var_[0-9]+(\[[0-9]+\])?(;| = LVI_(NIL|BOOL|INT|CHAR|REAL|RAT))")
	(re-set "^var_[0-9]+ =")
	(re-use "var_[0-9]+")
	(re-num "[0-9]+")
	(re-copy "^var_[0-9]+ = var_[0-9]+;")
	(multi-dec "warning! var_%s is being declared more than once!\n")
	(output ()))
    (for-each (lambda (l)
		(let ((r l))
		  (cond ((regex-match re-dec l)
			 (let ((var (cadr (regex-match re-num l))))
			   (if (zero? (strvec-ref decl var))
			       (strvec-set! decl var)
			       (fprintf stderr multi-dec var))
			   (set! r #f)))
			((regex-match re-set l)
			 (let ((chop (regex-match re-num l)))
			   (strvec-set! set (cadr chop))
			   (set! r (caddr chop)))))
		  (while r
			 (set! r
			       (let ((chop1 (regex-match re-use r)))
				 (if chop1
				     (let ((chop2 (regex-match
						   re-num (cadr chop1))))
				       (strvec-set! used (cadr chop2))
				       (caddr chop1))
				     #f))))))
	      lines)
    (let ((count 0))
      (strvec-foreach
       (lambda (d s)
	 (when (and (truthy s) (not (truthy d)))
	   (set! count (+ count 1))))
       decl set)
      (when (positive? count)
	(fprintf stderr
		 "wile error: %d set but undeclared variables!\n" count)))
    (let ((count 0))
      (strvec-foreach
       (lambda (s u)
	 (when (and (truthy s) (not (truthy u)))
	   (set! count (+ count 1))))
       set used)
      (strvec-foreach
       (lambda (d u)
	 (when (and (truthy d) (not (truthy u)))
	   (set! count (+ count 1))))
       decl used)
      (if (zero? count)
	  lines
	  (begin
	    (when (> global-verbose 0)
	      (fprintf stderr "set but unused variables: %d\n" count))
	    (for-each (lambda (l)
			(let* ((var1 (regex-match re-num l))
			       (var (if var1 (cadr var1) #f)))
			  (cond ((not var) (add-output l))
				((regex-match re-dec l)
				 (when (truthy (strvec-ref used var))
				   (add-output l)))
				((regex-match re-set l)
				 (if (truthy (strvec-ref used var))
				     (add-output l)
				     (unless (regex-match re-copy l)
				       (let* ((ix (string-find-first-char
						   l #\=))
					      (l2 (string-copy l (+ ix 1))))
;;; TODO omg this is fragile! the space needs to be in there;
;;; however, at least if it fails it's still correct...
					 (unless (string=?
						  l2 " LVI_BOOL(false);")
					   (add-output "(void)")
					   (add-output l2))))))
				(else (add-output l)))))
		      lines)
	    (remove-unused-vars nvars (list-reverse output)))))))

(define (compile-file do-debug opt-level in-file out-path out-port fvals)
  (let ((decl-port (make-string-bag ()))
	(code-port (make-string-bag ()))
	(func-port (make-string-bag ())))
    (set! global-prims-memoized ())
    (set! global-funcs-used (hash-table-create string-hash string=?))
    (fluid-let ((global-out decl-port)
		(global-decl decl-port)
		(global-code code-port)
		(global-func func-port))
      (emit-str global-file-head)
      (let* ((inc-hash (hash-table-create string-hash string=?))
	     (dirs (get-config-val 'scheme-include-directories))
	     (data1 (read-recursive dirs inc-hash in-file))
	     (data2 (partition is-pragma? data1))
	     (prags (map cdr (car data2)))
	     ;;; ugh. find a way to remove the global and thread it through
	     ;;; the code... that's not hard, but a bit messy.
	     ;;; the local variable gets ignored, set! is what we want.
	     (_1 (set! global-library (find-pragma 'library prags)))
	     (_2 (when (find-pragma 'suppress-tail-call-generation prags)
		   (set! global-tc-min-args -1)))
	     (hname (if global-library (list-ref global-library 0) #f))
	     (sname (if global-library (list-ref global-library 1) #f))
	     (tmp1 (expand-toplevel
		    (make-top-env (if global-library 'extern 'main))
		    (cadr data2)))
	     (top-env (car tmp1))
	     (data3 (partition is-define? (cadr tmp1)))
	     (defs (car data3))
	     (exprs (cadr data3))
	     (hport (if (and hname (not (null? hname)))
			(open-file (string-append out-path hname) "w")
			#f))
	     (sport (if (and sname (not (null? sname)))
			(open-file (string-append out-path sname) "w")
			#f))
	     (mname (if hname (name-mangle hname) #f))
	     (do-decl (lambda (def)
			(let ((decl (declare-deffish top-env def hport sport)))
			  (if (list? decl)
			      (begin
				(set! top-env (cons decl top-env))
				(if (symbol=? (cadr decl) 'proc)
				    (cadddr decl)
				    #f))
			      #f))))

	     ;;; the entries in the args hash table are toggles:
	     ;;; if absent, use the default, if present, use the not-default
	     ;;; if the default is #t, then negate the lookup,
	     ;;; otherwise use the lookup

	     (rm-dc (not (hash-table-ref fvals "-rm-dc" #f)))
	     (rm-uf (not (hash-table-ref fvals "-rm-uf" #f)))
	     (rm-ul (not (hash-table-ref fvals "-rm-ul" #f)))
	     (rm-uv (not (hash-table-ref fvals "-rm-uv" #f)))
	     (do-prof (hash-table-ref fvals "-p" #f)))
	;;; TODO: implement profiling of library function
	(when (and (not global-library) do-prof)
	  (set! global-profile ()))
	(when hport
	  (write-string hport
			"#ifndef " mname #\newline
			"#define " mname #\newline #\newline)
	  (emit-fstr "#include \"%s\"\n" hname))
	(unless global-library
	  (emit-fstr "\nstruct wile_profile_t* wile_profile;\nint wile_profile_size;\n\n"))
	(for-each do-decl defs)
	(set! top-env (cons (list global-sym) top-env))
	(for-each (lambda (d) (compile-deffish top-env d)) defs)
	(when hport
	  (write-string hport "\n#endif // " mname #\newline)
	  (close-port hport))
	(guard (err (#t (fprintf stderr "caught exception\n    %v\n" err)
			(set! global-errors #t)))
	       (unless global-library
		 (with-output
		  global-code
		  (emit-function-tail
		   (compile-special-begin top-env #f exprs))))
	       (emit-fstr "\n// definitions\n")
	       (transfer-all-lines global-func global-out)
	       (unless global-library
		 (when global-profile
		   (emit-fstr "static struct wile_profile_t wile_profile_array[%d];\n"
			      (list-length global-profile)))
		 (emit-fstr "\nconst int wile_tc_min_args = %d;\n\nlval wile_main(int argc, char** argv)\n{\n" global-tc-min-args)
		 (if global-profile
		     (let* ((names (list-reverse global-profile))
			    (count (list-length names))
			    (r #f)
			    (a1 (number->string count)))
		       (emit-code
			"{"
			"int i;"
			"wile_profile = wile_profile_array;"
			"wile_profile_size = @1;"
			"for (i = 0; i < wile_profile_size; ++i) {"
			"wile_profile[i].count = 0;"
			"}")
		       (for-each (lambda (i n)
				   (emit-fstr
				    "wile_profile[%d].name = \"%s\";\n" i n))
				 (upfrom 0 count) names)
		       (emit-code "}"))
		     (let ((r #f))
		       (emit-code "wile_profile = NULL;"
				  "wile_profile_size = 0;")))
		 (transfer-all-lines global-code global-out))
	       (display global-out out-port)
	       (when (and (positive? opt-level) (not global-errors))
		 (let ((lines (read-all-lines out-port))
		       (nvars (+ new-svar-index 5)))
		   (when rm-dc
		     (when (> global-verbose 0)
		       (fprintf stderr "removing dead code\n"))
		     (set! lines (remove-dead-code lines)))
		   (when rm-uf
		     (when (> global-verbose 0)
		       (fprintf stderr "removing unused functions\n"))
		     (set! lines (remove-unused-functions lines)))
		   (when rm-ul
		     (when (> global-verbose 0)
		       (fprintf stderr "removing unused labels\n"))
		     (set! lines (remove-unused-lbls lines)))
		   (when rm-uv
		     (when (> global-verbose 0)
		       (fprintf stderr "removing unused variables\n"))
		     (set! lines (remove-unused-vars nvars lines)))
		   (set-file-position out-port 0 'start)
		   (for-each (lambda (l)
			       (write-string out-port l #\newline))
			     lines)
		   (flush-port out-port)
		   (truncate-file out-port)))
	       (close-port out-port))))))
