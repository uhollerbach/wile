;;; macros that got transmogrified into wile primitives; for test-wile.scm,
;;; they are inaccessible there, so bring them back here and include this
;;; file into test-wile.scm

(set-environment-variable "SKEEM_LIBRARY_PATH" ".:library")
(set-environment-variable "WILE_LIBRARY_PATH" ".:library")

;;; stuff that skeem doesn't know about

(define list-append append)

(define list-reverse reverse)

(define (list-length=? n lst)
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

(define (get-config-val key)
  (let ((kv (assv key global-config)))
    (if kv
	(cadr kv)
	(ERR "key '%s' was not found in config!" key))))

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

;;; (defmacro (add-output val) `(set! output (cons ,val output)))

(def-struct bbox name value)

(define (wile-standard-environment)
  (map (lambda (p) (make-bbox (car p) (cdr p)))
       (list
;;; TODO: these two are probably wrong for repl: probably want name of
;;; script if any as command-name, and command-line-arguments to not
;;; include said script name
;;;;	      (cons 'command-name command-name)
;;;;	      (cons 'command-line-arguments command-line-arguments)
	(cons 'stdin stdin)
	(cons 'stdout stdout)
	(cons 'stderr stderr)
	(cons 'pi pi)
	(cons 'euler-gamma euler-gamma)
	(cons 'default-show-sign default-show-sign)
	(cons 'default-int-base default-int-base)
	(cons 'default-float-base default-flt-base)
	(cons 'default-float-precision default-flt-precision)
;;; aliases
	(cons 'append list-append)
	(cons 'call-with-current-continuation call/cc)
	(cons 'char-lower-case? char-lower-case?)
	(cons 'char-upper-case? char-upper-case?)
	(cons 'cimag imag-part)
	(cons 'complex-conjugate conj)
	(cons 'conj conj)
	(cons 'creal real-part)
	(cons 'directory-exists? file-exists?)
	(cons 'filter filter)
	(cons 'flatten flatten)
	(cons 'last list-last)
	(cons 'length list-length)
	(cons 'list->string char->string)
	(cons 'magnitude abs)
	(cons 'make-bytevector
	      (case-lambic 1 (lambda (a1)
			       (make-bytevector a1))
			   2 (lambda (a1 a2)
			       (make-bytevector a1 a2))))
	(cons 'make-rectangular cmplx)
	(cons 'make-string
	      (case-lambic 1 (lambda (a1)
			       (make-string a1))
			   2 (lambda (a1 a2)
			       (make-string a1 a2))))
	(cons 'make-vector
	      (case-lambic 1 (lambda (a1)
			       (make-vector a1))
			   2 (lambda (a1 a2)
			       (make-vector a1 a2))))
	(cons 'modulo floor-remainder)
	(cons 'number? complex?)
	(cons 'partition partition)
	(cons 'phase angle)
	(cons 'quot-rem truncate/)
	(cons 'quotient truncate-quotient)
	(cons 'read-all read-all)
	(cons 'remainder truncate-remainder)
	(cons 'rename-directory rename-file)
	(cons 'reverse list-reverse)
	(cons 'set-file-position
	      (case-lambic 2 (lambda (a1 a2)
			       (set-file-position a1 a2))
			   3 (lambda (a1 a2 a3)
			       (set-file-position a1 a2 a3))))
	(cons 'sqlite-close close-port)
	(cons 'string->char string->list)
	(cons 'sqlite-open
	      (case-lambic 0 (lambda ()
			       (sqlite-open))
			   1 (lambda (a1)
			       (sqlite-open a1))
			   2 (lambda (a1 a2)
			       (sqlite-open a1 a2))))
	(cons 'substring
	      (case-lambic 1 (lambda (a1)
			       (substring a1))
			   2 (lambda (a1 a2)
			       (substring a1 a2))
			   3 (lambda (a1 a2 a3)
			       (substring a1 a2 a3))))
	(cons 'vector-capacity vector-length)
	(cons 'write-char write-string)
	(cons '* *)
	(cons '+ +)
	(cons '- -)
	(cons '/ /)
	(cons '/= /=)
	(cons '< <)
	(cons '<= <=)
	(cons '= =)
	(cons '> >)
	(cons '>= >=)
	(cons 'UTCtime (case-lambic 0 (lambda ()
					(UTCtime))
				    1 (lambda (a1)
					(UTCtime a1))))
	(cons 'abs abs)
	(cons 'accept accept)
	(cons 'acos acos)
	(cons 'acosh acosh)
	(cons 'all-true? all-true?)
	(cons 'angle angle)
	(cons 'any-true? any-true?)
	(cons 'apply apply)
	(cons 'arithmetic-geometric-mean agm)
	(cons 'asin asin)
	(cons 'asinh asinh)
	(cons 'assp assp)
	(cons 'assv assv)
	(cons 'atan (case-lambic 1 (lambda (a1)
				     (atan a1))
				 2 (lambda (a1 a2)
				     (atan a1 a2))))
	(cons 'atanh atanh)
	(cons 'bessel-j bessel-j)
	(cons 'bessel-y bessel-y)
	(cons 'bits-and bits-and)
	(cons 'bits-clear bits-clear)
	(cons 'bits-flip bits-flip)
	(cons 'bits-get bits-get)
	(cons 'bits-not bits-not)
	(cons 'bits-or bits-or)
	(cons 'bits-set bits-set)
	(cons 'bits-set? bits-set?)
	(cons 'bits-shift bits-shift)
	(cons 'bits-xor bits-xor)
	(cons 'boolean? boolean?)
	(cons 'bytevector bytevector)
	(cons 'bytevector->string bytevector->string)
	(cons 'bytevector-create
	      (case-lambic 1 (lambda (a1)
			       (bytevector-create a1))
			   2 (lambda (a1 a2)
			       (bytevector-create a1 a2))))
	(cons 'bytevector? bytevector?)
	(cons 'c* *)
	(cons 'c+ +)
	(cons 'c- -)
	(cons 'c/ /)
	(cons 'caaaar caaaar)
	(cons 'caaadr caaadr)
	(cons 'caaar caaar)
	(cons 'caadar caadar)
	(cons 'caaddr caaddr)
	(cons 'caadr caadr)
	(cons 'caar caar)
	(cons 'cadaar cadaar)
	(cons 'cadadr cadadr)
	(cons 'cadar cadar)
	(cons 'caddar caddar)
	(cons 'caddddr caddddr)
	(cons 'cadddr cadddr)
	(cons 'caddr caddr)
	(cons 'cadr cadr)
	(cons 'call/cc call/cc)
	(cons 'car car)
	(cons 'cbrt cbrt)
	(cons 'cconj conj)
	(cons 'cdaaar cdaaar)
	(cons 'cdaadr cdaadr)
	(cons 'cdaar cdaar)
	(cons 'cdadar cdadar)
	(cons 'cdaddr cdaddr)
	(cons 'cdadr cdadr)
	(cons 'cdar cdar)
	(cons 'cddaar cddaar)
	(cons 'cddadr cddadr)
	(cons 'cddar cddar)
	(cons 'cdddar cdddar)
	(cons 'cddddr cddddr)
	(cons 'cdddr cdddr)
	(cons 'cddr cddr)
	(cons 'cdr cdr)
	(cons 'ceiling ceiling)
	(cons 'ceiling-quotient ceiling-quotient)
	(cons 'ceiling-remainder ceiling-remainder)
	(cons 'ceiling/ ceiling/)
	(cons 'cfft-good-n? cfft-good-n?)
	(cons 'change-file-owner change-file-owner)
	(cons 'change-root-directory change-root-directory)
	(cons 'change-symbolic-link-owner change-symbolic-link-owner)
	(cons 'char->integer char->integer)
	(cons 'char->string char->string)
	(cons 'char-alphabetic? char-alphabetic?)
	(cons 'char-alphanumeric? char-alphanumeric?)
	(cons 'char-ci<=? char-ci<=?)
	(cons 'char-ci<? char-ci<?)
	(cons 'char-ci=? char-ci=?)
	(cons 'char-ci>=? char-ci>=?)
	(cons 'char-ci>? char-ci>?)
	(cons 'char-control? char-control?)
	(cons 'char-downcase char-downcase)
	(cons 'char-hex-digit? char-hex-digit?)
	(cons 'char-lowercase? char-lower-case?)
	(cons 'char-numeric? char-numeric?)
	(cons 'char-oct-digit? char-oct-digit?)
	(cons 'char-printable? char-printable?)
	(cons 'char-upcase char-upcase)
	(cons 'char-uppercase? char-upper-case?)
	(cons 'char-whitespace? char-whitespace?)
	(cons 'char/=? char/=?)
	(cons 'char<=? char<=?)
	(cons 'char<? char<?)
	(cons 'char=? char=?)
	(cons 'char>=? char>=?)
	(cons 'char>? char>?)
	(cons 'char? char?)
	(cons 'close-port close-port)
	(cons 'cmplx cmplx)
	(cons 'complex? complex?)
	(cons 'connect-to connect-to)
	(cons 'cons cons)
	(cons 'continuation? continuation?)
	(cons 'cos cos)
	(cons 'cosh cosh)
	(cons 'cosine-integral cosine-integral)
	(cons 'cputime cputime)
	(cons 'create-directory
	      (case-lambic 1 (lambda (a1)
			       (create-directory a1))
			   2 (lambda (a1 a2)
			       (create-directory a1 a2))))
	(cons 'create-link create-link)
	(cons 'create-symbolic-link create-symbolic-link)
	(cons 'cxr cxr)
	(cons 'denominator denominator)
	(cons 'describe-system-error describe-system-error)
	(cons 'digamma digamma)
	(cons 'display (case-lambic 1 (lambda (a1)
					(display a1))
				    2 (lambda (a1 a2)
					(display a1 a2))))
	(cons 'display-object-hook display-object-hook)
	(cons 'elliptic-E elliptic-E)
	(cons 'elliptic-K elliptic-K)
	(cons 'epochtime epochtime)
	(cons 'eqv? eqv?)
	(cons 'erfc erfc)
;;; we're trying to build this one!
;;;	(list 'eval eval)
	(cons 'even? even?)
	(cons 'exit exit)
	(cons 'exp exp)
	(cons 'expmod expmod)
	(cons 'expt expt)
	(cons 'factorial factorial)
	(cons 'file-executable? file-executable?)
	(cons 'file-exists? file-exists?)
	(cons 'file-readable? file-readable?)
	(cons 'file-writable? file-writable?)
	(cons 'finite? finite?)
	(cons 'float float)
	(cons 'floor floor)
	(cons 'floor-quotient floor-quotient)
	(cons 'floor-remainder floor-remainder)
	(cons 'floor/ floor/)
	(cons 'flush-port (case-lambic 0 (lambda ()
					   (flush-port))
				       1 (lambda (a1)
					   (flush-port a1))))
	(cons 'fmod fmod)
	(cons 'foldl foldl)
	(cons 'foldl1 foldl1)
	(cons 'foldr foldr)
	(cons 'for-each for-each)
	(cons 'fork-process fork-process)
	(cons 'fprintf fprintf)
	(cons 'frexp frexp)
	(cons 'fromto fromto)
	(cons 'gcd gcd)
	(cons 'gensym gensym)
	(cons 'get-current-directory get-current-directory)
	(cons 'get-domain-name get-domain-name)
	(cons 'get-effective-group-id get-effective-group-id)
	(cons 'get-effective-user-id get-effective-user-id)
	(cons 'get-environment-variable get-environment-variable)
	(cons 'get-file-position get-file-position)
	(cons 'get-file-status get-file-status)
	(cons 'get-group-id get-group-id)
	(cons 'get-host-name get-host-name)
	(cons 'get-parent-process-id get-parent-process-id)
	(cons 'get-process-id get-process-id)
	(cons 'get-session-id get-session-id)
	(cons 'get-symbolic-link-status get-symbolic-link-status)
	(cons 'get-user-id get-user-id)
	(cons 'get-user-information get-user-information)
	(cons 'hypot hypot)
	(cons 'i* *)
	(cons 'i+ +)
	(cons 'i- -)
	(cons 'i/ /)
	(cons 'ilog ilog)
	(cons 'imag-part imag-part)
	(cons 'infinite? infinite?)
	(cons 'integer integer)
	(cons 'integer->char integer->char)
	(cons 'integer? integer?)
	(cons 'is-block-device? is-block-device?)
	(cons 'is-char-device? is-char-device?)
	(cons 'is-directory? is-directory?)
	(cons 'is-named-pipe? is-named-pipe?)
	(cons 'is-regular-file? is-regular-file?)
	(cons 'is-socket? is-socket?)
	(cons 'is-symbolic-link? is-symbolic-link?)
	(cons 'lambert-W lambert-W)
	(cons 'lambert-W+ lambert-W+)
	(cons 'lambert-W- lambert-W-)
	(cons 'lcm lcm)
	(cons 'ldexp ldexp)
	(cons 'list list)
	(cons 'list->vector list->vector)
	(cons 'list-append list-append)
	(cons 'list-drop-while list-drop-while)
	(cons 'list-filter filter)
	(cons 'list-flatten flatten)
	(cons 'list-head list-head)
	(cons 'list-last list-last)
	(cons 'list-length list-length)
	(cons 'list-length=? list-length=?)
	(cons 'list-length>=? list-length>=?)
	(cons 'list-length>? list-length>?)
	(cons 'list-length<? list-length<?)
	(cons 'list-length<=? list-length<=?)
	(cons 'list-partition partition)
	(cons 'list-ref list-ref)
	(cons 'list-remove-dups list-remove-dups)
	(cons 'list-reverse list-reverse)
	(cons 'list-sort list-sort)
	(cons 'list-tail list-tail)
	(cons 'list-take-while list-take-while)
	(cons 'list-unhead list-unhead)
	(cons 'list-untail list-untail)
	(cons 'list? list?)
	(cons 'listen-on listen-on)
	(cons 'localtime (case-lambic 0 (lambda ()
					  (localtime))
				      1 (lambda (a1)
					  (localtime a1))))
	(cons 'log log)
	(cons 'log-gamma log-gamma)
	(cons 'make-polar make-polar)
	(cons 'make-rational (lambda (n d) (/ n d)))
	(cons 'map map)
	(cons 'max max)
	(cons 'max/i max)
	(cons 'max/q max)
	(cons 'max/r max)
	(cons 'memp memp)
	(cons 'memv memv)
	(cons 'min min)
	(cons 'min/i min)
	(cons 'min/q min)
	(cons 'min/r min)
	(cons 'nan? nan?)
	(cons 'negative -)
	(cons 'negative? negative?)
	(cons 'newline (case-lambic 0 (lambda ()
					(newline))
				    1 (lambda (a1)
					(newline a1))))
	(cons 'not not)
	(cons 'null? null?)
	(cons 'number->string
	      (case-lambic 1 (lambda (a1)
			       (number->string a1))
			   2 (lambda (a1 a2)
			       (number->string a1 a2))
			   3 (lambda (a1 a2 a3)
			       (number->string a1 a2 a3))))
	(cons 'numerator numerator)
	(cons 'odd? odd?)
	(cons 'open-file open-file)
	(cons 'open-temporary-file open-temporary-file)
	(cons 'pair? pair?)
	(cons 'parse-file read-all)
	(cons 'parse-string parse-string)
	(cons 'pipe-port? port?)
	(cons 'poly-chebyshev1 poly-chebyshev1)
	(cons 'poly-chebyshev2 poly-chebyshev2)
	(cons 'poly-hermite1 poly-hermite1)
	(cons 'poly-hermite2 poly-hermite2)
	(cons 'poly-laguerre poly-laguerre)
	(cons 'poly-legendre poly-legendre)
	(cons 'port? port?)
	(cons 'positive? positive?)
	(cons 'printf printf)
	(cons 'procedure? procedure?)
	(cons 'promise? promise?)
	(cons 'q* *)
	(cons 'q+ +)
	(cons 'q- -)
	(cons 'q/ /)
	(cons 'r* *)
	(cons 'r+ +)
	(cons 'r- -)
	(cons 'r/ /)
	(cons 'raise raise)
	(cons 'random-exponential
	      (case-lambic 0 (lambda ()
			       (random-exponential))
			   1 (lambda (a1)
			       (random-exponential a1))))
	(cons 'random-normal-pair
	      (case-lambic 0 (lambda ()
			       (random-normal-pair))
			   2 (lambda (a1 a2)
			       (random-normal-pair a1 a2))))
	(cons 'random-poisson random-poisson)
	(cons 'random-seed!
	      (case-lambic 0 (lambda ()
			       (random-seed!))
			   1 (lambda (a1)
			       (random-seed! a1))))
	(cons 'random-uniform
	      (case-lambic 0 (lambda ()
			       (random-uniform))
			   2 (lambda (a1 a2)
			       (random-uniform a1 a2))))
	(cons 'rational? rational?)
	(cons 'read-char (case-lambic 0 (lambda ()
					  (read-char))
				      1 (lambda (a1)
					  (read-char a1))))
	(cons 'read-directory (case-lambic 0 (lambda ()
					       (read-directory))
					   1 (lambda (a1)
					       (read-directory a1))))
	(cons 'read-line read-line)
	(cons 'real-part real-part)
	(cons 'real? real?)
	(cons 'reciprocal /)
	(cons 'regex-match regex-match)
	(cons 'remove-directory remove-directory)
	(cons 'remove-file remove-file)
	(cons 'rename-file rename-file)
	(cons 'replicate replicate)
	(cons 'round round)
	(cons 'run-command run-command)
	(cons 'run-read-command run-read-command)
	(cons 'run-write-command run-write-command)
	(cons 'send-signal send-signal)
	(cons 'set-car! set-car!)
	(cons 'set-cdr! set-cdr!)
	(cons 'set-current-directory set-current-directory)
	(cons 'set-effective-group-id set-effective-group-id)
	(cons 'set-effective-user-id set-effective-user-id)
	(cons 'set-environment-variable set-environment-variable)
	(cons 'set-group-id set-group-id)
	(cons 'set-line-buffering! set-line-buffering!)
	(cons 'set-no-buffering! set-no-buffering!)
	(cons 'set-session-id set-session-id)
	(cons 'set-user-id set-user-id)
	(cons 'sign sign)
	(cons 'sin sin)
	(cons 'sine-integral sine-integral)
	(cons 'sinh sinh)
	(cons 'sleep sleep)
	(cons 'socket-port? port?)
	(cons 'sprintf sprintf)
	(cons 'sqlite-dump-table sqlite-dump-table)
	(cons 'sqlite-meta-schema sqlite-meta-schema)
	(cons 'sqlite-meta-tables sqlite-meta-tables)
	(cons 'sqlite-run sqlite-run)
	(cons 'sqlite-statement-bind sqlite-statement-bind)
	(cons 'sqlite-statement-cleanup sqlite-statement-cleanup)
	(cons 'sqlite-statement-info sqlite-statement-info)
	(cons 'sqlite-statement-prepare sqlite-statement-prepare)
	(cons 'sqlite-statement-run sqlite-statement-run)
	(cons 'sqlite-version sqlite-version)
	(cons 'sqrt sqrt)
	(cons 'string->list string->list)
	(cons 'string->number string->number)
	(cons 'string->symbol string->symbol)
	(cons 'string-append string-append)
	(cons 'string-ci-hash-32 string-ci-hash-32)
	(cons 'string-ci-hash-64 string-ci-hash-64)
	(cons 'string-ci<=? string-ci<=?)
	(cons 'string-ci<? string-ci<?)
	(cons 'string-ci=? string-ci=?)
	(cons 'string-ci>=? string-ci>=?)
	(cons 'string-ci>? string-ci>?)
	(cons 'string-copy
	      (case-lambic 1 (lambda (a1)
			       (string-copy a1))
			   2 (lambda (a1 a2)
			       (string-copy a1 a2))
			   3 (lambda (a1 a2 a3)
			       (string-copy a1 a2 a3))))
	(cons 'string-create
	      (case-lambic 1 (lambda (a1)
			       (string-create a1))
			   2 (lambda (a1 a2)
			       (string-create a1 a2))))
	(cons 'string-downcase string-downcase)
	(cons 'string-find-first-char string-find-first-char)
	(cons 'string-find-last-char string-find-last-char)
	(cons 'string-hash-32 string-hash-32)
	(cons 'string-hash-64 string-hash-64)
	(cons 'string-join-by string-join-by)
	(cons 'string-length string-length)
	(cons 'string-pad-center string-pad-center)
	(cons 'string-pad-left string-pad-left)
	(cons 'string-pad-right string-pad-right)
	(cons 'string-ref string-ref)
	(cons 'string-reverse string-reverse)
	(cons 'string-set! string-set!)
	(cons 'string-split-by string-split-by)
	(cons 'string-trim string-trim)
	(cons 'string-trim-left string-trim-left)
	(cons 'string-trim-right string-trim-right)
	(cons 'string-upcase string-upcase)
	(cons 'string/=? string/=?)
	(cons 'string<=? string<=?)
	(cons 'string<? string<?)
	(cons 'string=? string=?)
	(cons 'string>=? string>=?)
	(cons 'string>? string>?)
	(cons 'string? string?)
	(cons 'symbol->string symbol->string)
	(cons 'symbol=? symbol=?)
	(cons 'symbol? symbol?)
	(cons 'tan tan)
	(cons 'tanh tanh)
	(cons 'token-source-line token-source-line)
	(cons 'truncate truncate)
	(cons 'truncate-file
	      (case-lambic 1 (lambda (a1)
			       (truncate-file a1))
			   2 (lambda (a1 a2)
			       (truncate-file a1 a2))))
	(cons 'truncate-quotient truncate-quotient)
	(cons 'truncate-remainder truncate-remainder)
	(cons 'truncate/ truncate/)
	(cons 'unset-environment-variable unset-environment-variable)
	(cons 'upfrom upfrom)
	(cons 'vector vector)
	(cons 'vector->list vector->list)
	(cons 'vector-create
	      (case-lambic 1 (lambda (a1)
			       (vector-create a1))
			   2 (lambda (a1 a2)
			       (vector-create a1 a2))))
	(cons 'vector-fill! vector-fill!)
	(cons 'vector-for-each vector-for-each)
	(cons 'vector-length vector-length)
	(cons 'vector-ref vector-ref)
	(cons 'vector-set! vector-set!)
	(cons 'vector? vector?)
	(cons 'write-string write-string)
	(cons 'write-1str (case-lambic 1 (lambda (a1)
					   (write-1str a1))
				       2 (lambda (a1 a2)
					   (write-1str a1 a2))))
	(cons 'zero? zero?))))

(define (wile-environment-with-macros env)
  (wile-standard-environment))
