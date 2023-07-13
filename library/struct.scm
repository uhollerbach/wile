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
  (let* ((fs (cons field fields))
	 (lfs (list-length fs))
	 (nfs (+ 1 lfs))
	 (J0 (curry string-join-by ""))
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
    defs))
