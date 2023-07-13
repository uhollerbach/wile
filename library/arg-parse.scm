;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; Command-line flags processor
;;;
;;; (parse-command-line-flags . opt-specs)
;;;
;;; Process global variable command-line-arguments to extract options and
;;; store their arguments in a hash which is the return value of the
;;; routine, and leave remaining non-option command line arguments in
;;; the suitably modified command-line-arguments global variable.
;;; See example below.
;;;
;;; opt-specs are of the form (flag-name arg-type help-string default-value)
;;; -help automagically gets added; -- automagically ends options.
;;; Additionally, any number of strings can be added in, in any order,
;;; among the opt-specs; these strings will be printed out in the order of
;;; their appearance at the end of any help output.
;;;
;;; flag-name is a string - for example "-spleen"
;;;
;;; arg-type: a symbol specifying the type of the arg. some options are
;;; repeatable, some are not.
;;;
;;;     non-repeatable
;;;     s | string	=> string arg
;;;     i | int		=> int arg
;;;     r | real	=> real arg
;;;     b | bool	=> true/false arg
;;;     f | flag	=> no arg, just presence or absence of flag
;;;
;;;     repeatable
;;;     ss | strings	=> string arg
;;;     is | ints	=> int arg
;;;     rs | reals	=> real arg
;;;     bs | bools	=> true/false arg
;;;     fs | flags	=> no arg, just presence or absence of flag
;;;
;;; help-string is a string - for example "specify the size of the spleen"
;;;
;;; default-value is an optional default to be used if this flag is not
;;; specified by the user. Only non-repeating flags can have default values.
;;;
;;; The routine returns a hash containing all flags that were specified on
;;; the command line or that were set up with default values. The flag is
;;; the key in the hash, its argument is the value associated with that key.
;;; Flag options are different; the value stored in the hash is either #t/#f
;;; for a singleton, or the number of times it was seen for a repeatable.
;;;
;;;     s		=> specified string
;;;     i		=> specified int
;;;     r		=> specified real
;;;     b		=> specified bool
;;;     f		=> #t or #f
;;;
;;;     ss		=> array of specified strings
;;;     is		=> array of specified ints
;;;     rs		=> array of specified reals
;;;     bs		=> array of specified bools
;;;     fs		=> count of how many times this flag was given
;;;
;;; TODO items:
;;; * codelets? when some option is seen, run a bit of code
;;; * handle multiple values with one option?
;;;   for example "-coord cx cy" where cx and cy are numbers

;;; Example:
;;;
;;; (define fvals (parse-command-line-flags
;;; 	       '("-s1d" string "1 string with default" "s1def")
;;; 	       '("-s1n" string "1 string no default")
;;; 	       '("-s2" strings "multiple strings")
;;; 	       '("-i1d" int "1 int with default" 177)
;;; 	       '("-i1n" int "1 int no default")
;;; 	       '("-i2" ints "multiple ints")
;;; 	       (list "-r1d" 'real "1 real with default" pi)
;;; 	       '("-r1n" real "1 real no default")
;;; 	       '("-r2" reals "multiple reals")
;;; 	       '("-b1d" bool "1 bool with default" #t)
;;; 	       '("-b1n" bool "1 bool no default")
;;; 	       '("-b2" bools "multiple bools")
;;; 	       '("-f1" flag "1 flag")
;;; 	       '("-f2" flags "multiple flags")))
;;;
;;; Some options are specified as defaultable - the ones with 'd' at the end.
;;; Some options are specified as repeatable - the ones with '2' in the name.
;;; Given a command line (note presence of '--' toward the end)
;;;
;;;     <script> foo -s1d given -i1n 15 bar -i2 2 -i2 3 -i2 5 -i2 7 -f1 \
;;;              -f2 baz -f2 -f2 -- -i2 11 -i2 13
;;;
;;; the hash returned by parse-command-line-flags will contain
;;;
;;;     -b1d	=> #t
;;;     -f1	=> #t
;;;     -f2	=> 3
;;;     -i1d	=> 177
;;;     -i1n	=> 15
;;;     -i2	=> (2 3 5 7)
;;;     -r1d	=> 3.14159265358979323846264338327950280e+00
;;;     -s1d	=> given
;;;
;;; and the remaining command-line-arguments will be
;;;
;;;     ("foo" "bar" "baz" "-i2" "11" "-i2" "13")

;;; (load-library "hash.scm")
;;; (load-library "struct.scm")

(def-struct opt-rec type repeat help state value)

 ;;; TODO: when printing -help message, need to show default if specified

(define (parse-command-line-flags . opt-specs)
  (let* ((unseen 'unseen)

	 (seen 'seen)

	 (type-check
	  (lambda (tpred v nm)
	    (unless (or (null? v) (tpred v))
	      (raise (sprintf "default '%s' is the wrong type for flag '%s'"
			      v nm)))))

	 (no-default-check
	  (lambda (val name)
	    (unless (null? val)
	      (raise (sprintf "no default allowed for flag '%s'" name)))))

	 (build-opt-rec
	  (lambda (osh sp)
	    (let* ((name (car sp))
		   (type (cadr sp))
		   (hstr (caddr sp))
		   (dval (if (null? (cdddr sp)) () (cadddr sp)))
		   (state (if (null? (cdddr sp)) unseen 'def))
		   (tval (case type
			   ((s string)
			    (type-check string? dval name)
			    (make-opt-rec 'string #f hstr state dval))
			   ((ss strings)
			    (no-default-check dval name)
			    (make-opt-rec 'string #t hstr unseen ()))
			   ((i int)
			    (type-check integer? dval name)
			    (make-opt-rec 'int #f hstr state dval))
			   ((is ints)
			    (no-default-check dval name)
			    (make-opt-rec 'int #t hstr unseen ()))
			   ((r real)
			    (type-check real? dval name)
			    (make-opt-rec 'real #f hstr state dval))
			   ((rs reals)
			    (no-default-check dval name)
			    (make-opt-rec 'real #t hstr unseen ()))
			   ((b bool)
			    (type-check boolean? dval name)
			    (make-opt-rec 'bool #f hstr state dval))
			   ((bs bools)
			    (no-default-check dval name)
			    (make-opt-rec 'bool #t hstr unseen ()))
			   ((f flag)
			    (no-default-check dval name)
			    (make-opt-rec 'flag #f hstr unseen #f))
			   ((fs flags)
			    (no-default-check dval name)
			    (make-opt-rec 'flag #t hstr unseen 0))

			   (else
			    (raise
			     (sprintf "unknown type '%s' specified for '%s'"
				      type name))))))
	      (if (hash-table-contains? osh name)
		  (raise (sprintf "duplicate specification of '%s' flag" name))
		  (hash-table-set! osh name tval)))))

	 (set-val
	  (lambda (name orec conv-val)
	    (let ((arg (conv-val
			(if (null? command-line-arguments)
			    (raise
			     (sprintf "no arguments left for '%s'!" name))
			    (let ((arg (car command-line-arguments)))
			      (set! command-line-arguments
				    (cdr command-line-arguments))
			      arg)))))
	      (cond ((get-opt-rec-repeat orec)
		     (set-opt-rec-value!
		      orec (cons arg (get-opt-rec-value orec))))
		    ((eqv? (get-opt-rec-state orec) seen)
		     (raise (sprintf "'%s' was already specified" name)))
		    (else
		     (set-opt-rec-value! orec arg)))
	      (set-opt-rec-state! orec seen))))

	 (identity (lambda (x) x))

	 (conv-bool
	  (lambda (x)
	    (cond ((or (string-ci=? x "#f") (string-ci=? x "false")
		       (string-ci=? x "no") (string-ci=? x "0"))
		   #f)
		  ((or (string-ci=? x "#t") (string-ci=? x "true")
		       (string-ci=? x "yes") (string-ci=? x "1"))
		   #t)
		  (else (raise (sprintf "bad boolean value '%s'" x))))))

	 (conv-int
	  (lambda (x)
	    (let ((i (string->number x)))
	      (if (integer? i)
		  i
		  (raise (sprintf "non-integer value '%s'" x))))))

	 (conv-real
	  (lambda (x)
	    (let ((r (string->number x)))
	      (if (real? r)
		  r
		  (raise (sprintf "non-real value '%s'" x))))))

	 (set-flag
	  (lambda (name orec)
	    (cond ((get-opt-rec-repeat orec)
		   (set-opt-rec-value! orec (+ 1 (get-opt-rec-value orec))))
		  ((eqv? (get-opt-rec-state orec) seen)
		   (raise (sprintf "'%s' was already specified" name)))
		  (else
		   (set-opt-rec-value! orec #t)))
	    (set-opt-rec-state! orec seen)))

	 (update-opt-rec
	  (lambda (name orec)
	    (case (get-opt-rec-type orec)
	      ((bool) (set-val name orec conv-bool))
	      ((int) (set-val name orec conv-int))
	      ((real) (set-val name orec conv-real))
	      ((string) (set-val name orec identity))
	      ((flag) (set-flag name orec))
	      (else (raise (sprintf "internal error! bad opt-rec type %v"
				    (get-opt-rec-type orec)))))))

	 (finish-opt-rec
	  (lambda (osh kv)
	    (let ((orec (cdr kv)))
	      (if (eqv? (get-opt-rec-state orec) unseen)
		  (hash-table-delete! osh (car kv))
		  (let ((val (get-opt-rec-value orec)))
		    (when (pair? val)
		      (set! val (list-reverse val)))
		    (hash-table-set! osh (car kv) val))))))

	 (print-opt-rec
	  (lambda (osh name show-state)
	    (let* ((orec (hash-table-ref osh name #f))
		   (type (symbol->string (get-opt-rec-type orec))))
	      (when (get-opt-rec-repeat orec)
		(set! type (string-join-by "" type "s")))
	      (printf "    %l10s\t%l10s\t%s\n" name type (get-opt-rec-help orec))
	      (when show-state
		(printf "    %v\t%v\n\n"
			(get-opt-rec-state orec)
			(get-opt-rec-value orec)))))))
    (unless (null? opt-specs)
      (let ((osh (make-hash-table string-hash-64 string=?))
	    (info-strs (filter string? opt-specs))
	    (new-args ()))
	(for-each (lambda (sp) (when (pair? sp) (build-opt-rec osh sp)))
		  opt-specs)
	(until (null? command-line-arguments)
	       (let ((cur (car command-line-arguments)))
		 (set! command-line-arguments (cdr command-line-arguments))
		 (cond ((string=? cur "-help")
			(printf "Usage: %s [options] other-args\n"
				command-name)
			(write-string "where options may contain\n\n"
				      "    name\ttype\t\tnote\n\n")
			(for-each (lambda (nm)
				    (print-opt-rec osh nm #f))
				  (list-sort string<? (hash-table-keys osh)))
			(write-string
			 #\newline
			 "Or  -help to show this message" #\newline
			 "Options cannot be combined: -abc does not mean the same as -a -b -c" #\newline)
			(unless (null? info-strs)
			  (newline)
			  (for-each (lambda (s) (write-string s #\newline))
				    info-strs))
			(exit 0))
		       ((string=? cur "--")
			(set! new-args (append (reverse command-line-arguments)
					       new-args))
			(set! command-line-arguments ()))
		       ((hash-table-contains? osh cur)
			(update-opt-rec cur (hash-table-ref osh cur #f)))
		       (else (set! new-args (cons cur new-args))))))
	(set! command-line-arguments (reverse new-args))
	(for-each (lambda (kv) (finish-opt-rec osh kv))
		  (hash-table-entries osh))
	osh))))
