;;; -*- mode: scheme; -*-

;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: GPLv3 or later, see file 'LICENSE' for details

(unless (and (get-environment-variable "WILE_CONFIG")
	     (get-environment-variable "WILE_INCLUDE_DIRECTORIES")
	     (get-environment-variable "WILE_LINK_DIRECTORIES")
	     (get-environment-variable "WILE_LINK_LIBRARIES")
	     (get-environment-variable "WILE_LIBRARY_PATH"))
  (write-string stderr "wile warning: environment is not fully set up\n"))

(unless (get-environment-variable "WILE_LIBRARY_PATH")
  (set-environment-variable "WILE_LIBRARY_PATH" "."))

;;; comment this out for closing the self-hosting loop
;;; (defmacro (load-library fname)
;;;   (letrec* ((paths (string-split-by
;;; 		    (lambda (c) (eqv? c #\:))
;;; 		    (get-environment-variable "WILE_LIBRARY_PATH")))
;;; 	    (find (lambda (ps)
;;; 		    (if (null? ps)
;;; 			#f
;;; 			(let* ((dir (car ps))
;;; 			       (fp (if (string=? dir ".")
;;; 				       fname
;;; 				       (string-join-by "/" dir fname))))
;;; 			  (if (file-exists? fp)
;;; 			      fp
;;; 			      (find (cdr ps)))))))
;;; 	    (filepath (find paths)))
;;; 	   (if filepath
;;; 	       `(load ,filepath)
;;; 	       `(write-string "unable to find file '" ,fname "'\n"))))

(define (is-colon? c) (char=? c #\:))

(define string-hash string-hash-64)
(define (symbol-hash sym) (string-hash (symbol->string sym)))

(load-library "hash.scm")
;;; comment this out for closing the self-hosting loop
;;; (load-library "struct.scm")
(load-library "arg-parse.scm")
(load-library "wile-comp.scm")

(define (infer-file-type fname)
  (let ((pix (string-find-last-char fname #\.)))
    (if pix
	(let ((suffix (string-copy fname pix)))
	  (cond ((string=? suffix ".scm") 0)
		((string=? suffix ".c") 1)
		((string=? suffix ".o") 2)
		(else 3)))
	3)))

(define (compile-s2c do-debug opt-level input output keep-int fvals)
  (let* ((sepos (string-find-last-char output #\/))
	 (out-path (if sepos (string-copy output 0 (+ sepos 1)) ""))
	 (out-port (open-file output "w+")))
    (when (positive? global-verbose)
      (write-string "#### compiling " input " to " output #\newline))
    (compile-file do-debug opt-level input out-path out-port fvals)
    (unless (zero? global-errors)
      (write-string stderr "compilation failed!\n")
      (exit 1))))

(define (get-env-val env-var def-val)
  (let ((val (get-environment-variable env-var)))
    (if val val def-val)))

(define cc-default "gcc")

(define cf-default "-ansi -std=c11 -Wall -Wstrict-prototypes -Wmissing-prototypes -Winline -Wpointer-arith -Wshadow -Wnested-externs -Wformat-security -Wunused -Wsign-compare -D_DEFAULT_SOURCE")

;;; currently turned off: -DWILE_USES_RC4_RAND

(define wc-default "-DWILE_USES_LONG_INT -DWILE_USES_LONG_DOUBLE")

(define (debug-filter-wc wc do-debug)
  (if do-debug
      (string-join-by
       " " (cons "-g" (filter (lambda (s) (not (string=? s "-DWILE_USES_GC")))
			      (string-split-by char-whitespace? wc))))
      wc))

(define (colon-split-string val)
  (if (or (not val) (null? val) (string=? val ""))
      () (string-split-by is-colon? val)))

(define (compile-c2o do-debug opt-level input output)
  (let* ((cc (get-env-val "CC" cc-default))
	 (cf (get-env-val "CFLAGS" cf-default))
	 (wc (get-env-val "WILE_CONFIG" wc-default))
	 (wi (get-env-val "WILE_INCLUDE_DIRECTORIES" ()))
	 (wis (colon-split-string wi))
	 (cmd (string-join-by " " cc cf (debug-filter-wc wc do-debug)
			      (string-append "-O" (number->string opt-level))))
	 (status #f))
    (for-each (lambda (d) (set! cmd (string-append cmd " " "-I" d))) wis)
    (set! cmd (string-join-by " " cmd "-c" input "-o" output))
    (when (positive? global-verbose)
      (write-string "#### " cmd #\newline))
    (unless (zero? (run-command cmd))
      (write-string stderr "compilation failed!\n")
      (exit 1))))

(define (compile-o2x do-debug opt-level input output)
  (let* ((cc (get-env-val "CC" cc-default))
	 (cf (get-env-val "CFLAGS" cf-default))
	 (wc (get-env-val "WILE_CONFIG" wc-default))
	 (wld (get-env-val "WILE_LINK_DIRECTORIES" ()))
	 (wll (get-env-val "WILE_LINK_LIBRARIES" ()))
	 (wlds (colon-split-string wld))
	 (wlls (colon-split-string wll))
	 (rtlib (if do-debug "wrtl-dbg" "wrtl"))
	 (cmd (string-join-by " " cc cf  (debug-filter-wc wc do-debug)
			      (string-append "-O" (number->string opt-level))))
	 (status #f))
    (set! wlls (append (list rtlib) wlls (list "pthread" "m")))
    (for-each (lambda (d) (set! cmd (string-append cmd " " "-L" d))) wlds)
    (set! cmd (string-join-by " " cmd input "-o" output))
    (for-each (lambda (d) (set! cmd (string-append cmd " " "-l" d))) wlls)
    (when (positive? global-verbose)
      (write-string "#### " cmd #\newline))
    (unless (zero? (run-command cmd))
      (write-string stderr "link failed!\n")
      (exit 1))))

;;; TODO: this is nice and all, but it destroys the ability to compare files;
;;; they will all have different embedded timestamps, and that will kill MD5
;;; (defmacro (compile-time)
;;;   (let ((now (list-head (UTCtime) 6)))
;;;     `(list ,@now)))

(let* ((fvals (parse-command-line-flags
	       '("-v" flags "provide more details of compilation process")
	       '("-P" flag "show brief hint list of primitives")
	       '("-D" flag "show undocumented primitives")
	       '("-Q" strings "show doc-strings for specified primitives")
	       '("-V" flag "show compiler version and configuration")
	       '("-c" flag "compile to c")
	       '("-o" flag "compile to object")
	       '("-p" flag "do profiling: count function invocations")
	       '("-x" flag "compile/link to executable")
	       '("-g" flag "turn on debugging")
	       '("-O" int "set optimization level to specified value")
	       '("-T" int "set tail call #args")
	       '("-k" flag "keep intermediate files")
	       '("-rm-dc" flag "suppress dead-code removal")
	       '("-rm-ul" flag "suppress unused-labels removal")
	       '("-rm-uv" flag "suppress unused-variables removal")
	       "Other arguments are the (required) input file, which must end in one of"
	       ""
	       "    .scm\tfor scheme files"
	       "    .c\t\tfor c files"
	       "    .o\t\tfor object files"
	       ""
	       "The output file is optional; if specified, it must come *after* the"
	       "input file, unlike some other compilers. If it is specified, wile will"
	       "use it; if not, wile will infer it from the input filename and the"
	       "specified compilation flag -c -o or -x: foo.scm -> foo.c -> foo.o -> foo"))
       (compile-to-c (hash-table-ref fvals "-c" #f))
       (compile-to-o (hash-table-ref fvals "-o" #f))
       (compile-to-x (hash-table-ref fvals "-x" #f))
       (doc-prims (hash-table-ref fvals "-Q" ()))
       (keep-int (hash-table-ref fvals "-k" #f))
       (opt-level (hash-table-ref fvals "-O" #f))
       (do-debug (hash-table-ref fvals "-g" #f))
       (targs (hash-table-ref fvals "-T" 0))
       (input-file #f)
       (input-type #f)
       (input-prefix #f)
       (output-file #f)
       (output-type #f))
  (let ((wc (get-env-val "WILE_CONFIG" wc-default)))
    (unless (null? (filter (lambda (s) (string=? s "-g"))
			   (string-split-by char-whitespace? wc)))
      (set! do-debug #t)))
  (when (hash-table-ref fvals "-V" #f)
    (write-string
     ";;; Wile, the extremely stable scheming genius compiler\n"
     ";;; Wile is available from https://github.com/uhollerbach/wile\n"
     ";;; under GPLv3 or later for the compiler, LGPLv3 or later for the RTL\n"
     ";;; Copyright 2023, Uwe Hollerbach\n")
    (for-each (lambda (v)
		(display v)
		(newline))
	      (wile-build-info #t))
;;;    (display `(wile-compiled-on ,(compile-time)))
;;;    (newline)
    (exit 0))
  (when (hash-table-ref fvals "-P" #f)
    (show-prims-table)
    (exit 0))
  (when (hash-table-ref fvals "-D" #f)
    (show-undoc)
    (exit 0))
  (unless (null? doc-prims)
    (for-each (lambda (p)
		(write-string p #\newline #\tab
			      (lookup-doc-string (string->symbol p))
			      #\newline #\newline))
	      doc-prims)
    (exit 0))
  (unless opt-level
    (set! opt-level (if do-debug 0 3)))
  (set! global-tc-min-args (max global-tc-min-args targs))
  (unless (or (and compile-to-c (not compile-to-o) (not compile-to-x))
	      (and (not compile-to-c) compile-to-o (not compile-to-x))
	      (and (not compile-to-c) (not compile-to-o) compile-to-x)
	      (and (not compile-to-c) (not compile-to-o) (not compile-to-x)))
    (write-string
     stderr "wile error: must specify at most exactly one of -c -o -x\n")
    (exit 1))
  (when (null? command-line-arguments)
    (write-string stderr "wile error: must specify input file!\n")
    (exit 1))
  (set! input-file (car command-line-arguments))
  (set! input-type (infer-file-type input-file))
  
  (when (string=? input-file "-h")
    (write-string
     "Minimal usage: " command-name " {-c|-o|-x} input-file" #\newline
     "For more help, run " command-name " -help" #\newline)
    (exit 0))

  (unless (or (= input-type 0) (= input-type 1) (= input-type 2))
    (write-string stderr "wile error: unknown input file type!\n")
    (exit 1))

  (let* ((pix (string-find-last-char input-file #\/))
	 (tmp (string-copy input-file (if pix (+ pix 1) 0))))
    (set! input-prefix
	  (string-copy tmp 0 (string-find-last-char tmp #\.))))

  (set! output-file
	(if (null? (cdr command-line-arguments))
	    (cond (compile-to-c (string-append input-prefix ".c"))
		  (compile-to-o (string-append input-prefix ".o"))
		  (else input-prefix))
	    (cadr command-line-arguments)))

  (set! output-type (infer-file-type output-file))
  (cond (compile-to-c (set! output-type 1))
	(compile-to-o (set! output-type 2))
	(compile-to-x (set! output-type 3)))

  (unless (= output-type (infer-file-type output-file))
    (write-string "warning: output specified by flags does not match inferred output type\n"))

  (when (> input-type output-type)
    (write-string stderr "wile is a compiler, not a decompiler!\n")
    (exit 1))

  (when (= input-type output-type)
    (write-string stderr "wile is a compiler, not a copier!\n")
    (exit 1))

  (when (string=? input-file output-file)
    (write-string stderr "wile refuses to overwrite its input file\n")
    (exit 1))

  (unless (file-exists? input-file)
    (write-string stderr "wile cannot find input file!\n")
    (exit 1))

  (set! global-verbose (hash-table-ref fvals "-v" 0))

  (let* ((intermediates (cond ((and (= input-type 0) (= output-type 3))
			       (list (string-append input-prefix "-int.c")
				     (string-append input-prefix "-int.o")))
			      ((and (= input-type 0) (= output-type 2))
			       (list (string-append input-prefix "-int.c")))
			      ((and (= input-type 1) (= output-type 3))
			       (list (string-append input-prefix "-int.o")))
			      (else ())))
	 (flist (append (cons input-file intermediates) (list output-file))))
    (while (< input-type output-type)
	   (cond ((= input-type 0)
		  (compile-s2c do-debug opt-level
			       (car flist) (cadr flist) keep-int fvals))
		 ((= input-type 1)
		  (compile-c2o do-debug opt-level
			       (car flist) (cadr flist)))
		 ((= input-type 2)
		  (compile-o2x do-debug opt-level
			       (car flist) (cadr flist)))
		 (else (raise "wile internal error! bad compilation phase")))
	   (set! input-type (+ input-type 1))
	   (set! flist (cdr flist)))
    (unless keep-int
      (for-each (lambda (f) (remove-file f)) intermediates))))
