;;; -*- mode: scheme; -*-

;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: GPLv3 or later, see file 'LICENSE' for details

(define string-hash string-hash-64)

(define global-config ())

(define (get-config-val key)
  (let ((kv (assv key global-config)))
    (if kv
	(cadr kv)
	(ERR #f "key '%s' was not found in config!" key))))

(load-library "hash.scm")
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

;;; Process the specified config file

(define (setup-config-from file paths)
  (when (positive? global-verbose)
    (write-string "#### config is " file "\n"))
  (let ((data (read-all file))
	(multi-string? (lambda (val)
			 (all-true? (map string? val))))
	(check-item (lambda (key test)
		      (let ((cv (get-config-val key)))
			(unless (and cv (test cv))
			  (ERR #f "bad config entry '%s' -> %v" key cv))))))
    (set! global-config
	  (map (lambda (kv)
		 (if (and (pair? kv)
			  (symbol? (car kv))
			  (symbol=? (car kv) 'scheme-include-directories))
		     (list (car kv) (list-append paths (cadr kv)))
		     kv))
	       data))
    (check-item 'c-compiler string?)
    (check-item 'c-compiler-flags string?)
    (check-item 'c-include-directories multi-string?)
    (check-item 'c-link-directories multi-string?)
    (check-item 'c-link-libraries multi-string?)
    (check-item 'scheme-include-directories multi-string?)
    (check-item 'wile-config multi-string?)
    (let* ((cv (get-config-val 'scheme-include-directories))
	   (sv (if (null? cv)
		   "."
		   (string-join-by ";" cv))))
      (set-environment-variable "WILE_LIBRARY_PATH" sv))))

(define (write-build-info)
  (let* ((conf1 (wile-build-info))
	 (ftype (cadr (assv 'float-type conf1)))
	 (itype (cadr (assv 'integer-type conf1)))
	 (gc? (cadr (assv 'garbage-collector-version conf1)))
	 (sqlite? (cadr (assv 'sqlite-version conf1)))
	 (libs ())
	 (conf2 ())
	 (add-quotes (lambda (s) (string-append "\"" s "\""))))
    ;;; don't mess with the order of these, gc has to come last in libs
    ;;; which means it has to get processed first
    (when gc?
      (set! libs (cons "gc" libs))
      (set! conf2 (cons "-DWILE_USES_GC" conf2)))
    (cond ((eqv? ftype 'quad-double)
	   (set! libs (cons "quadmath" libs))
	   (set! conf2 (cons "-DWILE_USES_QUAD_DOUBLE" conf2)))
	  ((eqv? ftype 'long-double)
	   (set! conf2 (cons "-DWILE_USES_LONG_DOUBLE" conf2)))
	  (else
	   (set! conf2 (cons "-DWILE_USES_DOUBLE" conf2))))
    (cond ((eqv? itype 'int-128)
	   (set! conf2 (cons "-DWILE_USES_INT128" conf2)))
	  (else
	   (set! conf2 (cons "-DWILE_USES_LONG_INT" conf2))))
    (when sqlite?
      (set! libs (cons "sqlite3" libs))
      (set! conf2 (cons "-DWILE_USES_SQLITE" conf2)))
    (set! libs (map add-quotes libs))
    (set! conf2 (map add-quotes conf2))
    (printf "(c-link-libraries %v)\n(wile-config %v)\n" libs conf2)))

;;; Check for config files in the order command-line, env-var, baked-in, cwd;
;;; use the first one that's found

(define (setup-configuration cmd-line-file paths)
  (let ((baked-in-file (wile-config-file))
	(env-file (get-environment-variable "WILE_CONFIG_FILE"))
	(local-file "wile-config.dat"))
    (cond ((and cmd-line-file (file-exists? cmd-line-file))
	   (setup-config-from cmd-line-file paths))
	  ((and env-file (file-exists? env-file))
	   (setup-config-from env-file paths))
	  ((and baked-in-file (file-exists? baked-in-file))
	   (setup-config-from baked-in-file paths))
	  ((file-exists? local-file)
	   (setup-config-from local-file paths))
	  (else (write-string stderr "no compiler configuration file found!\n")
		(exit 1)))))

(define (compile-s2c do-debug opt-level input output keep-int fvals)
  (let* ((sepos (string-find-last-char output #\/))
	 (out-path (if sepos (string-copy output 0 (+ sepos 1)) ""))
	 (out-port (open-file output "w+")))
    (when (positive? global-verbose)
      (write-string "#### compiling " input " to " output #\newline))
    (compile-file do-debug opt-level input out-path out-port fvals)
    (when global-errors
      (write-string stderr "compilation failed!\n")
      (exit 1))))

;;; currently turned off: -DWILE_USES_RC4_RAND

(define (debug-filter-wc wc do-debug)
  (string-join-by
   " "
   (if do-debug
       (cons "-g" (filter (lambda (s) (not (string=? s "-DWILE_USES_GC"))) wc))
       wc)))

(define (compile-c2o do-debug opt-level do-cprof input output)
  (let* ((cc (get-config-val 'c-compiler))
	 (cf (get-config-val 'c-compiler-flags))
	 (wc (get-config-val 'wile-config))
	 (wis (get-config-val 'c-include-directories))
	 (cmd (string-join-by
	       " " cc cf (debug-filter-wc wc do-debug) (if do-cprof "-pg" "")
	       (string-append "-O" (number->string opt-level))))
	 (status #f))
    (for-each (lambda (d) (set! cmd (string-append cmd " " "-I" d))) wis)
    (set! cmd (string-join-by " " cmd "-c" input "-o" output))
    (when (positive? global-verbose)
      (write-string "#### " cmd #\newline))
    (unless (zero? (run-command cmd))
      (write-string stderr "compilation failed!\n")
      (exit 1))))

(define (compile-o2x do-debug opt-level do-cprof input output)
  (let* ((cc (get-config-val 'c-compiler))
	 (cf (get-config-val 'c-compiler-flags))
	 (wc (get-config-val 'wile-config))
	 (wlds (get-config-val 'c-link-directories))
	 (wlls (get-config-val 'c-link-libraries))
	 (rtlib (cond (do-debug "wrtl-dbg")
		      (do-cprof "wrtl-pg")
		      (else "wrtl")))
	 (cmd (string-join-by
	       " " cc cf  (debug-filter-wc wc do-debug) (if do-cprof "-pg" "")
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
	       '("-CF" string "specify compiler configuration file")
	       '("-L" strings "specify more scheme source search directories")
	       '("-wr-conf" flag "initialize default config file")
	       '("-c" flag "compile to c")
	       '("-o" flag "compile to object")
	       '("-x" flag "compile/link to executable")
	       '("-g" flag "turn on debugging")
	       '("-p" flag "turn on profiling")
	       '("-O" int "set optimization level to specified value")
	       '("-T" int "set tail call #args")
	       '("-k" flag "keep intermediate files")
	       '("-rm-dc" flag "suppress dead-code removal")
	       '("-rm-uf" flag "suppress unused-functions removal")
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
       (more-paths (hash-table-ref fvals "-L" ()))
       (doc-prims (hash-table-ref fvals "-Q" ()))
       (keep-int (hash-table-ref fvals "-k" #f))
       (opt-level (hash-table-ref fvals "-O" #f))
       (do-debug (hash-table-ref fvals "-g" #f))
       (do-cprof (hash-table-ref fvals "-p" #f))
       (targs (hash-table-ref fvals "-T" 0))
       (input-file #f)
       (input-type #f)
       (input-prefix #f)
       (output-file #f)
       (output-type #f))
  (when (hash-table-ref fvals "-wr-conf" #f)
    (write-build-info)
    (exit 0))
  (when (hash-table-ref fvals "-V" #f)
    (write-string
     ";;; Wile, the extremely stable scheming genius compiler\n"
     ";;; Wile is available from https://github.com/uhollerbach/wile\n"
     ";;; under GPLv3 or later for the compiler, LGPLv3 or later for the RTL\n"
     ";;; Copyright 2023, Uwe Hollerbach\n")
    (for-each (lambda (v)
		(display v)
		(newline))
	      (wile-build-info))
;;;    (display `(wile-compiled-on ,(compile-time)))
;;;    (newline)
    (exit 0))
  (set! global-verbose (hash-table-ref fvals "-v" 0))
  (setup-configuration (hash-table-ref fvals "-CF" #f) more-paths)
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
  (unless (null? (filter (lambda (s) (string=? s "-g"))
			 (get-config-val 'wile-config)))
    (set! do-debug #t))
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
		  (compile-c2o do-debug opt-level do-cprof
			       (car flist) (cadr flist)))
		 ((= input-type 2)
		  (compile-o2x do-debug opt-level do-cprof
			       (car flist) (cadr flist)))
		 (else (raise "wile internal error! bad compilation phase")))
	   (set! input-type (+ input-type 1))
	   (set! flist (cdr flist)))
    (unless keep-int
      (for-each (lambda (f) (remove-file f)) intermediates))))
