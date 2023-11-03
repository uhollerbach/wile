#!/home/uwe/tools/skeem
;;; -*- mode: scheme; -*-

;;; Usage: <prog> [-g] archive [-k] file1 file2 ... [-s] fileN ...
;;;
;;; If -g option is given, build debug version, otherwise non-debug;
;;; -g option must come first, this is not a general-purpose tool.
;;; If -k option is given, keep intermediate files, otherwise clean up.
;;; All files before a -s option are built in one compile, all files
;;; after are built split.

(define (ERR . strs)
  (apply write-string stderr strs)
  (exit 1))

(define (read-all-lines port)
  (let loop ((acc ()))
    (let ((line (read-line port)))
      (if line
	  (loop (cons line acc))
	  (list-reverse acc)))))

(define (run-cmd-or-die retry? str . strs)
  (let ((cmd (apply string-append str " " strs)))
    (unless (zero? (run-command cmd))
      (fprintf stderr "command %s failed!\n" cmd)
      (if retry?
	  (let ((cmv (string-append cmd " -v")))
	    (unless (zero? (run-command cmv))
	      (fprintf stderr "command %s failed!\n" cmv)
	      (if retry?
		  (let ((cmvv (string-append cmv " -v")))
		    (unless (zero? (run-command cmvv))
		      (fprintf stderr "command %s failed!\n" cmvv)
		      (exit 1)))
		  (exit 1))))
	  (exit 1)))))

(define (run-cmd str . strs)
  (let* ((cmd (apply string-append str " " strs))
	 (status (run-command cmd)))
    (unless (zero? status)
      (fprintf stderr "command '%s' failed with status %d\n" cmd status)
      (exit 1))))

(define (compile-single file dflag)
  (let* ((dix (string-find-last-char file #\.))
	 (prefix (if dix (string-copy file 0 dix) file))
	 (suffix (if dix (string-copy file dix) "")))
    (run-cmd-or-die #t "wile -o" (if dflag "-g " "") file)
    (string-append prefix ".o")))

(define (take-section cut-pattern lines)
  (let loop ((ls lines)
	     (acc ()))
    (cond ((null? ls)
	   (list (list-reverse acc) ()))
	  ((regex-match cut-pattern (car ls))
	   (list (list-reverse acc) (cdr ls)))
	  (else (loop (cdr ls) (cons (car ls) acc))))))

(define (compile-split file bld-dir dflag)
  (let* ((port (open-file file "r"))
	 (data1 (read-all-lines port))
	 (dix (string-find-last-char file #\.))
	 (prefix1 (if dix (string-copy file 0 dix) file))
	 (suffix (if dix (string-copy file dix) ""))
	 (cut-pat "(--8><--)+$")
	 (dh (take-section cut-pat data1))
	 (header (car dh))
	 (data2 (cadr dh))
	 (count 0))
    (close-port port)
    (when (null? data2)
      (write-string "there is no split directive!\n")
      (exit 1))
    (unless (or (string=? suffix ".scm") (string=? suffix ".c"))
      (write-string "error: don't know how to handle " suffix " files yet!\n")
      (exit 1))
    (let loop ((ls data2)
	       (ix 0))
      (if (null? ls)
	  (write-string "all done!\n")
	  (let* ((cstr (string-pad-left (number->string ix) #\0 6))
		 (prefix2 (string-append bld-dir "/" prefix1 "-"cstr))
		 (ofile (string-append prefix2 suffix))
		 (oport (open-file ofile "w"))
		 (sd (take-section cut-pat ls))
		 (section (car sd)))
	    (write-string "writing " ofile #\newline)
	    (for-each (lambda (l) (write-string oport l #\newline)) header)
	    (for-each (lambda (l) (write-string oport l #\newline)) section)
	    (flush-port oport)
	    (close-port oport)
	    (when (string=? suffix ".scm")
	      (run-cmd-or-die #t "wile -c " (if dflag "-g" "") " "
		       prefix2 ".scm " prefix2 ".c"))
	    (run-cmd-or-die #f "rm -f" bld-dir "/*.h")
	    (run-cmd-or-die #t "wile -o " (if dflag "-g" "") " "
		     prefix2 ".c " prefix2 ".o")
	    (loop (cadr sd) (+ ix 1)))))))

(let ((debug? #f)
      (split? #f)
      (clean? #t)
      (bld-dir "bld-rtl-dir")
      (archive #f)
      (objects #f))
  (when (and (not (null? command-line-arguments))
	     (string=? (car command-line-arguments) "-g"))
    (set! debug? #t)
    (set! command-line-arguments (cdr command-line-arguments)))
  (when (null? command-line-arguments)
    (ERR "error: no output archive file specified!\n"))
  (set! archive (car command-line-arguments))
  (set! command-line-arguments (cdr command-line-arguments))
  (when (directory-exists? bld-dir)
    (ERR "error: build directory '" bld-dir "' already exists!\n"))
  (set! objects
	(let loop ((fs command-line-arguments)
		   (acc ()))
	  (cond ((null? fs)
		 (list-reverse acc))
		((string=? (car fs) "-s")
		 (unless (create-directory bld-dir)
		   (ERR
		    "error: create-directory '" bld-dir "' failed:\n"
		    (describe-system-error (get-errno)) #\newline))
		 (set! split? #t)
		 (loop (cdr fs) acc))
		((string=? (car fs) "-k")
		 (set! clean? #f)
		 (loop (cdr fs) acc))
		(split?
		 (compile-split (car fs) bld-dir debug?)
		 (loop (cdr fs) acc))
		(else
		 (loop (cdr fs)
		       (cons (compile-single (car fs) debug?) acc))))))
  (let ((cmd (apply string-join-by " " "ar rcs" archive objects)))
    (when split?
      (set! cmd (string-append cmd " " bld-dir "/*.o")))
    (let ((status (run-command cmd)))
      (if (and (integer? status) (zero? status))
	  (write-string "archive created ok\n")
	  (ERR "archive create failed!\n"))))
  (when clean?
    (for-each (lambda (f)
		(write-string "remove-file '" f "' "
			      (if (remove-file f) "ok" "fail") "!\n"))
	      objects)
    (let ((status (run-command (string-append "rm -rf " bld-dir))))
      (if (and (integer? status) (zero? status))
	  (write-string "build directory successfully cleaned up\n")
	  (ERR "build directory cleanup failed!\n")))))
