;;; -*- mode: scheme; -*-

;;; Wile -- the extremely stable scheming genius compiler *and* interpreter
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: GPLv3 or later, see file 'LICENSE' for details

(def-struct bbox name value)

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
			    (display
			     (cond ((define-form? expr)
				    (set! env (eval-define
					       (symbol=? (car expr) 'defmacro)
					       env (cdr expr)))
				    (get-bbox-value (car env)))
				   ((load-form? expr)
				    (let* ((fname (load-file-path
						   (symbol=? (car expr)
							     'load-library)
						   (eval env (cadr expr))))
					   (data (parse-file fname))
					   (ebox (make-bbox #f env))
					   (val (eval-begin ebox env data)))
				      (set! env (get-bbox-value ebox))
				      val))
				   ((begin-form? expr)
				    (let* ((ebox (make-bbox #f env))
					   (val (eval-begin
						 ebox env (cdr expr))))
				      (set! env (get-bbox-value ebox))
				      val))
				   (else (eval env expr))))
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
	   ((load-form? (car es))
	    (let* ((fname (load-file-path
			   (symbol=? (caar es) 'load-library)
			   (eval env (cadar es))))
		   (data (parse-file fname))
		   (ebox (make-bbox #f env))
		   (val (eval-begin ebox env data)))
	      (set! env (get-bbox-value ebox))
	      val))
	   ((begin-form? (car es))
	    (let* ((ebox (make-bbox #f env))
		   (val (eval-begin ebox env (cdar es))))
	      (set! env (get-bbox-value ebox))
	      val))
	   (else (eval env (car es))))
     (loop (cdr es)))))

(let* ((stdenv1
	(list-append
	 (list
	  (make-bbox 'command-name command-name)
	  (make-bbox 'command-line-arguments command-line-arguments))
	 (wile-standard-environment)))
       (std-env (wile-environment-with-macros stdenv1)))
  (if (null? command-line-arguments)
      (begin
	(write-string
	 ";;; Welcome to wile          https://github.com/uhollerbach/wile\n"
	 ";;; Copyright 2023 Uwe Hollerbach         available under GPLv3+\n")
	(for-each (lambda (v) (printf ";;;   %v\n" v)) (wile-build-info))
	(run-repl std-env))
      (run-batch std-env (car command-line-arguments))))
