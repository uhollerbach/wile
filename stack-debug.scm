;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: GPLv3 or later, see file 'LICENSE' for details

;;; A small program to decipher stack traces to map as
;;; nearly as possible onto the original scheme source

;;; usage: <me> stack-trace-file [c-source-file]

(load-library "hash.scm")

(define (read-all-lines port)
  (flush-port port)
  (set-file-position port 0 'start)
  (let loop ((acc ()))
    (let ((line (read-line port)))
      (if line
	  (loop (cons line acc))
	  (list-reverse acc)))))

(define (filter-stack-trace data)
  (let loop ((ls data)
	     (acc ())
	     (do-acc #f))
    (cond ((null? ls)
	   acc)
	  ((string=? (car ls) "wile stack trace begin")
	   (loop (cdr ls) acc #t))
	  ((string=? (car ls) "wile stack trace end")
	   (loop (cdr ls) acc #f))
	  (do-acc
	   (loop (cdr ls) (cons (car ls) acc) #t))
	  (else
	   (loop (cdr ls) acc #f)))))

;;; This was originally display-stack-trace from the rtl; I'm not
;;; sure yet about that interface, so this is for experimenting

(define (translate-stack-trace trace-data)
  (let* ((exe-name #f)
	 (data2 (filter
		 (lambda (x) x)		;;; filter out false values
		 (map (lambda (l)
			(let ((m (regex-match "\\(\\+0x[0-9a-fA-F]+\\)" l)))
			  (when m
			    (unless exe-name
			      (set! exe-name (car m)))
			    (set! m (string-copy (cadr m) 2))
			    (set! m (string-copy m 0 (- (string-length m) 1))))
			  m))
		      trace-data))))
    (let loop ((port (run-read-command (apply string-join-by " "
					      "addr2line -f -p -e"
					      exe-name "-a" data2)))
	       (acc ()))
      (let ((line (read-line port)))
	(if line
	    (let* ((vs1 (string-split-by char-whitespace? line))
		   (vs2 (filter (lambda (s) (string/=? s "at")) vs1)))
	      (loop port (cons vs2 acc)))
	    (begin
	      (close-port port)
	      (list-reverse acc)))))))

;;; Take a comment line from the emitted C source that maps scheme
;;; functions with line number to C functions, and split it into the
;;; important pieces

(define at-reg (char->string #\space #\* #\@ #\@ #\@ #\space #\*))

(define (at-split line)
  (let loop ((lc line)
	     (acc ()))
    (let ((spl (regex-match at-reg lc)))
      (if spl
	  (loop (caddr spl) (cons (car spl) acc))
	  (list-reverse acc)))))

(define debug #f)
(when (and (not (null? command-line-arguments))
	   (string=? (car command-line-arguments) "-d"))
  (set! debug #t)
  (set! command-line-arguments (cdr command-line-arguments)))

;;; Main program: create the C->scheme map, if the c-source-file was
;;; given, then read the stack trace, do stage-1 translation through
;;; translate-stack-frame, above, and then if applicable do stage 2
;;; translation using fmap

(let ((fmap (hash-table-create string-hash-64 string=?)))
  (when (null? command-line-arguments)
    (write-string stderr command-name " stack-trace-file [c-source-file]\n")
    (exit 1))
  (unless (null? (cdr command-line-arguments))
    ;;; need to split up the three '@', otherwise there is a foot-gun
    (when debug
      (write-string "# scanning source file\n"))
    (let ((port (run-read-command
		 (string-append
		  "grep \"@" "@" "@\" " (cadr command-line-arguments)))))
;;;    (let ((port (open-file (cadr command-line-arguments) "r")))
      (let loop ()
	(let ((line (read-line port)))
	  (flush-port)
	  (if line
	      (let* ((at-map (at-split line))
		     (s-fn (cadr at-map))
		     (s-loc (caddr at-map))
		     (c-fn (cadddr at-map)))
		(hash-table-set! fmap c-fn
				 (string-append s-fn " :: " s-loc))
		(loop))
	      (close-port port))))))
  (let* ((port (open-file (car command-line-arguments) "r"))
	 (lines (read-all-lines port))
	 (filts (filter-stack-trace lines))
	 (stak1 (translate-stack-trace filts)))
    (close-port port)
    (when debug
      (for-each (lambda (v) (display v) (newline)) stak1))
    (for-each (lambda (v)
		(let ((val (hash-table-ref fmap (cadr v) #f)))
		  (if val
		      (write-string (car v) " # " val #\newline)
		      (write-string (car v) "   " (cadr v) " :: "
				    (caddr v) #\newline))))
	      stak1)))
