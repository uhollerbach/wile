#!/home/uwe/tools/skeem

;;; usage: <me> test-loc

(define tname "twp-executable")

(define show-diffs? #f)
(define compile-only? #f)

(define (status flag . strs)
  (list flag (string-join-by " " strs)))

(define (run-comparison file tst ref msg)
  (if (zero? (run-command (string-join-by " " "cmp" tst ref)))
      (status #t file)
      (begin
	(when show-diffs?
	  (run-command (string-join-by " " "diff -c" tst ref)))
	(status #f file msg "comparison"))))

;;; compare the test result first against ref, if that exists,
;;; then against refN as long as those exist; return success
;;; on the first match, or failure if none match

(define (run-comparisons file tst ref msg)
  (if (file-exists? ref)
      (run-comparison file tst ref msg)
      (let ((ix 1)
	    (try #t)
	    (result (status #f file msg "comparisons")))
	(do-while try
		  (let ((ref1 (string-append ref (number->string ix))))
		    (if (file-exists? ref1)
			(let ((res1 (run-comparison file tst ref1 msg)))
			  (if (car res1)
			      (begin (set! result res1) (set! try #f))
			      (set! ix (+ ix 1))))
			(set! try #f))))
	result)))

;;; given file "test_NNN.scm"
;;; compile it, run it, save output into "test_NNN.tst"

(define (do-one-test dir file)
  (write-string "## run " file #\newline)
  (let ((pix (string-find-last-char file #\.)))
    (when pix
      (string-set! file pix #\x00)))
  (let* ((pf (if (string=? dir "")
		 file
		 (string-append dir "/" file)))
	 (scm (string-append pf ".scm"))
	 (tst (string-append pf ".tst"))
	 (msg (string-append pf ".msg"))
	 (ref (string-append pf ".ref"))
	 (inp (string-append pf ".input"))
	 (cla (get-environment-variable "TEST_WILE_COMMAND_LINE_ARGS")))
    (remove-file tname)
    (if (file-exists? msg)
	(let* ((port (open-file msg "r"))
	       (line (string-append (read-line port) ";")))
	  (close-port port)
	  (set! msg line))
	(set! msg ""))
    (if compile-only?
	(begin
	  (run-command (string-append "wile -c " (if cla cla "") " " scm))
	  (status #t file "compiled"))
	(if (run-command (string-append "wile " (if cla cla "")
					" " scm " " tname))
	    (if (run-command
		 (if (file-exists? inp)
		     (string-append tname " < " inp " > " tst)
		     (string-append tname " > " tst)))
		(run-comparisons file tst ref msg)
		(status #f file "run"))
	    (status #f file "compile 1")))))

(define (f-pred s)
  (regex-match "^test_[0-9]+\.scm$" s))

(define (s-pred a b)
  (let* ((na (string->number (cadr (regex-match "[0-9]+" a))))
	 (nb (string->number (cadr (regex-match "[0-9]+" b)))))
    (< na nb)))

(define (vinc v i)
  (vector-set! v i (+ 1 (vector-ref v i))))

(when (and (not (null? command-line-arguments))
	   (or (string=? (car command-line-arguments) "-c")
	       (string=? (car command-line-arguments) "-d")
	       (string=? (car command-line-arguments) "--diff")))
  (if (string=? (car command-line-arguments) "-d")
      (set! show-diffs? #t)
      (set! compile-only? #t))
  (set! command-line-arguments (cdr command-line-arguments)))

(when (null? command-line-arguments)
  (write-string stderr "usage: " command-name " test-loc\n")
  (exit 0))

(for-each
 (lambda (tl)
   (if (is-directory? tl)
       (let* ((dir1 (filter f-pred (map car (read-directory tl))))
	      (dir2 (list-sort s-pred dir1))
	      (nr (vector-create 2 0)))
	 (for-each
	  (lambda (r)
  	    (write-string (apply string-join-by " " (cdr r)))
	    (if (car r)
		(begin (write-string " succeeded\n") (vinc nr 0))
		(begin (write-string " failed\n") (vinc nr 1))))
	  (map (lambda (f) (do-one-test tl f)) dir2))
	 (printf "\n%d tests passed\n%d tests failed\n"
		 (vector-ref nr 0) (vector-ref nr 1)))
       (let ((st (do-one-test "" tl)))
	 (write-string (apply string-join-by " " (cdr st)))
	 (write-string (if (car st) " succeeded\n" " failed\n")))))
 command-line-arguments)

(remove-file tname)
