(load "regexp-samples.scm")
(load "regexp.scm")

(define draw-it #f)
(define make-c #f)

(define (dn msg v)
  (write-string msg #\: #\space)
  (display v)
  (newline)
  (flush-port stdout))

(define (doit str)
  (let ((expr (parse-string str)))
    (if (null? expr)
	(write-string "nothing to do!\n")
	(let* ((re (car expr))
	       (nfa (ast-to-nfa re))
	       (dfa (nfa-to-dfa nfa))
	       (dfr (dfa-reduce2 dfa)))
	  (cond (draw-it
		 (draw-nfa nfa)
		 (draw-dfa dfa)
		 (rename-file "dot-dfa.ps" "dot-dfa1.ps")
		 (draw-dfa dfr)
		 (rename-file "dot-dfa.ps" "dot-dfa2.ps"))
		(make-c
		 (let ((port1 (open-file "recognize1.c" "w"))
		       (port2 (open-file "recognize2.c" "w")))
		   (dfa-to-c dfa port1 "recognize1" #f)
		   (dfa-to-c dfr port2 "recognize2" #f)))
		(else
		 (dn "RE" re)
		 (dn "NFA" nfa)
		 (dn "NFA alphabet" (nfa-alphabet nfa))
		 (dn "DFA" dfa)
		 (dn "DFA alphabet" (dfa-alphabet dfa))
;;;		 (dn "AST entities" (ast-entities re))
;;;		 (dn "DFA classes" (dfa-classes dfa))
;;;		 (for-each (lambda (c) (display c) (newline))
;;;			   (dfa-classes dfa))
		 (dn "DFA reduced" dfr)
		 (dn "DFr classes" (dfa-classes dfr))
		 (for-each (lambda (c) (display c) (newline))
			   (dfa-classes dfr))
		 (dn "AST entities" (ast-entities re))
		 ))))))

(unless (null? command-line-arguments)
  (cond ((string=? (car command-line-arguments) "-d")
	 (set! draw-it #t))
	((string=? (car command-line-arguments) "-c")
	 (set! make-c #t))))

(let loop ()
  (let ((line (read-line-interactive "regexp")))
    (if (or (not line)
	    (string=? line "quit"))
	(begin (write-string stderr "g'bye!\n") (exit 0))
	(begin (doit line) (loop)))))
