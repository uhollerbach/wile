;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; Macros & functons to make testing more systematic

;;; one of 'off 'summary 'report-failed 'report

(define *test-mode* 'report-failed)

(define *test-ntest* 0)
(define *test-nskip* 0)
(define *test-nfail* 0)
(define *test-nexcept* 0)
(define *test-time-start* 0)
(define *test-cpu-start* 0)

(define (test-title str)
  (let* ((len (+ (string-length str) 2))
	 (lc (- 64 len))
	 (lr (quotient lc 2))
	 (ll (- lc lr))
	 (add (and (positive? ll) (positive? lr))))
    (when add
	  (apply write-string (replicate #\# ll))
	  (write-string #\space))
    (write-string str)
    (when add
	  (write-string #\space)
	  (apply write-string (replicate #\# lr)))
    (write-string #\linefeed)))

(define (test-mode mode)
  (if (or (eqv? mode 'off)
	  (eqv? mode 'summary)
	  (eqv? mode 'report-failed)
	  (eqv? mode 'report))
      (set! *test-mode* mode)
      (raise (list "test-mode got unknown mode" mode))))

(define (test-mode? mode)
  (if (or (eqv? mode 'off)
	  (eqv? mode 'summary)
	  (eqv? mode 'report-failed)
	  (eqv? mode 'report))
      (eqv? *test-mode* mode)
      (raise (list "test-mode? got unknown mode" mode))))

(defmacro (test-pred pass-pred expect expr)
  (let ((tv (gensym))
	(ev (gensym)))
    `(if (eqv? *test-mode* 'off)
	 (set! *test-nskip* (+ *test-nskip* 1))
	 (let ((,ev ,expect))
	   (set! *test-ntest* (+ *test-ntest* 1))
	   (guard (err (#t (unless (eqv? *test-mode* 'summary)
			     (printf "\ntest\t%v\nexpect\t%v\n" ',expr ,ev)
			     (printf "caught exception %v\nFAIL\n" err))
			   (set! *test-nexcept* (+ *test-nexcept* 1))))
		  (let ((,tv ,expr))
		    (if (,pass-pred ,tv ,ev)
			(when (eqv? *test-mode* 'report)
			  (printf "\ntest\t%v\nexpect\t%v\ngot\t%v\n"
				  ',expr ,ev ,tv))
			(begin
			  (set! *test-nfail* (+ *test-nfail* 1))
			  (unless (eqv? *test-mode* 'summary)
			    (printf "\ntest\t%v\nexpect\t%v\ngot\t%v\n"
				    ',expr ,ev ,tv))
			  (when (eqv? *test-mode* 'report)
			    (printf "FAIL\n"))))))))))

;;; TODO: implement this -- pass if there is an exception and
;;; (pass-pred exception expect) returns true; fail otherwise
;;; (test-except-pred pass-pred expect expr)

(defmacro (test expect expr)
  `(test-pred eqv? ,expect ,expr))

(defmacro (test-except expect expr)
  `(test-except-pred eqv? ,expect ,expr))

(define (test-report msg)
  (printf "\n%s\ntests: run %d, skipped %d, failed %d, exceptions %d\n"
	  msg *test-ntest* *test-nskip* *test-nfail* *test-nexcept*)
  (printf "time cpu %.4f sec elapsed %.4f sec\n\n"
	  (- (apply + (cputime)) *test-cpu-start*)
	  (- (epochtime) *test-time-start*)))

(define (test-reset)
  (set! *test-ntest* 0)
  (set! *test-nskip* 0)
  (set! *test-nfail* 0)
  (set! *test-nexcept* 0)
  (set! *test-time-start* (epochtime))
  (set! *test-cpu-start* (apply + (cputime))))

(define (test-expected run skip fail except)
  (and (= *test-ntest* run)
       (= *test-nskip* skip)
       (= *test-nfail* fail)
       (= *test-nexcept* except)))

(define (test-group str nr ns nf ne)
  (test-report str)
  (unless (test-expected nr ns nf ne)
	  (printf "expected %d %d %d %d\n" nr ns nf ne)
	  (exit 1))
  (test-reset))

(test-reset)
