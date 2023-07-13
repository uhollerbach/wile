;;; with this active, the program crashes quickly - as expected
;;; (pragma suppress-tail-call-generation)

;;; this needs to be deep enough that the stack will be exhausted if
;;; tail recursion is not working
(define deep-n 50000)

(define verbose? #f)

;;; wile doesn't like curry yet
;;; (define wstr (curry write-string))
(define (wstr . s) (apply write-string s))
(define (shout n) (display n) (wstr #\space))
(define (finish n)
  (shout n)
  (wstr "finished!\n"))

(define (diver-lambda n)
  (when verbose? (shout n))
  (if (> n deep-n)
      (finish n)
      (diver-lambda (+ n 1))))

(define (diver-and n)
  (when verbose? (shout n))
  (if (> n deep-n)
      (finish n)
      (and #t (diver-and (+ n 1)))))

(define (diver-or n)
  (when verbose? (shout n))
  (if (> n deep-n)
      (finish n)
      (or #f (diver-or (+ n 1)))))

(define (diver-when n)
  (when verbose? (shout n))
  (if (> n deep-n)
      (finish n)
      (when #t (diver-when (+ n 1)))))

(define (diver-unless n)
  (when verbose? (shout n))
  (if (> n deep-n)
      (finish n)
      (unless #f (diver-unless (+ n 1)))))

(define (diver-cond n)
  (when verbose? (shout n))
  (if (> n deep-n)
      (finish n)
      (cond (#f (diver-cond (+ n 1)))
	    (#f (diver-cond (+ n 2)))
	    (else  (diver-cond (+ n 3))))))

(define (diver-let n)
  (when verbose? (shout n))
  (if (> n deep-n)
      (finish n)
      (let ((np (+ n 1)))
	(diver-let np))))

(define (diver-let* n)
  (when verbose? (shout n))
  (if (> n deep-n)
      (finish n)
      (let* ((np (+ n 1)))
	(diver-let* np))))

(define (diver-case c n)
  (when verbose? (shout n))
  (if (> n deep-n)
      (finish n)
      (case c
	((1) (diver-case 2 (+ n 1)))
	((2) (diver-case 3 (+ n 2)))
	((3) (diver-case 4 (+ n 3)))
	((4) (diver-case 1 (+ n 4))))))

(define (diver-do n)
  (when verbose? (shout n))
  (if (> n deep-n)
      (finish n)
      (do ((i 0 (+ i 1)))
	  ((> i 0) (diver-do (+ n 1))))))

(wstr "# tail-recursion tests -- this will take a few seconds\n")

(wstr "lambda" #\tab)	(diver-lambda 0)
(wstr "and" #\tab)	(diver-and 0)
(wstr "or" #\tab)	(diver-or 0)
(wstr "when" #\tab)	(diver-when 0)
(wstr "unless" #\tab)	(diver-unless 0)
(wstr "cond" #\tab)	(diver-cond 0)
(wstr "let" #\tab)	(diver-let 0)
(wstr "let*" #\tab)	(diver-let* 0)
(wstr "case" #\tab)	(diver-case 1 0)
(wstr "do" #\tab)	(diver-do 0)

(wstr "# all done! success!\n")
