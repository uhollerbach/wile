;;; (define (primer val)
;;;   (case val
;;;     ((0) 'zero)
;;;     ((1) 'unit)
;;;     ((2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
;;; 	53 59 61 67 71 73 79 83 89 97)
;;;      'prime)
;;;     (else 'composite)))
;;;
;;; (if (null? command-line-arguments)
;;;     (write-string "usage: prog N")
;;;     (write-string
;;;      (symbol->string
;;;       (primer
;;;        (string->number
;;; 	(car command-line-arguments))))))
;;; (newline)

(define (escapify str)
  (let loop ((cs (list-reverse (string->list str)))
	     (acc ()))
    (if (null? cs)
	(apply char->string acc)
	(case (car cs)
	  ((#\alarm)		(loop (cdr cs) (cons #\\ (cons #\a acc))))
	  ((#\backspace)	(loop (cdr cs) (cons #\\ (cons #\b acc))))
	  ((#\escape)		(loop (cdr cs) (cons #\\ (cons #\e acc))))
	  ((#\page)		(loop (cdr cs) (cons #\\ (cons #\f acc))))
	  ((#\newline)		(loop (cdr cs) (cons #\\ (cons #\n acc))))
	  ((#\return)		(loop (cdr cs) (cons #\\ (cons #\r acc))))
	  ((#\tab)		(loop (cdr cs) (cons #\\ (cons #\t acc))))
	  ((#\vtab)		(loop (cdr cs) (cons #\\ (cons #\v acc))))
	  ((#\\)		(loop (cdr cs) (cons #\\ (cons #\\ acc))))
	  ((#\')		(loop (cdr cs) (cons #\\ (cons #\' acc))))
	  ((#\")		(loop (cdr cs) (cons #\\ (cons #\" acc))))
	  (else			(loop (cdr cs) (cons (car cs) acc)))))))

(let ((str "\"nuts to dat!\", he said.\nshe replied, \"you are a fool\"\n"))
  (write-string str)
  (newline)
  (write-string (escapify str))
  (newline))

