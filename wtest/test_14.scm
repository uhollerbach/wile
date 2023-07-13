;;; NO LONGER manually fake closures!

(define (primey? val)
  (case val
    ((0) #f)			;;; 'zero
    ((1) #f)			;;; 'unit
    ((2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
	53 59 61 67 71 73 79 83 89 97)
     #t)			;;; 'prime
    (else #f)))			;;; 'composite

(let* ((v (vector 0))
       (lst (fromto 1 23))
       (rets (map
	      (lambda (i)
		(if (primey? i)
		    (begin (vector-set! v 0 (+ (vector-ref v 0) 1)) #t)
		    #f))
	      lst)))
  (display rets)
  (newline)
  (display v)
  (newline))
