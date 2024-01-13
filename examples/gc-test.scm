;;; Test garbage collector: pass 0 1 or 2 as the command-line argument
;;; 0) allocate a constant amount of memory & drop it          -> constant size
;;; 1) allocate a linearly growing  amount of memory & drop it -> N^1 growth
;;; 2) allocate a linearly growing  amount of memory & keep it -> N^2 growth
;;; If program is compiled without GC or if GC is broken, it will gobble up
;;; all memory very quickly regardless of setting.

(let ((select (if (null? command-line-arguments) 0
		  (string->number (car command-line-arguments))))
      (l1 ())
      (n 1))
  (display select)
  (newline)
  (while #t
	 (let ((l2 (fromto 0 n)))
	   (cond ((= select 0)
		  (set! n (+ n 0)))	;;; a no-op
		 ((= select 1)
		  (set! n (+ n 1)))
		 ((= select 2)
		  (set! n (+ n 1))
		  (set! l1 (cons l2 l1)))))))
