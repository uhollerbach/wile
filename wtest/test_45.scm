;;; test that we get the right closure name even in nested calls:
;;; loop1 is the outer function, loop2 the inner, and loop1's closure
;;; variable is not visible inside loop2 from whence loop1 gets called

(define (top-level msg)
  (printf "try %s?\n\n" msg)
  (let loop1 ((vals (fromto 1 12)))
    (cond ((null? vals)
	   (printf "success %s!\n" msg))
	  ((zero? (modulo (car vals) 4))
	   (printf "\nbegin launch sequence\n")
	   (let loop2 ((count 3))
	     (if (zero? count)
		 (begin
		   (printf "blastoff!\n\n")
		   (loop1 (cdr vals)))
		 (begin
		   (printf "\t%d!\n" count)
		   (loop2 (- count 1))))))
	  (else
	   (printf "waiting %d...\n" (car vals))
	   (loop1 (cdr vals))))))

(top-level "froop")
