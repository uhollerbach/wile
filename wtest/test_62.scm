(load-library "bigint.scm")

(do ((i 0 (+ i 1)))
    ((>= i 10) #t)
  (let ((f (bigint-sum (bigint-left-shift bigint-const-1 (expt 2 i))
			  bigint-const-1)))
    (printf "F_%d = %s prime? %s\n"
	    i (bigint-to-string f 16)
	    (if (bigint-is-prime? f) "yes" "no"))))

(do ((i 0 (+ i 1)))
    ((>= i 150) #t)
  (let ((m (bigint-diff (bigint-left-shift bigint-const-1 i) bigint-const-1)))
    (printf "M_%d = %s prime? %s\n"
	    i (bigint-to-string m 16)
	    (if (bigint-is-prime? m) "yes" "no"))))
