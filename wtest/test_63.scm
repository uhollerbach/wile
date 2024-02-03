(load-library "bigint.scm")

(let ((n bigint-const-0)
      (lim (bigint-from-integer 200)))
  (while (bigint<? n lim)
	 (set! n (bigint-next-prime n))
	 (write-string #\space (bigint-to-string n 16)))
  (newline)
  (while n
	 (write-string #\space (bigint-to-string n 16))
	 (set! n (bigint-prev-prime n)))
  (newline))
