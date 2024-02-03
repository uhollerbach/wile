(load-library "bigint.scm")

(do ((i 0 (+ i 1)))
    ((> i 50) #t)
  (printf "%d %s\n" i (bigint-to-string (bigint-fibonacci i) 16)))

(printf "\n1000 %s\n" (bigint-to-string (bigint-fibonacci 1000) 16))
