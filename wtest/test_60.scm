(load-library "bigint.scm")

(do ((i 5 (+ i 1)))
    ((> i 57) #t)
  (bigint-set-base-bits! i)
  (let* ((num1 (bigint-from-string "1234577"))
	 (num2 (bigint-from-string "12345678901234567891"))
	 (num3 (bigint-from-string "#o575577603"))
	 (tst (bigint-exp-mod num1 num2 num3))
	 (cmp (bigint-from-string "#b1111110100000000010110011")))
    (if (bigint=? tst cmp)
	(write-string "exp-mod passes\n")
	(write-string "exp-mod fails!\n"))
    (if (bigint-even? tst)
	(write-string "number is even!?\n")
	(write-string "number is odd\n"))
    (if (bigint-odd? tst)
	(write-string "number is odd\n")
	(write-string "number is even!?\n"))
    (if (bigint-odd? (bigint-right-shift tst 3))
	(write-string "shifted number is odd!?\n")
	(write-string "shifted number is even\n"))))
