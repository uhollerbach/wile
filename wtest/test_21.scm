(printf "char %c string >>%s<< percent %% 10 percent %10% thang %v\n"
	#\G "foo" (cons 3 'a))
(printf "string %l20s\n" "foo")
(printf "string %m20s\n" "bar")
(printf "string %r20s\n" "baz")

(printf "b-int %l20b\n" 10)
(printf "b-int %m20b\n" 11)
(printf "b-int %r20b\n" 12)
(printf "b-int %l20b\n" -10)
(printf "b-int %m20b\n" -11)
(printf "b-int %r20b\n" -12)
(printf "b-int %l 20b\n" 10)
(printf "b-int %m 20b\n" 11)
(printf "b-int %r 20b\n" 12)
(printf "b-int %l+20b\n" 10)
(printf "b-int %m+20b\n" 11)
(printf "b-int %r+20b\n" 12)

(printf "x-int %x\n" 1234567)
(printf "x-int %+x\n" 1234567)
(printf "x-int % x\n" 1234567)
(printf "x-int %x\n" -1234567)
(printf "x-int %+x\n" -1234567)
(printf "x-int % x\n" -1234567)

(printf "d-int %l20d\n" 10)
(printf "d-int %m20d\n" 11)
(printf "d-int %r20d\n" 12)
(printf "d-int %l20d\n" -10)
(printf "d-int %m20d\n" -11)
(printf "d-int %r20d\n" -12)
(printf "d-int %l 20d\n" 10)
(printf "d-int %m 20d\n" 11)
(printf "d-int %r 20d\n" 12)
(printf "d-int %l+20d\n" 10)
(printf "d-int %m+20d\n" 11)
(printf "d-int %r+20d\n" 12)

(printf "now it is %b o'clock... or 43%% of the HoP if you %v\n" 13 "insist")
(printf "now I will %c%c%c%c%c sleep\n" #\g #\o #\space #\t #\o)
(printf "float %l20.4e\n" 1.3)
(printf "float %m20.4e\n" 1.3)
(printf "float %r20.4e\n" 1.3)
(printf "float %l 20.4e\n" 1.3)
(printf "float %m 20.4e\n" 1.3)
(printf "float %r 20.4e\n" 1.3)
(printf "float %l 20.4e\n" -1.3)
(printf "float %m 20.4e\n" -1.3)
(printf "float %r 20.4e\n" -1.3)

(printf "%r1s\n" #\o)
(printf "%r2s\n" #\o)
(printf "%r3s\n" #\o)
(printf "%r4s\n" #\o)
(printf "%r5s\n" #\o)
(printf "%r6s\n" #\o)
(printf "%r7s\n" #\o)
(printf "%r8s\n" #\o)
(printf "%r9s\n" #\o)
(printf "%r9sooohlala!\n" #\space)
