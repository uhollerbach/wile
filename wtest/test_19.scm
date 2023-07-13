(define (guard-test exception)
  (guard
   (err ((begin (write-string "exception is '")
		(if (string? err)
		    (write-string err)
		    (display err))
		(write-string "'... ")
		#f) #t)
	((and (string? err) (string=? err "meh"))
	 (write-string "caught 'meh'\n")
	 "I am the 'meh'-catcher")
	((and (string? err) (string=? err "barf"))
	 (write-string "caught 'barf'... yuk!\n")
	 -42)
	((and (string? err) (string=? err "yahoo!"))
	 (write-string "a little excitable today, aren't we...\n")
	 "US$44.6billion")
	((eqv? err (list 1 2))
	 (write-string "does not compute <dalek's head explodes>\n")
	 "daleks rule! (until their heads blow off)"))
   (raise exception)))

(define (nested-guard exception)
  (guard
   (err (else (write-string "hah, wimpy inner guard didn't catch that one!\n")
	      #f))
   (guard-test exception)))

(write-string "Test (guard)\n\n")

(guard-test "meh")
(guard-test "barf")
(guard-test "yahoo!")
(guard-test (list 1 2))

(write-string "\nAgain, this time with nested guards\n\n")

(nested-guard "meh")
(nested-guard "barf")
(nested-guard "yahoo!")
(nested-guard (list 1 2))
(nested-guard "shazam")

(write-string "\nAnd a final flourish\n")
