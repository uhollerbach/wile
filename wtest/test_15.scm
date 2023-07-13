(letrec ((evo? (lambda (ne)
		 (if (zero? ne)
		     #t
		     (odo? (- ne 1)))))
	 (odo? (lambda (no)
		 (if (zero? no)
		     #f
		     (evo? (- no 1))))))
  (write-string "88 is " (if (evo? 88) "evo" "odo") #\newline))
