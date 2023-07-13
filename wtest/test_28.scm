;;; this tests the begin-breakable macro and string->symbol more
;;; than nested guards; those are already exercised in test_19

(define (do-raise itag)
  (write-string "                just doing our job here...\n")
  (if (zero? itag)
      #t
      (raise (string->symbol (string-append "break" (number->string itag))))))

(let ((i 0))
  (while (< i 4)
    (write-string "######## begin loop " (number->string i) #\newline)
    (write-string "before breakable 3\n")
    (begin-breakable 'break3
	(write-string "    begin breakable 3\n")
	(write-string "    before breakable 2\n")
	(begin-breakable 'break2
	    (write-string "        begin breakable 2\n")
	    (write-string "        before breakable 1\n")
	    (begin-breakable 'break1
		(write-string "            begin breakable 1\n")
		(do-raise i)
		(write-string "            end breakable 1\n"))
	    (write-string "        after breakable 1\n")
	    (write-string "        end breakable 2\n"))
	(write-string "    after breakable 2\n")
	(write-string "    end breakable 3\n"))
    (write-string "after breakable 3\n\n")
    (set! i (+ i 1))))
(write-string "it's all over except for the screaming\n")
