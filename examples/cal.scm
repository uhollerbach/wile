;;; Very simple "cal" replacement - I discovered that "cal" wasn't
;;; installed, so I took it as a small test of the usefulness of wile
;;; to write a replacement. It seems to work pretty well. (I'm aware
;;; that "sudo apt install <some-package>" would have been simpler.)

;;; Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: 2clause BSD

;;; generate a calendar, assuming (proleptic) Gregorian system
;;; no args ->	current year+month
;;; one arg ->	if it looks like a month, show current year + that month
;;;		if it's "-3", show prev,cur,next month
;;;		otherwise assume it's a year, sanity-check, show full year
;;; two args -> assume month and year, or year and month, sanity-
;;;		and ambiguity-check, then display that year+month

;;; "is this a number-looking thing?"

(define (nummy val)
  (regex-match "^[1-9][0-9]*$" val))

;;; if the year looks like a number, and the month looks like a
;;; valid month, return a two-list of the numeric year+month;
;;; otherwise, return #f

(define (yrmo y m)
  (let ((mt (case (string-downcase m)
	      (("jan" "january"   "1" "01") 1)
	      (("feb" "february"  "2" "02") 2)
	      (("mar" "march"     "3" "03") 3)
	      (("apr" "april"     "4" "04") 4)
	      (("may"             "5" "05") 5)
	      (("jun" "june"      "6" "06") 6)
	      (("jul" "july"      "7" "07") 7)
	      (("aug" "august"    "8" "08") 8)
	      (("sep" "september" "9" "09") 9)
	      (("oct" "october"       "10") 10)
	      (("nov" "november"      "11") 11)
	      (("dec" "december"      "12") 12)
	      (else -1))))
    (if (and (nummy y) (positive? mt))
	(list (string->number y) mt)
	#f)))

;;; map of how many days are in each month

(define mon-day-vec #(0 31 28 31 30 31 30 31 31 30 31 30 31))

;;; given a list of strings of length 3, return a list of strings
;;; of the inputs grouped seven at a time

(define (gen-week vs)
  (if (null? vs)
      ()
      (cons (apply string-join-by " " (list-head vs 7))
	    (gen-week (list-tail vs 7)))))

;;; haven't imported this into wile stdlib yet... shame on me

(define (day-of-week v . vs)
  (if (null? vs)
      (modulo (+ 1 v) 7)
      (modulo (+ 1 (julian-day v (car vs) (cadr vs))) 7)))

;;; map of standard month name abbreviations

(define mon-name-vec #(#f "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;;; given numeric legal year+month, generate a calendar for that month

(define (gen-month y m)
  (let* ((h1 (vector-ref mon-name-vec m))
	 (h2 (number->string y))
	 (h3 (string-join-by " " "     " h1 h2))
	 (d1 (upfrom 1 (+ (vector-ref mon-day-vec m)
			  (if (and (is-leap-year? y) (= m 2)) 1 0))))
	 (d2 (map (lambda (n) (string-pad-left (number->string n) #\space 2))
		  d1))
	 (d3 (append '("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa")
		     (replicate "  " (day-of-week y m 1)) d2)))
    (cons h3 (gen-week d3))))

;;; given numeric legal year+month, show a calendar for that month

(define (show-month y m)
  (for-each (lambda (str) (write-string str #\newline)) (gen-month y m)))

;;; car and cdr analogs which deal with short lists the way we want

(define (short-car lst)
  (if (null? lst) "" (car lst)))

(define (short-cdr lst)
  (if (null? lst) () (cdr lst)))

;;; print the lines of 3 months in proper sequence

(define (print-3 l1 l2 l3)
  (if (and (null? l1) (null? l2) (null? l3))
      ()
      (let* ((s1 (string-pad-right (short-car l1) #\space 24))
	     (s2 (string-pad-right (short-car l2) #\space 24))
	     (s3 (short-car l3)))
	(write-string s1 s2 s3 #\newline)
	(print-3 (short-cdr l1) (short-cdr l2) (short-cdr l3)))))

;;; given numeric legal year, generate and show a calendar for that full year

(define (show-year y)
  (print-3 (gen-month y 1) (gen-month y 2) (gen-month y 3))
  (newline)
  (print-3 (gen-month y 4) (gen-month y 5) (gen-month y 6))
  (newline)
  (print-3 (gen-month y 7) (gen-month y 8) (gen-month y 9))
  (newline)
  (print-3 (gen-month y 10) (gen-month y 11) (gen-month y 12)))

;;; print an error message and exit

(define (err . strs)
  (write-string stderr (apply string-append strs) #\newline)
  (exit 1))

;;; the main program: handle the various cases of 0, 1, or 2 arguments

(cond ((null? command-line-arguments)
       (let* ((now (localtime))
	      (y (car now))
	      (m (cadr now)))
	 (show-month y m)))
      ((null? (cdr command-line-arguments))
       (let* ((a1 (car command-line-arguments))
	      (d1 (yrmo "123" a1)))	;; fake year because we need a string
	 (cond (d1 (show-month (car (localtime)) (cadr d1)))
	       ((nummy a1) (show-year (string->number a1)))
	       ((string=? a1 "-3")
		(let* ((now (localtime))
		       (yc (car now))
		       (mc (cadr now)))
		  (print-3 (gen-month (if (= mc 1) (- yc 1) yc)
				      (if (= mc 1) 12 (- mc 1)))
			   (gen-month yc mc)
			   (gen-month (if (= mc 12) (+ yc 1) yc)
				      (if (= mc 12) 1 (+ mc 1))))))
	       (else
		(err "error: " a1 " doesn't look right")))))
      ((null? (cddr command-line-arguments))
       (let* ((a1 (car command-line-arguments))
	      (a2 (cadr command-line-arguments))
	      (d1 (yrmo a1 a2))
	      (d2 (yrmo a2 a1)))
	 (cond ((and d1 d2)
		(err "date " a1 " " a2 " is ambiguous, please re-specify"))
	       (d1 (show-month (car d1) (cadr d1)))
	       (d2 (show-month (car d2) (cadr d2)))
	       (else (err "date " a1 " " a2 " is invalid")))))
      (else (err (list "bad arguments" command-line-arguments))))
