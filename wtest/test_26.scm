(load-library "struct.scm")

(def-struct timestamp year month day hour minute second)

(write-string "begin using def-struct + timestamp\n")

(define ts (make-timestamp 2019 10 4 0 52 17))
(display ts)
(newline)

(write-string (if (isa-timestamp? ts) "Yasss!\n" "no...\n"))

(printf "%d %d %d :: %d %d %d\n"
	(get-timestamp-year ts)
	(get-timestamp-month ts)
	(get-timestamp-day ts)
	(get-timestamp-hour ts)
	(get-timestamp-minute ts)
	(get-timestamp-second ts))

(set-timestamp-year! ts 2020)
(set-timestamp-second! ts 59)
(set-timestamp-day! ts 17)
(set-timestamp-hour! ts 11)
(set-timestamp-minute! ts 43)
(set-timestamp-month! ts 11)

(display ts)
(newline)

(define (nf num w)
  (string-pad-left (number->string num) #\0 w))

(display-object-hook 'timestamp
 (lambda (t port)
   (let ((year (nf (get-timestamp-year t) 4))
	 (month (nf (get-timestamp-month t) 2))
	 (day (nf (get-timestamp-day t) 2))
	 (hour (nf (get-timestamp-hour t) 2))
	 (minute (nf (get-timestamp-minute t) 2))
	 (second (nf (get-timestamp-second t) 2)))
     (fprintf port "%s-%s-%s:%s-%s-%s"
	      year month day hour minute second))))

(display ts)
(newline)
