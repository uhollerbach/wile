;;; (define (r+ a b) (+ a b))

(let* ((a 3.14)
       (b 2.78)
       (op (if (zero? (remainder (epochtime) 2)) floor ceiling))
       (res (op (r+ a b))))
  (write-string (number->string res 10 4))
  (newline))

(let ((op (list-ref (list get-process-id get-user-id get-group-id)
		    (remainder (epochtime) 3))))
  (display (op))
  (newline))
