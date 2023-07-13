(begin
  (define (foo arg)
    (printf "hello, I am foo, and you have sent me %v\n" arg)))

(foo (list 1 2 3))

(define baz #xdeadbeef)

(let ((arg (list 4 5 6)))
  (define (bar arg)
    (printf "bar here, what have you got for me? %v hmmm, I see...\n"
	     arg))
  (define bogo #xc0ffee)
  (foo arg)
  (bar arg)
  (printf "baz is %x bogo is %x\n" baz bogo))

(let ((arg (list 7 8 9)))
  (define (bar arg)
    (printf "BAR HERE, WHAT HAVE YOU GOT FOR ME? %v HMMM, I SEE...\n"
	     arg))
  (define bogo "floof")
  (foo arg)
  (bar arg)
  (printf "baz is %x bogo is %s\n" baz bogo))
