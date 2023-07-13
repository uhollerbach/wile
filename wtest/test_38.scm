;;; (pragma suppress-tail-call-generation)

(define (printit foo)
  (display foo)
  (newline))

(define (shoggog)
  (printit "yowza!"))

(define (blintz arg)
  (write-string arg " ... ")
  (shoggog))

(define (kefir arg)
  (write-string arg " ... ")
  (shoggog)
  (write-string "and now we're done\n"))

(blintz "here we go, blintzily")
(kefir "here we go, kefirish")
(shoggog)
