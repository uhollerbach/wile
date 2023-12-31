(display command-name)
(newline)
(display command-line-arguments)
(newline)
(display pi)
(newline)
(display euler-gamma)
(newline)
(display default-show-sign)
(newline)
(display default-int-base)
(newline)
(display default-float-base)
(newline)
(display default-float-precision)
(newline)

(do ((i 2 (+ i 1)))
    ((> i 25) #t)
  (set! default-int-base i)
  (display i)
  (write-string #\space)
  (display 17)
  (newline))

(set! default-int-base 10)
(do ((i -25 (+ i 1)))
    ((> i 25) #t)
  (set! default-float-precision i)
  (display i)
  (write-string #\space)
  (display pi)
  (newline))
