(write-string "#### continuation not invoked\n")

(display
 (call/cc
  (lambda (return)
    (write-string "uno\n")
;;;   (return #t)
    (write-string "dos\n")
    #f)))
(newline)

(write-string "#### continuation invoked\n")
(display
 (call/cc
  (lambda (return)
    (write-string "One\n")
    (return #t)
    (write-string "Two\n")
    #f)))
(newline)
