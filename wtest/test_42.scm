;;; this test is gonna fail for a while... fixing this is
;;; a major rearrangement of pretty much everything :-(

(let ((lst '(a b c)))
  (display lst)
  (write-string #\space)
  (set-car! lst 1)
  (display lst)
  (write-string #\space)
  (set-car! (cdr lst) 2)
  (display lst)
  (write-string #\space)
  (set-car! (cddr lst) 3)
  (display lst)
  (newline))
