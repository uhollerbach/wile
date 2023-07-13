(define flag (null? command-line-arguments))

(defmacro (whenne pred . actions) `(if ,pred (begin ,@actions) #f))
(defmacro (excepting pred . actions) `(if ,pred #f (begin ,@actions)))

(when flag
  (write-string "calloo callay!\n")
  (write-string "vegemite power!\n"))

(whenne flag
  (write-string "o frabjous day!\n")
  (write-string "marmite power...urg\n"))

(unless flag
  (write-string "ook ook?\n")
  (write-string "bananananananananas rule!\n"))

(excepting flag
  (write-string "sirrah, I am a librarian, hast need of me?\n")
  (write-string "I prefer pineapples\n"))
