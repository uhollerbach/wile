;;; Define some character-oriented regexp ASTs.

;;; signed integers in bases 2 8 10 and 16

(define int-bin '(#\# (^ #\b #\B) (? (: sign)) (+ (^ #\0 #\1))))
(define int-oct '(#\# (^ #\o #\O) (? (: sign)) (+ (: odigit))))
(define int-dec '((? #\# (^ #\d #\D)) (? (: sign)) (+ (: digit))))
(define int-hex '(#\# (^ #\x #\X) (? (: sign)) (+ (: xdigit))))

;;; use this as a unified integer recognizer, or make separate NFAs from
;;; the above, then merge those NFAs with nfa-merge, to get a recognizer
;;; that can tell which alternative was seen

(define integer (list '^ int-bin int-oct int-dec int-hex))

;;; optionally signed decimal numbers with optional scientific-notation

(define signed-scientific
  '((? (: sign))
    (^ ((+ (: digit)) (? #\. (* (: digit))))
       ((* (: digit)) #\. (+ (: digit))))
    (? (^ #\e #\E) (? (: sign)) (+ (: digit)))))

;;; a simplified version, for looking at the DFA without eyes bleeding...
;;; well, less bleeding anyway

(define simple-signed-scientific
  '((? (: sign))
    (^ ((+ #\D) (? #\. (* #\D))) ((* #\D) #\. (+ #\D)))
    (? #\E (? (: sign)) (+ #\D))))

;;; IPV4 addresses in dotted-quad notation: allow only 0 to 255, no leading 0s
;;; [0-9] or [1-9][0-9] or 1[0-9][0-9] or 2[0-4][0-9] or 25[0-5]

(define ipv4-dotted-quad
  '((^ (: digit)
       ((: digit #\0) (: digit))
       (#\1 (: digit) (: digit))
       (#\2 (: digit #\5 #\6 #\7 #\8 #\9) (: digit))
       (#\2 #\5 (: digit #\6 #\7 #\8 #\9)))
    (# 3 #\.
	 (^ (: digit)
	    ((: digit #\0) (: digit))
	    (#\1 (: digit) (: digit))
	    (#\2 (: digit #\5 #\6 #\7 #\8 #\9) (: digit))
	    (#\2 #\5 (: digit #\6 #\7 #\8 #\9))))))

;;; These two are for conveniently building "killer" regexps which will
;;; cause backtracking NFA implementations (such as in perl, python, and
;;; many other scripting languages) to run exponentially slowly.

(define (make-slow-string n)
  (vector-create n #\a))

(define (make-slow-regexp n)
  (list (list '# n '(? #\a)) (list '# n #\a)))
