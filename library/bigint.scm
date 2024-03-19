;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2024, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; bignum library - unsigned integers only so far

;;; representation: a bigint is a list of digits (D0 D1 D2 D3 ...)
;;; where each digit is fewer than bigint-base-bits wide and the
;;; position in the list indicates the power of base associated with
;;; that digit: the above becomes D0 + BASE*D1 + BASE^2 * D2 + ...
;;; the maximum size for a digit is limited by the size of
;;; machine(ish) integers: the square of a digit must fit in a
;;; machine(ish) integer, and should remain positive; so for 64-bit
;;; machine integers, bigint-base-bits should be 31 or less, for
;;; 128-bit machine integers, it should be 63 or less, etc. Smaller
;;; works too, but at the low end gets limited by argument range for
;;; various routines which take a smallint: bigint-from-string will
;;; croak for base-10 numbers if bigint-base-bits is 3.

(define bigint-base-bits 60)
(define bigint-neg-base-bits (negative bigint-base-bits))
(define bigint-mask (- (expt 2 bigint-base-bits) 1))

;;; smallest base-bits that we allow is 4, which corresponds
;;; to base size 16; so all of these fit into one digit

(define bigint-const-0 ())
(define bigint-const-1 '(1))
(define bigint-const-2 '(2))
(define bigint-const-3 '(3))
(define bigint-const-4 '(4))

(define (bigint-set-base-bits! n)
  (if (and (> n 3) (< n 64))
      (begin
	(set! bigint-base-bits n)
	(set! bigint-neg-base-bits (negative bigint-base-bits))
	(set! bigint-mask (- (expt 2 bigint-base-bits) 1)))
      (raise (list "bigint-set-base-bits!: bad nbits" n))))

(define (bigint-zero? num)
  (cond ((null? num) #t)
	((zero? (car num)) (bigint-zero? (cdr num)))
	(else #f)))

(define (bigint-even? num)
  (or (null? num) (even? (car num))))

(define (bigint-odd? num)
  (and  (not (null? num)) (odd? (car num))))

(define (bigint-canonical num)
  (list-reverse (list-drop-while zero? (list-reverse num))))

(define (bigint-ilog num)
  (let* ((nc (bigint-canonical num))
	 (len (list-length nc))
	 (dl (list-last nc)))
    (if (zero? len)
	0
	(i+ (i* (i- len 1) bigint-base-bits) (ilog dl)))))

;;; left shift is multiplication, right shift is division

(define (bigint-left-shift num nb)
  (cond ((positive? nb)
	 (let* ((qr (quot-rem nb bigint-base-bits))
		(q (car qr))
		(r (cadr qr))
		(num1 (let loop ((ds num)
				 (c 0))
			(if (null? ds)
			    (cons c ())
			    (let* ((s (bits-or c (bits-shift (car ds) r)))
				   (dn (bits-and s bigint-mask))
				   (cn (bits-shift s bigint-neg-base-bits)))
			      (cons dn (loop (cdr ds) cn)))))))
	   (let loop ((ds num1)
		      (q q))
	     (if (zero? q)
		 ds
		 (loop (cons 0 ds) (i- q 1))))))
	((negative? nb)
	 (bigint-right-shift num (negative nb)))
	((zero? nb)
	 num)
	(else (raise (list "bigint-left-shift: bad shift" nb)))))

(define (bigint-right-shift num nb)
  (cond ((positive? nb)
	 (let* ((qr (quot-rem nb bigint-base-bits))
		(q (car qr))
		(r (cadr qr))
		(rn (negative r))
		(rc (i- bigint-base-bits r))
		(num1 (list-tail num q)))
	   (let loop ((ds (list-reverse num1))
		      (c 0)
		      (acc ()))
	     (if (null? ds)
		 acc
		 (let* ((d (car ds))
			(dn (bits-and (bits-or (bits-shift c rc)
					       (bits-shift d rn))
				      bigint-mask)))
		   (loop (cdr ds) d (cons dn acc)))))))
	((negative? nb)
	 (bigint-left-shift num (negative nb)))
	((zero? nb)
	 num)
	(else (raise (list "bigint-right-shift: bad shift" nb)))))

(define (get-small-digits str)
  (list-reverse
   (let ((cs (string->list (string-trim char-whitespace? str))))
     (if (char=? (car cs) #\#)
	 (if (or (char-ci=? (cadr cs) #\b)
		 (char-ci=? (cadr cs) #\o)
		 (char-ci=? (cadr cs) #\d)
		 (char-ci=? (cadr cs) #\x))
	     (cddr cs)
	     (cdr (list-drop-while char-numeric? (cddr cs))))
	 cs))))

(define (make-big-digit ds)
  (string->number (string-append "#b" (char->string (list-reverse ds)))))

;;; turn separate binary digits into big digits

(define (get-big-digits cs)
  (if (null? cs)
      ()
      (cons (make-big-digit (list-head cs bigint-base-bits))
	    (get-big-digits (list-tail cs bigint-base-bits)))))

(define (map-tet2bin cs)
  (list-flatten
   (map (lambda (c)
	  (case c
	    ((#\0) '(#\0 #\0))
	    ((#\1) '(#\1 #\0))
	    ((#\2) '(#\0 #\1))
	    ((#\3) '(#\1 #\1))))
	cs)))

(define (map-oct2bin cs)
  (list-flatten
   (map (lambda (c)
	  (case c
	    ((#\0) '(#\0 #\0 #\0))
	    ((#\1) '(#\1 #\0 #\0))
	    ((#\2) '(#\0 #\1 #\0))
	    ((#\3) '(#\1 #\1 #\0))
	    ((#\4) '(#\0 #\0 #\1))
	    ((#\5) '(#\1 #\0 #\1))
	    ((#\6) '(#\0 #\1 #\1))
	    ((#\7) '(#\1 #\1 #\1))))
	cs)))

;;; TODO: it's inefficient to reverse the digits list twice, once here
;;; and once inside get-small-digits - fix at some point

;;; TODO: for greater efficiency, group the digits, so that
;;; we multiply by say 10000 and add in 4 digits at a time?

(define (smallbig c)
  (case c
    ((#\0) '(0))
    ((#\1) '(1))
    ((#\2) '(2))
    ((#\3) '(3))
    ((#\4) '(4))
    ((#\5) '(5))
    ((#\6) '(6))
    ((#\7) '(7))
    ((#\8) '(8))
    ((#\9) '(9))
    ((#\a #\A) '(10))
    ((#\b #\B) '(11))
    ((#\c #\C) '(12))
    ((#\d #\D) '(13))
    ((#\e #\E) '(14))
    ((#\f #\F) '(15))
    ((#\g #\G) '(16))
    ((#\h #\H) '(17))
    ((#\i #\I) '(18))
    ((#\j #\J) '(19))
    ((#\k #\K) '(20))
    ((#\l #\L) '(21))
    ((#\m #\M) '(22))
    ((#\n #\N) '(23))
    ((#\o #\O) '(24))
    ((#\p #\P) '(25))
    ((#\q #\Q) '(26))
    ((#\r #\R) '(27))
    ((#\s #\S) '(28))
    ((#\t #\T) '(29))
    ((#\u #\U) '(30))
    ((#\v #\V) '(31))
    ((#\w #\W) '(32))
    ((#\x #\X) '(33))
    ((#\y #\Y) '(34))
    ((#\z #\Z) '(35))))

(define (generic-str2num str base)
  (let loop ((num ())
	     (ds (list-reverse (get-small-digits str))))
    (if (null? ds)
	num
	(loop (bigint-sum (bigint-prod-small num base)
			  (smallbig (car ds))) (cdr ds)))))

(define (map-hex2bin cs)
  (list-flatten
   (map (lambda (c)
	  (case c
	    ((#\0) '(#\0 #\0 #\0 #\0))
	    ((#\1) '(#\1 #\0 #\0 #\0))
	    ((#\2) '(#\0 #\1 #\0 #\0))
	    ((#\3) '(#\1 #\1 #\0 #\0))
	    ((#\4) '(#\0 #\0 #\1 #\0))
	    ((#\5) '(#\1 #\0 #\1 #\0))
	    ((#\6) '(#\0 #\1 #\1 #\0))
	    ((#\7) '(#\1 #\1 #\1 #\0))
	    ((#\8) '(#\0 #\0 #\0 #\1))
	    ((#\9) '(#\1 #\0 #\0 #\1))
	    ((#\a #\A) '(#\0 #\1 #\0 #\1))
	    ((#\b #\B) '(#\1 #\1 #\0 #\1))
	    ((#\c #\C) '(#\0 #\0 #\1 #\1))
	    ((#\d #\D) '(#\1 #\0 #\1 #\1))
	    ((#\e #\E) '(#\0 #\1 #\1 #\1))
	    ((#\f #\F) '(#\1 #\1 #\1 #\1))))
	cs)))

;;; convert a string representation of a bigint into the internal
;;; form. currently allowed bases are 2-10, 16

(define (bigint-from-string str)
  (cond
   ;;; most commonly-used 10 16 2 8
   ((regex-match "^[ \t]*(#([dD]|\\{10\\}))?[0-9]+[ \t]*$" str)
    (generic-str2num str 10))
   ((regex-match "^[ \t]*#([xX]|\\{16\\})[0-9a-fA-F]+[ \t]*$" str)
    (get-big-digits (map-hex2bin (get-small-digits str))))
   ((regex-match "^[ \t]*#([bB]|\\{2\\})[01]+[ \t]*$" str)
    (get-big-digits (get-small-digits str)))
   ((regex-match "^[ \t]*#([oO]|\\{8\\})[0-7]+[ \t]*$" str)
    (get-big-digits (map-oct2bin (get-small-digits str))))
   ;;; less commonly-used
   ((regex-match "^[ \t]*#\\{3\\}[0-2]+[ \t]*$" str)
    (generic-str2num str 3))
   ((regex-match "^[ \t]*#\\{4\\}[0-3]+[ \t]*$" str)
    (get-big-digits (map-tet2bin (get-small-digits str))))
   ((regex-match "^[ \t]*#\\{5\\}[0-4]+[ \t]*$" str)
    (generic-str2num str 5))
   ((regex-match "^[ \t]*#\\{6\\}[0-5]+[ \t]*$" str)
    (generic-str2num str 6))
   ((regex-match "^[ \t]*#\\{7\\}[0-6]+[ \t]*$" str)
    (generic-str2num str 7))
   ((regex-match "^[ \t]*#\\{9\\}[0-8]+[ \t]*$" str)
    (generic-str2num str 9))
   (else (raise (list "bigint-from-string: bad base" str)))))

;;; convert a native non-negative integer into a bigint

(define (bigint-from-integer snum)
  (if (negative? snum)
      (raise (list "bigint-from-integer: bad input" snum))
      (let loop ((c snum))
	(if (zero? c)
	    ()
	    (let* ((dn (bits-and c bigint-mask))
		   (cn (bits-shift c bigint-neg-base-bits)))
	      (cons dn (loop cn)))))))

;;; generate binary representation as list-of-bits of a number

(define (num2bits num)
  (let ((acc ()))
    (while (positive? num)
	   (set! acc (cons (if (even? num) 0 1) acc))
	   (set! num (bits-shift num -1)))
    (list-reverse
     (list-append
      (replicate 0 (- bigint-base-bits (list-length acc))) acc))))

(define (num2str num)
  (case num
    ((0) "0")
    ((1) "1")
    ((2) "2")
    ((3) "3")
    ((4) "4")
    ((5) "5")
    ((6) "6")
    ((7) "7")
    ((8) "8")
    ((9) "9")
    ((10) "a")
    ((11) "b")
    ((12) "c")
    ((13) "d")
    ((14) "e")
    ((15) "f")
    (else (raise (list "num2str: bad num" num)))))

(define (map-bin2tet bits)
  (if (or (null? bits) (null? (cdr bits)))
      bits
      (cons (i+ (car bits) (i* 2 (cadr bits)))
	    (map-bin2tet (cddr bits)))))

(define (map-bin2oct bits)
  (cond ((or (null? bits) (null? (cdr bits)))
	 bits)
	((null? (cddr bits))
	 (cons (i+ (car bits) (i* 2 (cadr bits))) ()))
	(else
	 (cons (i++ (car bits) (i* 2 (cadr bits)) (i* 4 (caddr bits)))
	       (map-bin2oct (cdddr bits))))))

(define (map-bin2hex bits)
  (cond ((or (null? bits) (null? (cdr bits)))
	 bits)
	((null? (cddr bits))
	 (cons (i+ (car bits) (i* 2 (cadr bits))) ()))
	((null? (cdddr bits))
	 (cons (i++ (car bits) (i* 2 (cadr bits)) (i* 4 (caddr bits))) ()))
	(else
	 (cons
	  (i++ (car bits) (i* 2 (cadr bits))
	       (i* 4 (caddr bits)) (i* 8 (cadddr bits)))
	  (map-bin2hex (cddddr bits))))))

(define (make-big-string prefix ds)
  (apply string-append
	 prefix (map num2str (list-drop-while zero? (list-reverse ds)))))

;;; convert a bigint into its string representation in bases 2, 4, 8, 16
;;; TODO: also implement base 10

(define (pnum-to-decimal-string n)
  (let loop ((n n)
	     (acc ()))
    (if (bigint-zero? n)
	(apply string-append acc)
	(let ((qr (bigint-quot-rem-small n 10)))
	  (loop (car qr) (cons (num2str (cadr qr)) acc))))))

(define (bigint-to-string num . base)
  (set! base (if (null? base) 10 (car base)))
  (cond ((bigint-zero? num)
	 "0")
	((= base 10)
	 (pnum-to-decimal-string num))
	(else
	 (let ((bits (apply list-append (map num2bits num))))
	   (case base
	     ((2) (make-big-string "#b" bits))
	     ((4) (make-big-string "#{4}" (map-bin2tet bits)))
	     ((8) (make-big-string "#o" (map-bin2oct bits)))
	     ((16) (make-big-string "#x" (map-bin2hex bits)))
	     (else (raise (list "bigint-to-string: bad base" base))))))))

;;; return the sum of two bigints

(define (bigint-sum num1 num2)
  (let loop ((n1 num1)
	     (n2 num2)
	     (c 0))
    (cond ((and (bigint-zero? n1) (bigint-zero? n2))
	   (if (zero? c)
	       ()
	       (cons c ())))
	  ((bigint-zero? n1)
	   (if (zero? c)
	       n2
	       (let* ((s (i+ c (car n2)))
		      (dn (bits-and s bigint-mask))
		      (cn (bits-shift s bigint-neg-base-bits)))
		 (cons dn (loop () (cdr n2) cn)))))
	  ((bigint-zero? n2)
	   (if (zero? c)
	       n1
	       (let* ((s (i+ c (car n1)))
		      (dn (bits-and s bigint-mask))
		      (cn (bits-shift s bigint-neg-base-bits)))
		 (cons dn (loop (cdr n1) () cn)))))
	  (else
	   (let* ((s (i++ c (car n1) (car n2)))
		  (dn (bits-and s bigint-mask))
		  (cn (bits-shift s bigint-neg-base-bits)))
	     (cons dn (loop (cdr n1) (cdr n2) cn)))))))

;;; return the unsigned difference between two bigints

(define (bigint-diff num1 num2)
  (cond ((bigint-zero? num1) num2)
	((bigint-zero? num2) num1)
	((bigint<? num1 num2)
	 (bigint-diff num2 num1))
	(else
	 (bigint-canonical
	  (let loop ((n1 num1)
		     (n2 num2)
		     (b 0))
	    (cond ((bigint-zero? n1)
		   (if (and (bigint-zero? n2) (zero? b))
		       ()
		       (begin
			 (fprintf stderr "%v %d\n" n2 b)
			 (raise "bigint internal error!"))))
		  ((bigint-zero? n2)
		   (if (zero? b)
		       n1
		       (let* ((s (i- (car n1) b))
			      (dn (bits-and s bigint-mask))
			      (bn (bits-and
				   (bits-shift s bigint-neg-base-bits) 1)))
			 (cons dn (loop (cdr n1) () bn)))))
		  (else
		   (let* ((s (i-- (car n1) (car n2) b))
			  (dn (bits-and s bigint-mask))
			  (bn (bits-and
			       (bits-shift s bigint-neg-base-bits) 1)))
		     (cons dn (loop (cdr n1) (cdr n2) bn))))))))))

;;; return the product of a bigint and a smallint

(define (bigint-prod-small num snum)
  (when (or (negative? snum)
	    (> snum bigint-mask))
    (raise (list "bigint-prod-small: bad snum" snum)))
  (if (zero? snum)
      ()
      (let loop ((n1 num)
		 (c 0))
	(if (bigint-zero? n1)
	    (if (zero? c)
		()
		(cons c ()))
	    (let* ((s (i+ c (i* (car n1) snum)))
		   (dn (bits-and s bigint-mask))
		   (cn (bits-shift s bigint-neg-base-bits)))
	      (cons dn (loop (cdr n1) cn)))))))

(define (shift-by lst n)
  (if (zero? n)
      lst
      (shift-by (cons 0 lst) (i- n 1))))

;;; return the product of two bigints

(define (bigint-prod num1 num2)
  (if (or (bigint-zero? num1) (bigint-zero? num2))
      ()
      (foldl1 bigint-sum
	      (map (lambda (factor shift)
		     (shift-by (bigint-prod-small num2 factor) shift))
		   num1 (upfrom 0 (list-length num1))))))

;;; TODO: first implement slow bit-by-bit division, later we can make it faster

;;; divide N by D returning Q and R: N = Q*D + R with R in [0, D-1],
;;; with D less than half of bigint-base-bits in size

(define (bigint-quot-rem-small num smallden)
  (let* ((nlo-bits (quotient bigint-base-bits 2))
	 (nhi-bits (- bigint-base-bits nlo-bits))
	 (lo-mask (- (expt 2 nlo-bits) 1))
	 (d (bits-and smallden lo-mask))
	 (nlb (negative nlo-bits))
	 (hi-mask (- (expt 2 nhi-bits) 1)))
    (if (zero? d)
	(raise "bigint-quot-rem-small: division by zero")
	(let ((qr (let loop ((n (bigint-canonical num)))
		    (if (null? n)
			'(0)
			(let* ((pr (loop (cdr n)))
			       (nc (car n))
			       (r (car pr))
			       (l (bits-and nc lo-mask))
			       (h (bits-and (bits-shift nc nlb) hi-mask))
			       (r1 (bits-or h (bits-shift r nhi-bits)))
			       (h1 (quotient r d))
			       (r2 (i- r1 (i* h1 d)))
			       (r3 (bits-or l (bits-shift r2 nlo-bits)))
			       (l1 (quotient r3 d))
			       (r4 (i- r3 (i* l1 d)))
			       (nn (bits-or l1 (bits-shift h1 nlo-bits))))
			  (cons r4 (cons nn (cdr pr))))))))
	  (list (bigint-canonical (cdr qr)) (car qr))))))

;;; divide N by D returning Q and R: N = Q*D + R with R in [0, D-1]

(define (bigint-quot-rem num den)
  (if (bigint-zero? den)
      (raise "bigint-quot-rem: division by zero")
      (let ((nd (i- (bigint-ilog num) (bigint-ilog den))))
	(if (negative? nd)
	    (list () num)
	    (let loop ((q ())
		       (r num)
		       (d (bigint-left-shift den nd))
		       (b (bigint-left-shift bigint-const-1 nd))
		       (nd nd))
	      (if (negative? nd)
		  (list (bigint-canonical q) r)
		  (let ((dn (bigint-right-shift d 1))
			(bn (bigint-right-shift b 1))
			(ndn (i- nd 1)))
		    (if (bigint<? r d)
			(loop q r dn bn ndn)
			(loop (bigint-sum q b)
			      (bigint-diff r d) dn bn ndn)))))))))

(define (bigint-quotient n d)
  (car (bigint-quot-rem n d)))

(define (bigint-remainder n d)
  (cadr (bigint-quot-rem n d)))

;;; return -1 0 +1 if num1 < = > num2

(define (bigint-compare num1 num2)
  (let loop ((n1 num1)
	     (n2 num2))
    (cond ((and (bigint-zero? n1) (bigint-zero? n2)) 0)
	  ((bigint-zero? n1) -1)
	  ((bigint-zero? n2) 1)
	  (else
	   (let ((cmp (loop (cdr n1) (cdr n2))))
	     (if (zero? cmp)
		 (let ((d1 (car n1))
		       (d2 (car n2)))
		   (cond ((< d1 d2) -1)
			 ((> d1 d2) 1)
			 (else 0)))
		 cmp))))))

(define (bigint=? num1 num2) (zero? (bigint-compare num1 num2)))
(define (bigint<? num1 num2) (negative? (bigint-compare num1 num2)))
(define (bigint>? num1 num2) (positive? (bigint-compare num1 num2)))

(define (bigint-min num1 num2)
  (if (bigint<? num1 num2) num1 num2))

(define (bigint-max num1 num2)
  (if (bigint>? num1 num2) num1 num2))

(define (bigint-and num1 num2)
  (let loop ((n1 num1)
	     (n2 num2))
    (if (or (null? n1) (null? n2))
	()
	(cons (bits-and (car n1) (car n2)) (loop (cdr n1) (cdr n2))))))

(define (bigint-or num1 num2)
  (let loop ((n1 num1)
	     (n2 num2))
    (cond ((null? n1)
	   n2)
	  ((null? n2)
	   n1)
	  (else
	   (cons (bits-or (car n1) (car n2)) (loop (cdr n1) (cdr n2)))))))

(define (bigint-xor num1 num2)
  (let loop ((n1 num1)
	     (n2 num2))
    (cond ((null? n1)
	   n2)
	  ((null? n2)
	   n1)
	  (else
	   (cons (bits-xor (car n1) (car n2)) (loop (cdr n1) (cdr n2)))))))

(define (bigint-expt a n)
  (cond ((bigint-zero? n)
	 bigint-const-1)
	((bigint-even? n)
	 (bigint-expt (bigint-prod a a) (bigint-right-shift n 1)))
	(else
	 (bigint-prod a (bigint-expt a (bigint-diff n bigint-const-1))))))

(define (bigint-exp-mod a n m)
  (cond ((bigint-zero? n)
	 bigint-const-1)
	((bigint-even? n)
	 (bigint-exp-mod (bigint-remainder (bigint-prod a a) m)
			(bigint-right-shift n 1) m))
	(else
	 (bigint-remainder
	  (bigint-prod a (bigint-exp-mod a (bigint-diff n bigint-const-1) m))
	  m))))

(define (bigint-gcd num1 num2)
  (if (bigint<? num1 num2)
      (bigint-gcd num2 num1)
      (let loop ((num1 num1)
		 (num2 num2))
	(if (bigint-zero? num2)
	    (bigint-max num1 bigint-const-1)
	    (loop num2 (bigint-remainder num1 num2))))))

(define (bigint-lcm num1 num2)
  (bigint-quotient (bigint-prod num1 num2) (bigint-gcd num1 num2)))

;;; return a random bigint in the range [0,n)
;;; WARNING: this is not cryptographically secure!

(define (bigint-random n)
  (let ((r1 (map (lambda (_)
		   (truncate
		    (random-uniform 0.0 (+ 1.0 bigint-mask))))
		 (fromto 0 (quotient (list-length n) bigint-base-bits)))))
    (bigint-remainder r1 n)))

;;; Miller-Rabin primality test

(define (bigint-is-prime? n . k)
  (set! k (if (null? k) 50 (car k)))
  (cond ((bigint<? n bigint-const-2) #f)	;;; 0 or 1 not prime
	((bigint<? n bigint-const-4) #t)	;;; 2 or 3 yes prime
	((bigint-even? n) #f)			;;; not prime
	(else
	 (letrec* ((nm (bigint-diff n bigint-const-1))
		   (factor-2 (lambda (d s)
			       (if (bigint-even? d)
				   (factor-2 (bigint-right-shift d 1) (i+ s 1))
				   (list d s))))
		   (sd (factor-2 nm 0))
		   (d (car sd))
		   (s (i- (cadr sd) 1))
		   (wloop (lambda (kk)
			    (if (zero? kk)
				#t
				(let* ((a (bigint-sum
					   (bigint-random
					    (bigint-diff n bigint-const-4))
					   bigint-const-2))
				       (x (bigint-exp-mod a d n)))
				  (if (or (bigint=? x bigint-const-1)
					  (bigint=? x nm))
				      (wloop (i- kk 1))
				      (sloop s x kk))))))
		   (sloop (lambda (ss x kk)
			    (if (zero? ss)
				#f
				(let ((x2 (bigint-exp-mod x bigint-const-2 n)))
				  (cond ((bigint=? x2 bigint-const-1) #f)
					((bigint=? x2 nm) (wloop (i- kk 1)))
					(else (sloop (i- ss 1) x2 kk))))))))
	   (wloop k)))))

;; return the next-bigger prime

(define (bigint-next-prime n)
  (if (bigint<? n bigint-const-2)
      bigint-const-2
      (let loop ((n (bigint-sum n (if (bigint-even? n)
				      bigint-const-1
				      bigint-const-2))))
	(if (bigint-is-prime? n)
	    n
	    (loop (bigint-sum n bigint-const-2))))))

;; return the next-smaller prime, or #f if there isn't one

(define (bigint-prev-prime n)
  (cond ((bigint<? n bigint-const-3) #f)
	((bigint=? n bigint-const-3) bigint-const-2)
	(else
	 (let loop ((n (bigint-diff n (if (bigint-even? n)
					  bigint-const-1
					  bigint-const-2))))
	   (if (bigint-is-prime? n)
	       n
	       (loop (bigint-diff n bigint-const-2)))))))

;;; TODO: this needs some cleanup - better test for done-ness, remove print

(define (bigint-sqrt num)
  (let loop ((r (bigint-left-shift bigint-const-1
				   (quotient (bigint-ilog num) 2)))
	     (i 0))
    (let ((rn (bigint-quotient
	       (bigint-sum num (bigint-prod r r)) (bigint-left-shift r 1))))
      (write-string (bigint-to-string (bigint-diff r rn) 16) "\n")
      (if (or (bigint-zero? (bigint-diff r rn))
	      (> i 100))
	  rn
	  (loop rn (i+ i 1))))))

;;; compute the factorial of a non-negative smallint, returning a bigint

(define factorial-cache (list bigint-const-1))

(define (bigint-factorial snum)
  (let ((cc (list-length factorial-cache)))
    (if (< snum cc)
	(list-ref factorial-cache (i-- cc snum 1))
	(let loop ((facts factorial-cache)
		   (i cc))
	  (if (> i snum)
	      (begin
		(set! factorial-cache facts)
		(car facts))
	      (loop (cons (bigint-prod-small (car facts) i) facts)
		    (i+ i 1)))))))

;;; compute the nth fibonacci number for non-negative smallint snum = n

(define (bigint-fibonacci snum)
  (define (pf m e)
    (define (sf m)
      (let* ((a (car m))
	     (b (cadr m))
	     (c (caddr m))
	     (d (cadddr m))
	     (ad (bigint-sum a d))
	     (bc (bigint-prod b c))
	     (as (bigint-sum (bigint-prod a a) bc))
	     (bs (bigint-prod ad b))
	     (cs (bigint-prod c ad))
	     (ds (bigint-sum bc (bigint-prod d d))))
	(list as bs cs ds)))
    (define (mf m1 m2)
      (let* ((a1 (car m1))
	     (b1 (cadr m1))
	     (c1 (caddr m1))
	     (d1 (cadddr m1))
	     (a2 (car m2))
	     (b2 (cadr m2))
	     (c2 (caddr m2))
	     (d2 (cadddr m2))
	     (a3 (bigint-sum (bigint-prod a1 a2) (bigint-prod b1 c2)))
	     (b3 (bigint-sum (bigint-prod a1 b2) (bigint-prod b1 d2)))
	     (c3 (bigint-sum (bigint-prod c1 a2) (bigint-prod d1 c2)))
	     (d3 (bigint-sum (bigint-prod c1 b2) (bigint-prod d1 d2))))
	(list a3 b3 c3 d3)))
    (cond ((zero? e)
	   (list bigint-const-1 bigint-const-0 bigint-const-0 bigint-const-1))
	  ((= e 1)
	   m)
	  ((even? e)
	   (pf (sf m) (quotient e 2)))
	  (else
	   (mf m (pf m (- e 1))))))
  (if (zero? snum)
      bigint-const-0
      (car (pf (list bigint-const-1 bigint-const-1
		     bigint-const-1 bigint-const-0)
	       (- snum 1)))))

;;; compute a row of binomial coefficients, returning a list of bigints

(define binomial-cache (list (list bigint-const-1)))

(define (bigint-binomial-coeffs snum)
  (let ((summer (lambda (bs)
		  (let loop ((p bigint-const-0)
			     (c (car bs))
			     (rs (cdr bs)))
		    (let ((s (bigint-sum p c)))
		      (if (null? rs)
			  (list s c)
			  (cons s (loop c (car rs) (cdr rs))))))))
	(cc (list-length binomial-cache)))
    (if (< snum cc)
	(list-ref binomial-cache (i-- cc snum 1))
	(let loop ((bincos binomial-cache)
		   (i cc))
	  (if (> i snum)
	      (begin
		(set! binomial-cache bincos)
		(car bincos))
	      (loop (cons (summer (car bincos)) bincos) (i+ i 1)))))))

;;; compute a row of unsigned Stirling coefficients of the first kind,
;;; returning a list of bigints

(define stirling1-cache (list (list bigint-const-1)))

(define (bigint-stirling1-coeffs snum)
  (let ((summer (lambda (n bs)
		  (let loop ((p bigint-const-0)
			     (c (car bs))
			     (rs (cdr bs)))
		    (let ((s (bigint-sum p (bigint-prod-small c n))))
		      (if (null? rs)
			  (list s c)
			  (cons s (loop c (car rs) (cdr rs))))))))
	(cc (list-length stirling1-cache)))
    (if (< snum cc)
	(list-ref stirling1-cache (i-- cc snum 1))
	(let loop ((stirls stirling1-cache)
		   (i cc))
	  (if (> i snum)
	      (begin
		(set! stirling1-cache stirls)
		(car stirls))
	      (loop (cons (summer (i- i 1) (car stirls)) stirls)
		    (i+ i 1)))))))

;;; compute a row of unsigned Stirling coefficients of the second kind,
;;; returning a list of bigints

(define stirling2-cache (list (list bigint-const-1)))

(define (bigint-stirling2-coeffs snum)
  (let ((summer (lambda (bs)
		  (let loop ((k 0)
			     (p bigint-const-0)
			     (c (car bs))
			     (rs (cdr bs)))
		    (let ((s (bigint-sum p (bigint-prod-small c k))))
		      (if (null? rs)
			  (list s c)
			  (cons s (loop (i+ k 1) c (car rs) (cdr rs))))))))
	(cc (list-length stirling2-cache)))
    (if (< snum cc)
	(list-ref stirling2-cache (i-- cc snum 1))
	(let loop ((stirls stirling2-cache)
		   (i cc))
	  (if (> i snum)
	      (begin
		(set! stirling2-cache stirls)
		(car stirls))
	      (loop (cons (summer (car stirls)) stirls)
		    (i+ i 1)))))))

;;; compute the Catalan number of a non-negative smallint, returning a bigint

(define catalan-cache (list bigint-const-1))

(define (bigint-catalan snum)
  (let ((cc (list-length catalan-cache)))
    (if (< snum cc)
	(list-ref catalan-cache (i-- cc snum 1))
	(let loop ((cats catalan-cache)
		   (i cc))
	  (if (> i snum)
	      (begin
		(set! catalan-cache cats)
		(car cats))
	      (loop (cons (foldl1 bigint-sum
				  (map (lambda (n1 n2) (bigint-prod n1 n2))
				       cats (list-reverse cats)))
			  cats) (i+ i 1)))))))

;;; compute the Motzkin number of a non-negative smallint, returning a bigint

(define motzkin-cache (list bigint-const-1 bigint-const-1))

(define (bigint-motzkin snum)
  (define (calc n mc mp)
    (bigint-canonical
     (bigint-quotient
      (bigint-sum
       (bigint-prod mc (bigint-from-integer (i+ 3 (i* 2 n))))
       (bigint-prod mp (bigint-from-integer (i* 3 n))))
      (bigint-from-integer (i+ n 3)))))
  (let ((cc (i- (list-length motzkin-cache) 1)))
    (if (<= snum cc)
	(list-ref motzkin-cache (i- cc snum))
	(let loop ((mots motzkin-cache)
		   (i cc))
	  (if (>= i snum)
	      (begin
		(set! motzkin-cache mots)
		(car mots))
	      (loop (cons (calc i (car mots) (cadr mots)) mots) (i+ i 1)))))))

;;; compute the Schroeder-Hipparchus number of a non-negative smallint,
;;; returning a bigint

(define schroeder-hipparchus-cache (list bigint-const-1 bigint-const-1))

(define (bigint-schroeder-hipparchus snum)
  (define (calc n mc mp)
    (bigint-canonical
     (bigint-quotient
      (bigint-diff
       (bigint-prod mc (bigint-from-integer (i- (i* 6 n) 9)))
       (bigint-prod mp (bigint-from-integer (i- n 3))))
      (bigint-from-integer n))))
  (set! snum (i+ snum 2))
  (let ((cc (i+ (list-length schroeder-hipparchus-cache) 1)))
    (if (<= snum cc)
	(list-ref schroeder-hipparchus-cache (i- cc snum))
	(let loop ((shs schroeder-hipparchus-cache)
		   (i cc))
	  (if (>= i snum)
	      (begin
		(set! schroeder-hipparchus-cache shs)
		(car shs))
	      (loop (cons (calc i (car shs) (cadr shs)) shs) (i+ i 1)))))))

;;; compute the (central) Delannoy number of a non-negative smallint,
;;; returning a bigint

(define delannoy-cache (list bigint-const-3 bigint-const-1))

(define (bigint-delannoy snum)
  (define (calc n mc mp)
    (bigint-canonical
     (bigint-quotient
      (bigint-diff
       (bigint-prod mc (bigint-from-integer (i- (i* 6 n) 3)))
       (bigint-prod mp (bigint-from-integer (i- n 1))))
      (bigint-from-integer n))))
  (set! snum (i+ snum 1))
  (let ((cc (list-length delannoy-cache)))
    (if (<= snum cc)
	(list-ref delannoy-cache (i- cc snum))
	(let loop ((shs delannoy-cache)
		   (i cc))
	  (if (>= i snum)
	      (begin
		(set! delannoy-cache shs)
		(car shs))
	      (loop (cons (calc i (car shs) (cadr shs)) shs) (i+ i 1)))))))

;;; see wikipedia about Jacobi symbol

(define (bigint-jacobi-symbol a n)
  (when (bigint-even? n)
    (raise "bigint-jacobi-symbol: bad n"))
  (set! a (bigint-remainder a n))
  (let ((t 1)
	(gfd (lambda (num) (if (null? num) 0 (car num)))))
    (until (bigint-zero? a)
	   (while (bigint-even? a)
		  (set! a (bigint-right-shift a 1))
		  (let ((r (modulo (gfd n) 8)))
		    (when (or (= r 3) (= r 5))
		      (set! t (negative t)))))
	   (let ((tmp n))
	     (set! n a)
	     (set! a tmp))
	   (when (and (= (modulo (gfd a) 4) 3) (= (modulo (gfd n) 4) 3))
	     (set! t (negative t)))
	   (set! a (bigint-remainder a n)))
    (if (bigint=? n bigint-const-1) t 0)))
