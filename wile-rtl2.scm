;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

(pragma library "wile-rtl2.h" "wrtl.sch")

;;; Note: need to not mutate inputs, these routines
;;; are lower-level than they appear

;;; --8><----8><----8><--

(define-alias reverse list-reverse)

(define-primitive "wile_list_reverse"
  "expects one list and returns a new list which is the reverse of the input: (a b c) -> (c b a)"
  (list-reverse lst)
  (let ((t ())
	(l lst))
    (until (null? l)
	   (set! t (cons (car l) t))
	   (set! l (cdr l)))
    t))

;;; --8><----8><----8><--

(define-alias length list-length)

(define-primitive "wile_list_length"
  "expects one list and returns its length"
  (list-length lst)
  (let ((t 0)
	(l lst))
    (until (null? l)
	   (set! t (i+ t 1))
	   (set! l (cdr l)))
    t))

;;; --8><----8><----8><--

;;; the original use case was for seeing if the length of some potentially
;;; long list was >= 2, for example: it would be inefficient to compute
;;; the whole length of the list when all that's really needed is that it
;;; should be at least 2. the other routines were easy to provide too.

(define-primitive "wile_list_length_eq"
  "expects one integer and one list and returns #t if the list length is equal to the given number, #f otherwise"
  (list-length=? n lst)
  (cond ((and (zero? n) (null? lst)) #t)
	((or (zero? n) (null? lst)) #f)
	(else (list-length=? (- n 1) (cdr lst)))))

;;; --8><----8><----8><--

(define-primitive "wile_list_length_ge"
  "expects one integer and one list and returns #t if the list length is equal to or greater than the given number, #f otherwise"
  (list-length>=? n lst)
  (cond ((zero? n) #t)
	((null? lst) #f)
	(else (list-length>=? (- n 1) (cdr lst)))))

;;; --8><----8><----8><--

(define-primitive "wile_list_length_gt"
  "expects one integer and one list and returns #t if the list length is greater than the given number, #f otherwise"
  (list-length>? n lst)
  (cond ((zero? n) (not (null? lst)))
	((null? lst) #f)
	(else (list-length>? (- n 1) (cdr lst)))))

;;; --8><----8><----8><--

(define-primitive "wile_list_length_lt"
  "expects one integer and one list and returns #t if the list length is less than the given number, #f otherwise"
  (list-length<? n lst)
  (not (list-length>=? n lst)))

;;; --8><----8><----8><--

(define-primitive "wile_list_length_le"
  "expects one integer and one list and returns #t if the list length is less than or equal to the given number, #f otherwise"
  (list-length<=? n lst)
  (not (list-length>? n lst)))

;;; --8><----8><----8><--

(define-primitive "wile_list_last"
  "expects one list and returns its last element, or () for a null list"
  (list-last lst)
  (let ((t ())
	(l lst))
    (until (null? l)
	   (set! t (car l))
	   (set! l (cdr l)))
    t))

;;; --8><----8><----8><--

(define-alias append list-append)

(define-primitive "wile_list_append"
  "expects any number of lists and returns a new list which is the concatenation of the inputs"
  (list-append . lsts)
  (if (null? lsts)
      ()
      (let ((l ())
	    (ls lsts))
	(set! ls (list-reverse ls))
	(set! l (car ls))
	(set! ls (cdr ls))
	(while (not (null? ls))
	       (let ((lc (list-reverse (car ls))))
		 (while (not (null? lc))
			(set! l (cons (car lc) l))
			(set! lc (cdr lc))))
	       (set! ls (cdr ls)))
	l)))

;;; --8><----8><----8><--

(define-alias flatten list-flatten)

(define-primitive "wile_list_flatten"
  "expects one list of lists of ... nested arbitrarily deeply, returns a new fully flattened list of all the atoms in the input list: (1 2 (3 4 (5 6) (7 (8 (9))))) -> (1 2 3 4 5 6 7 8 9)"
  (list-flatten lst)
  (cond ((null? lst) '())
	((list? (car lst))
	 (list-append (list-flatten (car lst)) (list-flatten (cdr lst))))
	(else
	 (cons (car lst) (list-flatten (cdr lst))))))

;;; --8><----8><----8><--

(define-primitive "wile_list_head"
  "expects a list L and an integer N, and returns the first N elements of L as a new list"
  (list-head lst n)
  (let loop ((l1 lst)
	     (l2 ())
	     (nl n))
    (if (or (null? l1) (<= nl 0))
	(list-reverse l2)
	(loop (cdr l1) (cons (car l1) l2) (i- nl 1)))))

;;; --8><----8><----8><--

(define-primitive "wile_list_tail"
  "expects a list L and an integer N, and returns all but the first N elements of L"
  (list-tail lst n)
  (let ((i n))
    (until (or (null? lst) (<= i 0))
	   (set! i (i- i 1))
	   (set! lst (cdr lst)))
    lst))

;;; --8><----8><----8><--

(define-primitive "wile_list_unhead"
  "expects a list L and an integer N, and returns the last N elements of L"
  (list-unhead lst n)
  (list-tail lst (i- (list-length lst) n)))

;;; --8><----8><----8><--

(define-primitive "wile_list_untail"
  "expects a list L and an integer N, and returns all but the last N elements of L as a new list"
  (list-untail lst n)
  (list-head lst (i- (list-length lst) n)))

;;; --8><----8><----8><--

(define-primitive "wile_list_ref"
  "expects a list L and an integer N, and returns the Nth element of L; the counting is zero-based, ie (list-ref L 0) is the same as (car L)"
  (list-ref lst n)
  (let ((l lst)
	(i n))
    (when (negative? i)
      (set! i (i+ (list-length l) i)))
    (if (negative? i)
	()
	(do ((j i (i- j 1)))
	    ((or (zero? j) (null? l)) (if (null? l) () (car l)))
	  (set! l (cdr l))))))

;;; --8><----8><----8><--

(define-alias filter list-filter)

(define-primitive "wile_list_filter"
  "expects one test function of one argument and one list and returns those elements of the input list for which the test function does not return #f"
  (list-filter pred lst)
  (let ((ts ())
	(vs lst))
    (until (null? vs)
	   (let ((val (car vs)))
	     (when (pred val)
	       (set! ts (cons val ts))))
	   (set! vs (cdr vs)))
    (list-reverse ts)))

;;; --8><----8><----8><--

(define-alias partition list-partition)

(define-primitive "wile_list_partition"
  "expects one test function of one argument and one list and returns a list of two lists, the first one being all those elements of the input for which the test function returns #t and the second one being all those elements of the input for which the test functions returns #f"
  (list-partition pred lst)
  (let ((ts ())
	(fs ())
	(vs lst))
    (until (null? vs)
	   (let ((val (car vs)))
	     (if (pred val)
		 (set! ts (cons val ts))
		 (set! fs (cons val fs))))
	   (set! vs (cdr vs)))
    (list (list-reverse ts) (list-reverse fs))))

;;; --8><----8><----8><--

(define-primitive "wile_list2vector"
  "expects one list of values and returns a vector of those values in the same order as in the input"
  (list->vector lst)
  (let* ((l (list-length lst))
	 (v (vector-create l)))
    (do ((i 0 (i+ i 1))
	 (lp lst (cdr lp)))
	((= i l) v)
      (vector-set! v i (car lp)))))

;;; --8><----8><----8><--

(define-primitive "wile_list2bytevector"
  "expects one list of characters or small integers and returns a bytevector containing all those bytes"
  (list->bytevector lst)
  (let* ((l (list-length lst))
	 (v (bytevector-create l)))
    (do ((i 0 (i+ i 1))
	 (lp lst (cdr lp)))
	((= i l) v)
      (let ((val (car lp)))
	(if (or (char? val)
		(and (integer? val) (>= val 0) (< val 256)))
	    (bytevector-set! v i val)
	    (raise "list->bytevector got a bad value"))))))

;;; --8><----8><----8><--

(define-alias string->char string->list)

(define-primitive "wile_string2list"
  "expects one string and returns a list of the individual characters of the string in order from the front"
  (string->list str)
  (let ((l (string-length str))
	(t ()))
    (until (zero? l)
	   (set! l (i- l 1))
	   (set! t (cons (string-ref str l) t)))
    t))

;;; --8><----8><----8><--

(define-primitive "wile_vector2list"
  "expects one vector and returns a list of the entries of the vector in order from the front"
  (vector->list vec)
  (let ((l (vector-length vec))
	(t ()))
    (until (zero? l)
	   (set! l (i- l 1))
	   (set! t (cons (vector-ref vec l) t)))
    t))

;;; --8><----8><----8><--

(define-primitive "wile_bytevector2list"
  "expects one bytevector and returns a list of the entries of the vector in order from the front"
  (bytevector->list vec)
  (let ((l (bytevector-length vec))
	(t ()))
    (until (zero? l)
	   (set! l (i- l 1))
	   (set! t (cons (bytevector-ref vec l) t)))
    t))

;;; --8><----8><----8><--

(define-primitive "wile_string_append"
  "expects any number of strings and returns a new string which is the concatenation of the inputs"
  (string-append . strs)
  (let ((lt 0)
	(ss strs)
	(stot #f))
    (until (null? ss)
	   (set! lt (i+ lt (string-length (car ss))))
	   (set! ss (cdr ss)))
    (set! stot (make-string lt))
    (set! ss strs)
    (set! lt 0)
    (until (null? ss)
	   (let* ((sc (car ss))
		  (lc (string-length sc)))
	     (do ((i 0 (i+ i 1)))
		 ((= i lc) #t)
	       (string-set! stot (i+ lt i) (string-ref sc i)))
	     (set! lt (i+ lt lc))
	     (set! ss (cdr ss))))
    stot))

;;; --8><----8><----8><--

(define-primitive "wile_foldr"
  "expects a values-combining function, a right-end value, and a list, and returns the right fold of that function over that list"
  (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

;;; --8><----8><----8><--

(define-primitive "wile_foldl"
  "expects a values-combining function, a left-end value, and a list, and returns the left fold of that function over that list"
  (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

;;; --8><----8><----8><--

(define-primitive "wile_expmod"
  "expects three non-negative integer inputs A N M and returns (A^N) modulo M computed in an efficient manner that avoids extremely large numbers"
  (expmod a n m)
  (cond ((negative? n) #f)  ;;; (raise "expmod got a negative exponent"))
	((zero? n) (modulo 1 m))
	((even? n) (expmod (modulo (i* a a) m) (bits-shift n -1) m))
	(else (modulo (i* a (expmod a (i- n 1) m)) m))))

;;; --8><----8><----8><--

(define-primitive "wile_string_join_by"
  "expects one or more strings, and returns the result of using the first string as a separator between the concatenation of all the rest"
  (string-join-by join . strs)
  (let ((ss (if (and (list-length=? 1 strs) (list? (car strs)))
		(car strs)
		strs)))
    (cond ((null? ss)
	   "")
	  ((string=? join "")
	   (apply string-append ss))
	  (else (let* ((rr (list-reverse ss))
		       (acc (cons (car rr) ())))
		  (set! rr (cdr rr))
		  (until (null? rr)
			 (set! acc (cons (car rr) (cons join acc)))
			 (set! rr (cdr rr)))
		  (apply string-append acc))))))

;;; --8><----8><----8><--

(define (list-keep-head keep? lst)
  (let loop ((l lst)
	     (acc ()))
    (if (or (null? l) (not (keep? (car l))))
	(list (list-reverse acc) l)
	(loop (cdr l) (cons (car l) acc)))))

(define-primitive "wile_string_split_by"
  "expects a character-testing predicate indicating which characters to drop and a string, and returns a list of sub-strings whose characters did not get dropped"
  (string-split-by drop? str)
  (let loop ((l (list-drop-while drop? (string->list str)))
	     (a ()))
    (let* ((proto (list-keep-head (lambda (c) (not (drop? c))) l))
	   (rest (list-drop-while drop? (cadr proto)))
	   (app (cons (char->string (car proto)) a)))
      (if (null? rest)
	  (list-reverse app)
	  (loop rest app)))))

(define-primitive "wile_string_split_by_whitespace"
  "expects a string and returns a list of the non-whitespace sub-strings"
  (string-split-by-whitespace str)
  (string-split-by char-whitespace? str))

;;; --8><----8><----8><--

(define-primitive "wile_foldl1"
  "expects a values-combining function and a non-empty list, and returns the left fold of that function over that list, using the first element of the list as the initial value"
  (foldl1 proc lst)
  (foldl proc (car lst) (cdr lst)))

;;; --8><----8><----8><--

(define (map1 proc lst)
  (let loop ((l1 lst)
	     (acc ()))
    (if (null? l1)
	(list-reverse acc)
	(loop (cdr l1) (cons (proc (car l1)) acc)))))

(define (map2 proc lst1 lst2)
  (let ((l1 (list-length lst1))
	(l2 (list-length lst2)))
    (if (= l1 l2)
	(let loop ((l1 lst1)
		   (l2 lst2)
		   (acc ()))
	  (if (null? l1)
	      (list-reverse acc)
	      (loop (cdr l1)
		    (cdr l2)
		    (cons (proc (car l1) (car l2))
			  acc))))
	(raise (sprintf "map: list lengths differ %d %d" l1 l2)))))

(define (map3 proc lst1 lst2 lst3)
  (let ((l1 (list-length lst1))
	(l2 (list-length lst2))
	(l3 (list-length lst3)))
    (if (and (= l1 l2) (= l1 l3))
	(let loop ((l1 lst1)
		   (l2 lst2)
		   (l3 lst3)
		   (acc ()))
	  (if (null? l1)
	      (list-reverse acc)
	      (loop (cdr l1)
		    (cdr l2)
		    (cdr l3)
		    (cons (proc (car l1) (car l2) (car l3))
			  acc))))
	(raise (sprintf "map: list lengths differ %d %d %d" l1 l2 l3)))))

(define (map4 proc lst1 lst2 lst3 lst4)
  (let ((l1 (list-length lst1))
	(l2 (list-length lst2))
	(l3 (list-length lst3))
	(l4 (list-length lst4)))
    (if (and (= l1 l2) (= l1 l3) (= l1 l4))
	(let loop ((l1 lst1)
		   (l2 lst2)
		   (l3 lst3)
		   (l4 lst4)
		   (acc ()))
	  (if (null? l1)
	      (list-reverse acc)
	      (loop (cdr l1)
		    (cdr l2)
		    (cdr l3)
		    (cdr l4)
		    (cons (proc (car l1) (car l2) (car l3) (car l4))
			  acc))))
	(raise (sprintf "map: list lengths differ %d %d %d %d"
			l1 l2 l3 l4)))))

(define (map5 proc lst1 lst2 lst3 lst4 lst5)
  (let ((l1 (list-length lst1))
	(l2 (list-length lst2))
	(l3 (list-length lst3))
	(l4 (list-length lst4))
	(l5 (list-length lst5)))
    (if (and (= l1 l2) (= l1 l3) (= l1 l4) (= l1 l5))
	(let loop ((l1 lst1)
		   (l2 lst2)
		   (l3 lst3)
		   (l4 lst4)
		   (l5 lst5)
		   (acc ()))
	  (if (null? l1)
	      (list-reverse acc)
	      (loop (cdr l1)
		    (cdr l2)
		    (cdr l3)
		    (cdr l4)
		    (cdr l5)
		    (cons (proc (car l1) (car l2) (car l3) (car l4) (car l5))
			  acc))))
	(raise (sprintf "map: list lengths differ %d %d %d %d %d"
			l1 l2 l3 l4 l5)))))

(define-primitive "wile_map"
  "expects a procedure of N arguments and N lists all of the same length, where N is at least 1; applies the procedure to each tuple consisting of taking the jth entry from each of the lists, and returns the list of results"
  (map proc lst . ls)
  (define (m2 proc acc ls)
    (if (null? (car ls))
	(list-reverse acc)
	(m2 proc (cons (apply proc (map1 car ls)) acc) (map1 cdr ls))))
  (case (list-length ls)
    ;;; special-case for better performance
    ((0) (map1 proc lst))
    ((1) (map2 proc lst (car ls)))
    ((2) (map3 proc lst (car ls) (cadr ls)))
    ((3) (map4 proc lst (car ls) (cadr ls) (caddr ls)))
    ((4) (map5 proc lst (car ls) (cadr ls) (caddr ls) (cadddr ls)))
    (else (let* ((ls (cons lst ls))
		 (ll (map1 list-length ls)))
	    (if (/= (foldl1 min/i ll) (foldl1 max/i ll))
		(raise "map got lists of different lengths")
		(m2 proc () ls))))))

;;; --8><----8><----8><--

(define-primitive "wile_for_each"
  "expects a procedure of N arguments and N lists all of the same length, where N is at least 1, and applies the procedure to each tuple consisting of taking the jth entry from each of the lists; but does not build any list of results"
  (for-each proc lst . lsts)
  (define (fore2 proc ls)
    (if (null? (car ls))
	#t
	(begin (apply proc (map car ls))
	       (fore2 proc (map cdr ls)))))
  (let* ((ls (cons lst lsts))
	 (ll (map list-length ls)))
    (if (/= (foldl1 min/i ll) (foldl1 max/i ll))
	(raise "for-each got lists of different lengths")
	(fore2 proc ls))))

;;; --8><----8><----8><--

;;; TODO: the basic arithmetic functions are dog-slow

(define-primitive "wile_add"
  "expects any number of numeric values and returns their sum"
  (+ . vs)
  (if (null? vs)
      0
      (let ((type 0)
	    (ws vs))
	(until (null? ws)
	       (set! type (max/i type (number/type (car ws))))
	       (set! ws (cdr ws)))
	(set! ws vs)
	(case type
	  ((0) (let ((res 0))
		 (until (null? ws)
			(set! res (i+ res (car ws)))
			(set! ws (cdr ws)))
		 res))
	  ((1) (let ((res (promote/rat 0)))
		 (until (null? ws)
			(set! res (q+ res (promote/rat (car ws))))
			(set! ws (cdr ws)))
		 (if (= (denominator res) 1)
		     (numerator res)
		     res)))
	  ((2) (let ((res 0.0))
		 (until (null? ws)
			(set! res (r+ res (promote/real (car ws))))
			(set! ws (cdr ws)))
		 res))
	  ((3) (let ((res (cmplx 0.0 0.0)))
		 (until (null? ws)
			(set! res (c+ res (promote/cmplx (car ws))))
			(set! ws (cdr ws)))
		 res))
	  (else (raise "'+' got a non-numeric argument"))))))

;;; --8><----8><----8><--

(define-primitive "wile_multiply"
  "expects any number of numeric values and returns their product"
  (* . vs)
  (if (null? vs)
      1
      (let ((type 0)
	    (ws vs))
	(until (null? ws)
	       (set! type (max/i type (number/type (car ws))))
	       (set! ws (cdr ws)))
	(set! ws vs)
	(case type
	  ((0) (let ((res 1))
		 (until (null? ws)
			(set! res (i* res (car ws)))
			(set! ws (cdr ws)))
		 res))
	  ((1) (let ((res (promote/rat 1)))
		 (until (null? ws)
			(set! res (q* res (promote/rat (car ws))))
			(set! ws (cdr ws)))
		 (if (= (denominator res) 1)
		     (numerator res)
		     res)))
	  ((2) (let ((res 1.0))
		 (until (null? ws)
			(set! res (r* res (promote/real (car ws))))
			(set! ws (cdr ws)))
		 res))
	  ((3) (let ((res (cmplx 1.0 0.0)))
		 (until (null? ws)
			(set! res (c* res (promote/cmplx (car ws))))
			(set! ws (cdr ws)))
		 res))
	  (else (raise "'*' got a non-numeric argument"))))))

;;; --8><----8><----8><--

(define-primitive "wile_subtract"
  "expects any number of numeric values; if just one, returns its negation; if more than one, subtracts all but the first from the first"
  (- . vs)
  (if (null? vs)
      0
      (if (null? (cdr vs))
	  (negative (car vs))
	  (let ((type 0)
		(ws vs))
	    (until (null? ws)
		   (set! type (max/i type (number/type (car ws))))
		   (set! ws (cdr ws)))
	    (set! ws vs)
	    (case type
	      ((0) (let ((res (car ws)))
		     (set! ws (cdr ws))
		     (until (null? ws)
			    (set! res (i- res (car ws)))
			    (set! ws (cdr ws)))
		     res))
	      ((1) (let ((res (promote/rat (car ws))))
		     (set! ws (cdr ws))
		     (until (null? ws)
			    (set! res (q- res (promote/rat (car ws))))
			    (set! ws (cdr ws)))
		     (if (= (denominator res) 1)
			 (numerator res)
			 res)))
	      ((2) (let ((res (promote/real (car ws))))
		     (set! ws (cdr ws))
		     (until (null? ws)
			    (set! res (r- res (promote/real (car ws))))
			    (set! ws (cdr ws)))
		     res))
	      ((3) (let ((res (promote/cmplx (car ws))))
		     (set! ws (cdr ws))
		     (until (null? ws)
			    (set! res (c- res (promote/cmplx (car ws))))
			    (set! ws (cdr ws)))
		     res))
	      (else (raise "'-' got a non-numeric argument")))))))

;;; --8><----8><----8><--

(define-primitive "wile_divide"
  "expects any number of numeric values; if just one, returns its reciprocal; if more than one, divides the first by all the remaining values"
  (/ . vs)
  (if (null? vs)
      1
      (if (null? (cdr vs))
	  (reciprocal (car vs))
	  (let ((type 0)
		(ws vs))
	    (until (null? ws)
		   (set! type (max/i type (number/type (car ws))))
		   (set! ws (cdr ws)))
	    (set! ws vs)
	    (case type
	      ((0) (let ((res (promote/rat (car ws))))
		     (set! ws (cdr ws))
		     (until (null? ws)
			    (set! res (q/ res (promote/rat (car ws))))
			    (set! ws (cdr ws)))
		     (if (= (denominator res) 1)
			 (numerator res)
			 res)))
	      ((1) (let ((res (promote/rat (car ws))))
		     (set! ws (cdr ws))
		     (until (null? ws)
			    (set! res (q/ res (promote/rat (car ws))))
			    (set! ws (cdr ws)))
		     (if (= (denominator res) 1)
			 (numerator res)
			 res)))
	      ((2) (let ((res (promote/real (car ws))))
		     (set! ws (cdr ws))
		     (until (null? ws)
			    (set! res (r/ res (promote/real (car ws))))
			    (set! ws (cdr ws)))
		     res))
	      ((3) (let ((res (promote/cmplx (car ws))))
		     (set! ws (cdr ws))
		     (until (null? ws)
			    (set! res (c/ res (promote/cmplx (car ws))))
			    (set! ws (cdr ws)))
		     res))
	      (else (raise "'/' got a non-numeric argument")))))))

;;; --8><----8><----8><--

(define-primitive "wile_min"
  "expects zero or more numeric values, and returns the smallest of them"
  (min . vs)
  (let ((p-ops (list (lambda (x) x) promote/rat promote/real))
	(m-ops (list min/i min/q min/r)))
    (cond ((null? vs) (r/ 1.0 0.0))
	  ((null? (cdr vs))
	   (if (< (number/type (car vs)) 3)
	       (car vs)
	       (raise "min got a non-real argument")))
	  (else
	   (let ((type (foldl max/i 0 (map number/type vs))))
	     (if (< type 3)
		 (foldl1 (list-ref m-ops type) (map (list-ref p-ops type) vs))
		 (raise "min got a non-real argument")))))))

;;; --8><----8><----8><--

(define-primitive "wile_max"
  "expects zero or more numeric values, and returns the largest of them"
  (max . vs)
  (let ((p-ops (list (lambda (x) x) promote/rat promote/real))
	(m-ops (list max/i max/q max/r)))
    (cond ((null? vs) (r/ -1.0 0.0))
	  ((null? (cdr vs))
	   (if (< (number/type (car vs)) 3)
	       (car vs)
	       (raise "max got a non-real argument")))
	  (else
	   (let ((type (foldl max/i 0 (map number/type vs))))
	     (if (< type 3)
		 (foldl1 (list-ref m-ops type) (map (list-ref p-ops type) vs))
		 (raise "max got a non-real argument")))))))

;;; --8><----8><----8><--

(define-primitive "wile_list_sort"
  "expects one less-than comparison function and one list containing only values that can be compared with that comparison function, and returns a new list that is sorted according to that comparison function. sort algorithm is merge sort, a stable sort"
  (list-sort is-lt? lst)
  (define (merge1 lst acc)
    (let ((l lst)
	  (a acc))
      (until (null? l)
	     (set! a (cons (car l) a))
	     (set! l (cdr l)))
      a))
  (define (merge2 is-lt? lst1 lst2 acc)
    (let ((l1 lst1)
	  (l2 lst2)
	  (a acc))
      (cond ((null? l1) (merge1 l2 a))
	    ((null? l2) (merge1 l1 a))
	    ((is-lt? (car l2) (car l1))
	     (merge2 is-lt? l1 (cdr l2) (cons (car l2) a)))
	    (else
	     (merge2 is-lt? (cdr l1) l2 (cons (car l1) a))))))
  (define (one-pass is-lt? lst acc)
    (cond
     ((null? lst) (list-reverse acc))
     ((null? (cdr lst)) (list-reverse (cons (car lst) acc)))
     (else
      (one-pass is-lt? (cddr lst)
		(cons (list-reverse (merge2 is-lt? (car lst) (cadr lst) ()))
		      acc)))))
  (let ((ll (list-length lst))
	(vs (map list lst))
	(cl 1))
    (if (positive? ll)
	(begin (while (< cl ll)
		      (set! cl (i* 2 cl))
		      (set! vs (one-pass is-lt? vs ())))
	       (car vs))
	())))

;;; --8><----8><----8><--

(define-primitive "wile_replicate"
  "expects one general value V and one integer N, and returns a list consisting of N repetitions of V"
  (replicate c n)
  (do ((i 0 (i+ i 1))
       (acc () (cons c acc)))
      ((>= i n) acc)))

;;; --8><----8><----8><--

(define-primitive "wile_memp"
  "expects a test function and a list and returns the first sub-list of the input for which the test applied to the car returns #t, or #f if test returns #f for all elements"
  (memp test? lst)
  (cond ((null? lst) #f)
	((test? (car lst)) lst)
	(else (memp test? (cdr lst)))))

(define-primitive "wile_memv"
  "expects a value and a list and returns the first sub-list of the input for which the value is eqv? to the car, or #f if no elements are eqv? to the value"
  (memv obj lst)
  (cond ((null? lst) #f)
	((eqv? obj (car lst)) lst)
	(else (memv obj (cdr lst)))))

;;; --8><----8><----8><--

(define-primitive "wile_assp"
  "expects a predicate and a list of pairs, and returns the first pair for which the predicate applied to its CAR returns non-false, or #f if no such pair exists"
  (assp test? lst)
  (cond ((null? lst) #f)
	((test? (caar lst)) (car lst))
	(else (assp test? (cdr lst)))))

(define-primitive "wile_assv"
  "expects a test value and a list of pairs, and returns the first pair for which the test value is equivalent to its CAR, or #f if no such pair exists"
  (assv obj lst)
  (cond ((null? lst) #f)
	((eqv? obj (caar lst)) (car lst))
	(else (assv obj (cdr lst)))))

;;; --8><----8><----8><--

(define-primitive "wile_list_drop_while"
  "expects a predicate and a list and removes all those elements at the head of the list for which the predicate returns #t"
  (list-drop-while drop? lst)
  (if (and (not (null? lst)) (drop? (car lst)))
      (list-drop-while drop? (cdr lst))
      lst))

;;; --8><----8><----8><--

(define-primitive "wile_list_take_while"
  "expects a predicate and a list, and returns all those values at the head of the list for which the predicate returns non-false"
  (list-take-while keep? lst)
  (let loop ((lst lst)
	     (acc ()))
    (if (and (not (null? lst)) (keep? (car lst)))
	(loop (cdr lst) (cons (car lst) acc))
	(list-reverse acc))))

;;; --8><----8><----8><--

(define-primitive "wile_list_remove_dups"
  "expects one list and returns a new list with all adjacent duplicates collapsed into one instance"
  (list-remove-dups lst)
  (let loop ((lst lst)
	     (acc ()))
    (if (null? lst)
	(list-reverse acc)
	(loop (list-drop-while (lambda (x) (eqv? x (car lst))) (cdr lst))
	      (cons (car lst) acc)))))

;;; --8><----8><----8><--

(define-primitive "wile_string_pad_left"
  "expects three arguments: a string S, a pad character C, and a minimum length L; returns S padded with as many C on the left as are required to make the length of the result at least L"
  (string-pad-left str pch lmin)
  (let ((sl (string-length str)))
    (if (< sl lmin)
	(string-join-by "" (string-create (i- lmin sl) pch) str)
	str)))

;;; --8><----8><----8><--

(define-primitive "wile_string_pad_right"
  "expects three arguments: a string S, a pad character C, and a minimum length L; returns S padded with as many C on the right as are required to make the length of the result at least L"
  (string-pad-right str pch lmin)
  (let ((sl (string-length str)))
    (if (< sl lmin)
	(string-join-by "" str (string-create (i- lmin sl) pch))
	str)))

;;; --8><----8><----8><--

(define-primitive "wile_string_pad_center"
  "expects three arguments: a string S, a pad character C, and a minimum length L; returns S padded symmetrically with as many C as are required to make the length of the result at least L"
  (string-pad-center str pch lmin)
  (let ((sl (string-length str)))
    (if (< sl lmin)
	(let* ((pt (i- lmin sl))
	       (lpl (quotient pt 2))
	       (lpr (i- pt lpl)))
	  (string-join-by "" (string-create lpl pch)
			  str (string-create lpr pch)))
	str)))

;;; --8><----8><----8><--

(define-primitive "wile_string_trim_left"
  "expects a drop? predicate and a string, and returns a new string with all the droppable characters on the left side of the string removed"
  (string-trim-left drop? str)
  (char->string (list-drop-while drop? (string->list str))))

;;; these two definitions are not the most efficient, but they're simple

;;; --8><----8><----8><--

(define-primitive "wile_string_trim_right"
  "expects a drop? predicate and a string, and returns a new string with all the droppable characters on the right side of the string removed"
  (string-trim-right drop? str)
  (string-reverse (string-trim-left drop? (string-reverse str))))

;;; --8><----8><----8><--

(define-primitive "wile_string_trim"
  "expects a drop? predicate and a string, and returns a new string with all the droppable characters on both sides of the string removed"
  (string-trim drop? str)
  (string-trim-left drop? (string-trim-right drop? str)))

;;; --8><----8><----8><--

(define-primitive "wile_fromto"
  "expects two integers and returns a list of numbers from the first to the second inclusive, counting up if the second integer is larger than the first, and counting down if the second integer is smaller than the first"
  (fromto f l)
  (cond ((= f l) (list f))
	((< f l) (let loop ((f f) (n l) (acc ()))
		   (if (< n f) acc (loop f (i- n 1) (cons n acc)))))
	((> f l) (let loop ((f f) (n l) (acc ()))
		   (if (> n f) acc (loop f (i+ n 1) (cons n acc)))))))

;;; --8><----8><----8><--

(define-primitive "wile_upfrom"
  "expects two integers, a start and a count, and returns a list of numbers counting up from the start: 4 3 -> (4 5 6)"
  (upfrom s n0)
  (let loop ((s s)
	     (n (i- (i+ s n0) 1))
	     (acc ()))
    (if (< n s)
	acc
	(loop s (i- n 1) (cons n acc)))))

;;; --8><----8><----8><--

(define-primitive "wile_any_true"
  "expects a list of values and returns #t if any value is true"
  (any-true? vals)
  (cond ((null? vals) #f)
	((car vals) #t)
	(else (any-true? (cdr vals)))))

;;; --8><----8><----8><--

(define-primitive "wile_all_true"
  "expects a list of values and returns #f if any value is false"
  (all-true? vals)
  (cond ((null? vals) #t)
	((not (car vals)) #f)
	(else (all-true? (cdr vals)))))

;;; --8><----8><----8><--

(define-alias write-char write-string)

(define-primitive "wile_write_string"
  "expects any number of arguments, which must all be strings or characters, except that optionally the first may be an output port. if the first argument is not an output port, the default port is stdout. writes all string/char arguments to the output port"
  (write-string . strs)
  (let ((port #f)
	(ss strs))
    (when (and (not (null? ss))
	       (port? (car ss)))
      (set! port (car ss))
      (set! ss (cdr ss)))
    (if port
	(until (null? ss)
	       (write-1str port (car ss))
	       (set! ss (cdr ss)))
	(until (null? ss)
	       (write-1str (car ss))
	       (set! ss (cdr ss))))))

;;; --8><----8><----8><--

(define-primitive "wile_sql_meta_tables"
  "expects one sqlite port and returns a list of the names of all tables in the database"
  (sqlite-meta-tables port)
  (map car (sqlite-run port "select name from sqlite_schema")))

;;; --8><----8><----8><--

(define-primitive "wile_sql_meta_schema"
  "expects one sqlite port and one string which is the name of a table, and returns the schema for that table in the form of an SQL CREATE statement"
  (sqlite-meta-schema port tbl)
  (let* ((stmt (sqlite-statement-prepare
		port "select sql from sqlite_schema where (name = ?1)"))
	 (ig (sqlite-statement-bind stmt tbl))
	 (res (sqlite-statement-run stmt)))
    (sqlite-statement-cleanup stmt)
    (caadr res)))

;;; --8><----8><----8><--

;;; need to protect against SQL injection, but sqlite-statement-prepare
;;; etc does not work: can't use parametrized table names. so validate
;;; the given table against the list of known tables.

(define-primitive "wile_sql_dump_table"
  "expects one sqlite port, one string which is the name of a table, and one output port, and dumps the named table into the output port in SQL format"
  (sqlite-dump-table sport tbl oport)
  (if (memv tbl (sqlite-meta-tables sport))
      (let* ((cols (sqlite-run
		    sport (string-append "pragma table_info('" tbl "')")))
	     (names (map cadr cols))
	     (types (map caddr cols))
	     (vals (sqlite-run sport (string-append "select * from " tbl)))
	     (nl (string-join-by "," names))
	 ;;; TODO: also check for real-number match
	     (squo (lambda (s)
		     (if (regex-match "^[-+]?[0-9]+$" s)
			 s
			 (string-join-by "" "'" s "'")))))
	(write-string oport "DROP TABLE IF EXISTS " tbl
		      ";\n\nCREATE TABLE " tbl " ("
		      (string-join-by ", "
				      (map (lambda (n t)
					     (string-join-by " " n t))
					   names types))
		      ");\n\nBEGIN TRANSACTION;\n\n")
	(map (lambda (v)
	       (write-string
		oport "INSERT INTO " tbl " VALUES ("
		(string-join-by "," (map squo v)) ");\n"))
	     vals)
	(write-string oport "\nCOMMIT;\n"))
      (write-string oport "-- No such table '" tbl "'\n")))

;;; --8><----8><----8><--

;;; Some utility routines to manipulate dates

;;; This returns an integer which is the Julian day number at noon + eps, ie,
;;; an instant after it has incremented to a new (Julian) day. Inputs are
;;; 4-digit (or whatever is appropriate) year, month from 1 to 12, day from
;;; 1 to 28/31 as appropriate.

;;; Jan 1 2000 = 2451545

(define-primitive "wile_julian_day"
  "expects three integers which represent year, month, and day respectively and returns the Julian day number corresponding to that date in the proleptic Gregorian calendar"
  (julian-day year month day)
  (let* ((a (quotient (i- 14 month) 12))
	 (y (i++ year 4800 (negative a)))
	 (m (i++ month (i* 12 a) -3)))
    (i++ day (quotient (i+ (i* 153 m) 2) 5) (i* 365 y)
	 (quotient y 4) (quotient y -100) (quotient y 400)
	 -32045)))

;;; --8><----8><----8><--

;;; This returns astronomical years for dates before 1 AD: the year before
;;; that is year 0, not 1 BC, etc. For years after 1 AD, this returns
;;; proleptic Gregorian dates. Taken & hacked up from FORTRAN routine from US
;;; Naval Observatory website

(define-primitive "wile_gregorian_date"
  "expects one integer which represents a Julian day number and returns a 3-list (Y M D) which is the corresponding proleptic Gregorian date"
  (gregorian-date jday)
  (let* ((l1 (i+ jday 68569))
	 (n (quotient (i* 4 l1) 146097))
	 (l2 (i- l1 (quotient (i+ (i* 146097 n) 3) 4)))
	 (i1 (quotient (i* 4000 (i+ l2 1)) 1461001))
	 (l3 (i+ l2 (i- 31 (quotient (i* 1461 i1) 4))))
	 (j1 (quotient (i* 80 l3) 2447))
	 (k (i- l3 (quotient (i* 2447 j1) 80)))
	 (l4 (quotient j1 11))
	 (j2 (i++ j1 2 (negative (i* 12 l4))))
	 (i2 (i++ (i* 100 (i- n 49)) i1 l4)))
    (list i2 j2 k)))

;;; --8><----8><----8><--

;;; This returns the Gregorian date corresponding to the given offset from
;;; the input date.

(define-primitive "wile_offset_date"
  "expects four integers year month day offset and returns a triple (year' day' month') corresponding to that offset from (year month day)"
  (offset-date year month day offset)
  (gregorian-date (i+ (julian-day year month day) offset)))

;;; --8><----8><----8><--

;;; This returns the number of days between two dates

(define-primitive "wile_delta_dates"
  "expects six integers Y1 M1 D1 Y2 M2 D2 and returns the number of days between those dates in the Gregorian calendar"
  (delta-dates y1 m1 d1 y2 m2 d2)
  (i- (julian-day y2 m2 d2) (julian-day y1 m1 d1)))

;;; --8><----8><----8><--

(define-primitive "wile_day_of_week"
  "expects either one Julian day number or a Gregorian date in the form of Y M D in numeric format, and returns the numeric weekday of that date, with Sunday being 0 to Saturday being 6"
  (day-of-week v . vs)
  (if (null? vs)
      (modulo (i+ 1 v) 7)
      (modulo (i+ 1 (julian-day v (car vs) (cadr vs))) 7)))

;;; --8><----8><----8><--

;;; Test whether a given year is a leap year

(define-primitive "wile_is_leap_year"
  "expects one integer representing a year and returns whether that year is a leap year according to the Gregorian calendar"
  (is-leap-year? y)
  (and
   (zero? (remainder y 4))
   (or
    (not (zero? (remainder y 100)))
    (zero? (remainder y 400)))))

;;; --8><----8><----8><--

;;; Given a Gregorian date Y M D, return the day of the year
;;; TODO: same for Julian day

(define day-vec #f)

(define-primitive "wile_day_of_year"
  "expects three integers, year month day, and returns the day of the year, from 1 to 365 (or 366)"
  (day-of-year y m d)
  (unless day-vec
    (set! day-vec (vector 0 31 59 90 120 151 181 212 243 273 304 334)))
  (i++ (vector-ref day-vec (i- m 1))
       d (if (and (> m 2) (is-leap-year? y)) 1 0)))

;;; --8><----8><----8><--

;;; Compute the Julian day on which Easter falls on the given year...
;;; once upon a time, this was important and hard

(define-primitive "wile_julian_day_of_easter"
  "expects one integer and returns returns the Julian day number on which Easter falls in that year"
  (julian-day-of-easter year)
  (let* ((a (modulo year 19))
	 (b (quotient year 100))
	 (c (modulo year 100))
	 (d (quotient b 4))
	 (e (modulo b 4))
	 (f (quotient (i+ b 8) 25))
	 (g (quotient (i+ (i- b f) 1) 3))
	 (h (modulo (i-- (i++ (i* 19 a) b 15) d g) 30))
	 (i (quotient c 4))
	 (k (modulo c 4))
	 (l (modulo (i-- (i++ 32 (i* 2 e) (i* 2 i)) h k) 7))
	 (m (quotient (i++ a (i* 11 h) (i* 22 l)) 451))
	 (qr (quot-rem (i- (i++ h l 114) (i* 7 m)) 31))
	 (month (car qr))
	 (day (i+ (cadr qr) 1)))
    (julian-day year month day)))

;;; --8><----8><----8><--

(define-primitive "wile_typeof"
  "expects one argument and returns a symbol describing the type of that argument; may be 'unknown"
  (type-of v)
  (cond ((null? v) 'nil)
	((symbol? v) 'symbol)
	((boolean? v) 'boolean)
	((char? v) 'char)
	((string? v) 'string)
	((integer? v) 'integer)
	((rational? v) 'rational)
	((real? v) 'real)
	((complex? v) 'complex)
	((pair? v) 'pair)
	((file-port? v) 'file-port)
	((pipe-port? v) 'pipe-port)
	((socket-port? v) 'socket-port)
	((string-port? v) 'string-port)
	((sqlite-port? v) 'sqlite-port)
	((sqlite-statement? v) 'sqlite-statement)
	((vector? v) 'vector)
	((bytevector? v) 'bytevector)
	((promise? v) 'promise)
	((compiled-procedure? v) 'compiled-procedure)
	((interpreted-procedure? v) 'interpreted-procedure)
	((continuation? v) 'continuation)
	(else 'unknown!)))

;;; --8><----8><----8><--

;;; *printf for scheme

;;; regexes for different types:
;;; character				"%c"
;;; anything				"%v"
;;; literal percent (or lots)		"%[0-9]*%"
;;; string (or symbol or char)		"%[lrm]?[0-9]*s"
;;; integer or rational numbers		"%[lrm]?[ +]?[0-9]*[bodx]"
;;; floating-point numbers		"%[lrm]?[ +]?[0-9]*(\.[0-9]*)?[fe]"

(define (type-mismatch v c)
  (string-append "*printf type/value mismatch: got "
		 (symbol->string (type-of v))
		 " object for %"
		 (char->string c)
		 " specifier"))

(define r-chr "%c")
(define r-obj "%v")
(define r-pct "%[0-9]*%")
(define r-str "%[lrm]?[0-9]*s")
(define r-exact "%[lrm]?[ +]?[0-9]*[bodx]")
(define r-float "%[lrm]?[ +]?[0-9]*(\.[0-9]*)?[fe]")

(define regex-all #f)

;;; analyze a format string and split it into fields:
;;; %[lrm]?[ +]?[0-9]*(.[0-9]*)?type
;;; initial literal #\%						default
;;; optional align flag #\l or #\r or #\m			#\l
;;; optional spref " " or "+"					""
;;; optional width sequence of digits				0
;;; optional precision, #\. followed by sequence of digits	#f
;;; final literal type character, one of [%scvbodxfe]

(define (split-format-string fstr)
  (let ((cs (string->list fstr))
	(zval (char->integer #\0))
	(align #\l)
	(spref "")
	(width 0)
	(zpad #f)
	(prec #f))
    (if (char=? (car cs) #\%)
	(set! cs (cdr cs))
	(raise (string-append "*printf error: malformed format string " fstr)))
    (when (or (char=? (car cs) #\l)
	      (char=? (car cs) #\r)
	      (char=? (car cs) #\m))
      (set! align (car cs))
      (set! cs (cdr cs)))
    (when (or (char=? (car cs) #\space) (char=? (car cs) #\+))
      (set! spref (char->string (car cs)))
      (set! cs (cdr cs)))
    (set! zpad (char=? (car cs) #\0))
    (while (char-numeric? (car cs))
	   (set! width (i+ (i* 10 width) (i- (char->integer (car cs)) zval)))
	   (set! cs (cdr cs)))
    (when (char=? (car cs) #\.)
      (set! cs (cdr cs))
      (set! prec 0)
      (while (char-numeric? (car cs))
	     (set! prec (i+ (i* 10 prec) (i- (char->integer (car cs)) zval)))
	     (set! cs (cdr cs))))
    (unless (and (null? (cdr cs))
		 (or (char=? (car cs) #\%)
		     (char=? (car cs) #\s)
		     (char=? (car cs) #\c)
		     (char=? (car cs) #\v)
		     (char=? (car cs) #\b)
		     (char=? (car cs) #\o)
		     (char=? (car cs) #\d)
		     (char=? (car cs) #\x)
		     (char=? (car cs) #\f)
		     (char=? (car cs) #\e)))
      (raise (string-append "*printf error: malformed format string " fstr)))
    (when (and (string=? spref "+")
	       (or (char=? (car cs) #\b)
		   (char=? (car cs) #\o)
		   (char=? (car cs) #\x)))
      (set! spref " "))
    (list (car cs) width align spref prec zpad)))

(define (align-str width align str)
  ((case align
     ((#\l) string-pad-right)
     ((#\m) string-pad-center)
     ((#\r) string-pad-left)
     (else (raise "*printf error: malformed alignment")))
   str #\space width))

;;; TODO: slight infelicity here: generic values printed with the %v
;;; specifier get shoved into a temp-file, then that temp file is read
;;; back. Hopefully %v specifiers are rare compared with other types...

(define (disp-obj val)
  (let* ((scr-dir (get-environment-variable "WILE_SCRATCH_DIRECTORY"))
	 (scr-tmp1 "sprintf-scratch.XXXXXX")
	 (scr-tmp2 (if scr-dir (string-append scr-dir "/" scr-tmp1) scr-tmp1))
	 (tfile (open-temporary-file scr-tmp2))
	 (port (car tfile))
	 (name (cadr tfile)))
    (remove-file name)
    (display val port)
    (set-file-position port 0 'start)
    (let loop ((p port)
	       (a ()))
      (let ((c (read-char p)))
	(if c
	    (loop p (cons c a))
	    (begin
	      (close-port p)
	      (char->string (list-reverse a))))))))

;;; small issue with numbers in bases 2 8 & 16 and plus prefix:
;;; negative numbers get written as #b-1100, but this makes positive
;;; numbers get represented as +#b1100, when it should be #b+1100.
;;; -> convert plus prefix to space prefix for types #\b #\o #\x.

(define (printf-helper fstr . vals)
  (unless regex-all
    (set! regex-all
	  (string-join-by "|" (list r-pct r-chr r-obj r-str r-exact r-float))))
  (let loop ((acc ())
	     (fstr fstr)
	     (vs vals))
    (let ((rr (regex-match regex-all fstr)))
      (if rr
	  (let* ((cur-f (cadr rr))
		 (rest (caddr rr))
		 (f-parts (split-format-string cur-f))
		 (type (car f-parts))
		 (width (cadr f-parts))
		 (align (caddr f-parts))
		 (spref (list-ref f-parts 3))
		 (prec (list-ref f-parts 4))
		 (zpad (list-ref f-parts 5)))
	    (set! acc (cons (car rr) acc))
	    (if (char=? type #\%)
		(loop (cons (string-create (max width 1) #\%) acc) rest vs)
		(begin (when (null? vs)
			 (raise "*printf error: not enough arguments"))
		       (let ((cur-v (car vs)))
			 (cond
			  ((char=? type #\c)
			   (cond ((char? cur-v)
				  (loop (cons cur-v acc) rest (cdr vs)))
				 (else (raise (type-mismatch cur-v #\c)))))
			  ((char=? type #\v)
			   (loop (cons
				  (if (or (string? cur-v) (char? cur-v))
				      cur-v
				      (disp-obj cur-v))
				  acc)
				 rest (cdr vs)))
			  ((char=? type #\s)
			   (cond ((string? cur-v)
				  (loop (cons (align-str width align cur-v)
					      acc)
					rest (cdr vs)))
				 ((char? cur-v)
				  (loop (cons (align-str
					       width align
					       (char->string cur-v))
					      acc)

					rest (cdr vs)))
				 ((symbol? cur-v)
				  (loop (cons (align-str
					       width align
					       (symbol->string cur-v))
					      acc)
					rest (cdr vs)))
				 (else (raise (type-mismatch cur-v #\s)))))
			  ((or (char=? type #\b)
			       (char=? type #\o)
			       (char=? type #\d)
			       (char=? type #\x))
			   (unless (rational? cur-v)
			     (raise (type-mismatch cur-v type)))
			   (let* ((base (case type
					  ((#\b) 2) ((#\o) 8)
					  ((#\d) 10) ((#\x) 16)))
				  (str1 (number->string cur-v base))
				  (str2 (if (negative? cur-v)
					    str1
					    (string-append spref str1))))
			     ;;; TODO: somewhere in here, do zero-padding
			     ;;; if requested. it's a bit tricky... need
			     ;;; to handle minus sign and base prefix
			     (loop (cons (align-str width align str2) acc)
				   rest (cdr vs))))
			  ((or (char=? type #\f)
			       (char=? type #\e))
			   (unless (number? cur-v)
			     (raise (type-mismatch cur-v type)))
			   (let* ((pr1 ((if (char=? type #\f) + -)
					(if prec prec 12)))
				  (str1 (number->string cur-v 10 pr1))
				  (str2 (if (negative? cur-v)
					    str1
					    (string-append spref str1))))
			     (loop (cons (align-str width align str2) acc)
				   rest (cdr vs)))))))))
	  (begin
	    (let ((pix (string-find-first-char fstr #\%)))
	      (when pix
		(raise (string-append
			"*printf error: unknown format directive "
			(string-copy fstr pix)))))
	    (unless (null? vs)
	      (raise "*printf error: excess arguments"))
	    (cons fstr acc))))))

;;; given an implementation of printf-helper, the rest is pretty trivial

(define-primitive "wile_printf"
  "expects a format string and any number of additional arguments, and writes the formatted output to stdout"
  (printf fstr . vals)
  (for-each (lambda (s) (write-1str s))
	    (list-reverse (apply printf-helper fstr vals))))

(define-primitive "wile_fprintf"
  "expects an output port, a format string and any number of additional arguments, and writes the formatted output to the specified output port"
  (fprintf port fstr . vals)
  (for-each (lambda (s) (write-1str port s))
	    (list-reverse (apply printf-helper fstr vals))))

(define-primitive "wile_sprintf"
  "expects a format string and any number of additional arguments and returns the formatted output as a string"
  (sprintf fstr . vals)
  (apply string-append
	 (map (lambda (v) (if (string? v) v (char->string v)))
	      (list-reverse (apply printf-helper fstr vals)))))

;;; --8><----8><----8><--

(define-primitive "wile_random_permutation"
  "expects one positive integer N as input and returns a list which is a random permutation of the numbers (1 .. N)"
  (random-permutation n)
  (let ((vec (vector-create n)))
    (do ((i 0 (i+ i 1)))
	((= i n) #t)
      (vector-set! vec i i))
    (do ((i (i- n 1) (i- i 1)))
	((zero? i) vec)
      (vector-swap! vec i (integer (random-uniform 0 i))))))

;;; --8><----8><----8><--

;;; TODO: this is Shell sort, replace with quicksort eventually

(define-primitive "wile_vector_sort_inplace"
  "expects one less-than comparison function and one vector containing only values that can be compared with that comparison function, and sorts the vector in-place according to that comparison function. current sort algorithm is Shell sort"
  (vector-sort! is-le? vec)
  (let* ((n (vector-length vec))
	 (k n)
	 (i 0))
    (do-while (> k 1)
	      (set! k (max (quotient (i- (i* 5 k) 1) 11) 1))
	      (do ((j k (i+ j 1)))
		  ((>= j n) vec)
		(let ((v (vector-ref vec j)))
		  (do ((i (i- j k) (i- i k)))
		      ((or (negative? i) (is-le? (vector-ref vec i) v))
		       (vector-set! vec (i+ i k) v))
		    (vector-set! vec (i+ i k) (vector-ref vec i))))))))

;;; --8><----8><----8><--

(define-primitive "wile_vector_map" "expects a function of N arguments and N equal-length vectors, and returns a vector of the same length whose entries are the values of the function applied to the corresponding entries of the input vectors"
  (vector-map proc vec . vecs)
  (let* ((vs (cons vec vecs))
	 (ls (map vector-length vs)))
    (unless (= (apply min ls) (apply max ls))
      (raise "vector-map: unequal vector lengths"))
    (let* ((len (car ls))
	   (res (make-vector len)))
      (do ((i 0 (+ i 1)))
	  ((= i len) res)
	(vector-set! res i
		     (apply proc (map (lambda (v) (vector-ref v i)) vs)))))))

;;; --8><----8><----8><--

(define-primitive "wile_vector_map_inplace"
  "expects a function of one argument and a vector, and updates the vector in place by applying the function to each element and replacing it with that value"
  (vector-map! proc vec)
  (let ((len (vector-length vec)))
    (do ((i 0 (+ i 1)))
	((= i len) vec)
      (vector-set! vec i (proc (vector-ref vec i))))))

;;; --8><----8><----8><--

(define-primitive "wile_vector_foreach"
  "expects a procedure of N arguments and N vectors all of the same length, where N is at least 1, and applies the procedure to each tuple consisting of taking the jth entry from each of the vectors; but does not build any vector of results"
  (vector-for-each proc vec . vecs)
  (let* ((vs (cons vec vecs))
	 (ls (map vector-length vs)))
    (unless (= (apply min ls) (apply max ls))
      (raise "vector-for-each: unequal vector lengths"))
    (let ((len (car ls)))
      (do ((i 0 (+ i 1)))
	  ((= i len) #t)
	(apply proc (map (lambda (v) (vector-ref v i)) vs))))))

;;; --8><----8><----8><--

(define-primitive "wile_bytevector_foreach"
  "expects a procedure of N arguments and N bytevectors all of the same length, where N is at least 1, and applies the procedure to each tuple consisting of taking the jth entry from each of the bytevectors; but does not build any bytevector of results"
  (bytevector-for-each proc vec . vecs)
  (let* ((vs (cons vec vecs))
	 (ls (map bytevector-length vs)))
    (unless (= (apply min ls) (apply max ls))
      (raise "bytevector-for-each: unequal vector lengths"))
    (let ((len (car ls)))
      (do ((i 0 (+ i 1)))
	  ((= i len) #t)
	(apply proc (map (lambda (v) (bytevector-ref v i)) vs))))))

;;; --8><----8><----8><--

;;; The cholesky-decompose routine takes an upper (or lower)
;;; half-matrix and does an almost-Cholesky decomposition U^T*D*U
;;; (or L*D*L^T). It returns a tuple containing the D matrix, stored
;;; as a simple list of the diagonal values, and the U matrix, stored
;;; in the same half-matrix form as the input.
;;;
;;; Only the diagonal-and-up rows (or equivalently the diagonal-and-
;;; down columns) are passed into the routine; see below for samples.
;;;
;;; The cholesky-solve routine takes a decomposed matrix returned by
;;; cholesky-decompose and a vector and computes the solution of the
;;; matrix equation.

(define-primitive "wile_cholesky_decompose"
  "expects an upper or lower half-matrix and does an L*D*L^T decomposition; returns a list containing the D matrix which is in turn a simple list of the diagonal values and the L matrix, stored in the same half-matrix form as the input"
  (cholesky-decompose mat)
  (define (wk1 r rs)
    (if (zero? r)
	(raise "Zero pivot in Cholesky decomposition")
	(cons 1 (map (lambda (p) (/ p r)) rs))))
  (define (wk2 r1 s r2)
    (map (lambda (e1 e2) (- e1 (* s e2))) r1 r2))
  (define (wk3 ms p us)
    (if (null? ms)
	()
	(cons (wk2 (car ms) (car us) p) (wk3 (cdr ms) (cdr p) (cdr us)))))
  (let loop ((ms mat)
	     (ds '())
	     (us '()))
    (if (null? ms)
	(list (list-reverse ds) (list-reverse us))
	(let* ((m1 (car ms))
	       (mh (car m1))
	       (mt (cdr m1))
	       (cu (wk1 mh mt)))
	  (loop (wk3 (cdr ms) mt (cdr cu)) (cons mh ds) (cons cu us))))))

;;; --8><----8><----8><--

(define-primitive "wile_cholesky_solve"
  "expects an L*D*L^T decomposed matrix as returned by cholesky-decompose and a list R, and solves the matrix equation L*D*L^T X = R for X"
  (cholesky-solve mat vec)
  (define (sf m v)
    (if (null? m)
	()
	(let ((s (car v)))
	  (cons s (sf (cdr m)
		      (map (lambda (e1 e2) (- e1 (* s e2)))
			   (cdr v) (cdar m)))))))
  (define (t1 n m)
    (if (zero? n)
	(list () m)
	(let* ((t (t1 (- n 1) (cdr m)))
	       (r (car m)))
	  (list (cons (car r) (car t)) (cons (cdr r) (cadr t))))))
  (define (t2 i n m)
    (if (= i n)
	'()
	(let* ((ip (+ i 1))
	       (t (t1 ip m)))
	  (cons (list-reverse (car t)) (t2 ip n (cadr t))))))
  (define (t3 m)
    (list-reverse (t2 0 (list-length m) m)))
  (let* ((d (car mat))
	 (u (cadr mat)))
    (list-reverse (sf (t3 u) (list-reverse (map (lambda (m v) (/ v m))
						d (sf u vec)))))))

;;; --8><----8><----8><--

;;; TODO: this is nice and all, but it destroys the ability to compare files;
;;; they will all have different embedded timestamps, and that will kill MD5
;;; (defmacro (compile-time)
;;;   (let ((now (list-head (UTCtime) 6)))
;;;     `(list ,@now)))

(load-library "wile-version.scm")

(define-primitive "wile_build_info"
  "expects a flag and returns a list of various build configuration items"
  (wile-build-info add-ctime?)
  (let* ((binfo (wile-basic-build-info))
	 (info1 `((operating-system ,(wile-os-name))
		  (machine-architecture ,(wile-architecture-name))
		  (wile-version ,wile-version)
		  (float-type ,(case (bits-shift (bits-and binfo #b0011000) -3)
				 ((0) 'double)
				 ((1) 'long-double)
				 ((2) 'quad-double)
				 (else 'unknown-float-type!?!)))
		  (integer-type ,(case (bits-shift (bits-and binfo #b1100000) -5)
				   ((0) 'long-int)
				   ((1) 'int-128)
				   ((2) 'semi-big-int-untested)
				   (else 'unknown-int-type!?!)))
		  (garbage-collector-version ,(gc-version))
		  (sqlite-version ,(sqlite-version))
		  (wile-config-file ,(wile-config-file)))))
    info1))
;;;    (if add-ctime?
;;;	(list-append info1 (list `(wrtl-compiled-on ,(compile-time))))
;;;	info1)))

;;; --8><----8><----8><--

(define-primitive "wile_display_stack_trace"
  "expects a list of stack trace strings and one output file port argument and writes a nice useful stack trace to that file port. returns nothing useful"
  (display-stack-trace trace-data port)
  (let* ((exe-name #f)
	 (data2 #f)
	 (cmd #f)
	 (pport #f))
    (set! data2
	  (list-filter
	   (lambda (x) x)		;;; filter out false values
	   (map (lambda (l)
		  (let ((m (regex-match "\\(\\+0x[0-9a-fA-F]+\\)" l)))
		    (when m
		      (unless exe-name
			(set! exe-name (car m)))
		      (set! m (string-copy (cadr m) 2))
		      (set! m (string-copy m 0 (- (string-length m) 1))))
		    m))
		trace-data))
	  port)
    (set! cmd (apply string-join-by " "
		     "addr2line -f -p -e" exe-name "-a" data2))
    (set! pport (run-read-command cmd))
    (write-string port "wile stack trace begin\n")
    (let loop ()
      (let ((line (read-line pport)))
	(if line
	    (begin (write-string port line #\newline)
		   (loop))
	    (close-port pport))))
    (write-string port "wile stack trace end\n")))

(define-primitive "wile_stack_trace"
  "expects one output file port argument and writes a nice useful stack trace to that file port. returns nothing useful"
  (stack-trace port)
  (let* ((tfile (open-temporary-file "wile-scratch.XXXXXX"))
	 (tport (car tfile))
	 (tname (cadr tfile))
	 (data1 #f)
	 (exe-name #f)
	 (data2 #f)
	 (cmd #f)
	 (pport #f))
    (remove-file tname)
    (stack-trace-minimal tport)
    (flush-port tport)
    (set-file-position tport 0 'start)
    (set! data1 (let loop ((acc ()))
		  (let ((line (read-line tport)))
		    (if line
			(loop (cons line acc))
			(list-reverse acc)))))
    (close-port tport)
    (display-stack-trace data1 port)))

;;; --8><----8><----8><--

;;; A bit of numerical  analysis stuff: root-finding

(define (bracket-error)
  (raise "un-bracketed root or minimum"))

(define (budget-error val)
  (raise (list "function evaluation budget exhausted" val)))

(define (exact v)
  (list v 0))

(define (approx v v1 v2)
  (list v (abs (- v1 v2))))

(define-primitive "wile_root_bracket"
  "expects a function of one real variable, a real-valued starting position, and a real-valued scale factor; returns a bracket on a root of the function"
  (root-bracket f x scale)
  (let iter ((s scale))
    (let* ((x1 (- x s))
	   (x2 (+ x s))
	   (f1 (f x1))
	   (f2 (f x2)))
      (if (/= (sign f1) (sign f2))
	  (list x1 x2)
	  (iter (* 1.414 s))))))

;;; Root finder using bisection method

(define-primitive "wile_root_bisect"
  "expects an error-testing function, the function whose root is to be found, and two real values which bracket a root; returns a 2-list containing a root and the function value at that root"
  (root-bisect et fn a0 b0)
  (letrec ((fa (fn a0))
	   (fb (fn b0))
	   (budget 128)
	   (iter (lambda (ne a fa b fb)
		   (let* ((c (* 0.5 (+ a b)))
			  (fc (fn c))
			  (rv (approx c a b))
			  (nn (- ne 1)))
		     (cond ((zero? ne) (budget-error rv))
			   ((zero? fc) (exact c))
			   ((et a b) rv)
			   ((negative? fc) (iter nn c fc b fb))
			   (else (iter nn a fa c fc)))))))
    (cond ((zero? fa) (exact a0))
	  ((zero? fb) (exact b0))
	  ((= (sign fa) (sign fb)) (bracket-error))
	  ((negative? fa) (iter budget a0 fa b0 fb))
	  (else (iter budget b0 fb a0 fa)))))

;;; Root finder using false-position method:
;;; identical to root-bisect except for calculation of "c"

;;; TODO: something wrong here: interval is not getting smaller

;;; (define (root-false-position et fn a0 b0)
;;;   (letrec ((fa (fn a0))
;;; 	   (fb (fn b0))
;;; 	   (budget 128)
;;; 	   (iter (lambda (ne a fa b fb)
;;; 		   (let* ((c (/ (- (* fa b) (* fb a)) (- fa fb)))
;;; 			  (fc (fn c))
;;; 			  (rv (approx c a b))
;;; 			  (nn (- ne 1)))
;;; 		     (cond ((zero? ne) (budget-error rv))
;;; 			   ((zero? fc) (exact c))
;;; 			   ((et a b) rv)
;;; 			   ((negative? fc) (iter nn c fc b fb))
;;; 			   (else (iter nn a fa c fc)))))))
;;;     (cond ((zero? fa) (exact a0))
;;; 	  ((zero? fb) (exact b0))
;;; 	  ((= (sign fa) (sign fb)) (bracket-error))
;;; 	  ((negative? fa) (iter budget a0 fa b0 fb))
;;; 	  (else (iter budget b0 fb a0 fa)))))

;;; Root finder using Ridders' method

(define-primitive "wile_root_ridders"
  "expects an error-testing function, the function whose root is to be found, and two real values which bracket a root; returns a 2-list containing a root and the function value at that root"
  (root-ridders et fn a0 b0)
  (letrec ((fa (fn a0))
	   (fb (fn b0))
	   (budget 128)
	   (iter (lambda (ne a fa b fb)
		   (let* ((c (* 0.5 (+ a b)))
			  (fc (fn c))
			  (d (+ c (/ (* (- c a) fc (sign (- fa fb)))
				     (sqrt (- (* fc fc) (* fa fb))))))
			  (fd (fn d))
			  (rv (approx d c d))
			  (nn (- ne 1)))
		     (cond ((zero? ne) (budget-error rv))
			   ((zero? fc) (exact c))
			   ((zero? fd) (exact d))
			   ((et a b) rv)
			   ((/= (sign fd) (sign fc)) (iter nn c fc d fd))
			   ((/= (sign fd) (sign fa)) (iter nn a fa d fd))
			   (else (iter nn d fd b fb)))))))
    (cond ((zero? fa) (exact a0))
	  ((zero? fb) (exact b0))
	  ((= (sign fa) (sign fb)) (bracket-error))
	  (else (iter budget a0 fa b0 fb)))))

;;; --8><----8><----8><--

(define-primitive "wile_curry"
  "expects a function and an argument and returns a partially-applied new function"
  (curry func arg1)
  (lambda args (apply func arg1 args)))

;;; --8><----8><----8><--

(define-primitive "wile_compose"
  "expects any number of functions and returns a function which is the composition of all of them"
  (compose f . fs)
  (set! fs (list-reverse (cons f fs)))
  (lambda args
    (let loop ((arg (apply (car fs) args))
	       (fns (cdr fs)))
      (if (null? fns)
	  arg
	  (loop ((car fns) arg) (cdr fns))))))

;;; --8><----8><----8><--

(define (span pred lst)
  (cond ((null? lst) (list () ()))
	((pred (car lst))
	 (let ((st (span pred (cdr lst))))
	   (list (cons (car lst) (car st)) (cadr st))))
	(else (list () lst))))

(define-primitive "wile_list_group_by"
  "expects a comparison function and a list and returns a list of lists of adjacent items which compare equal"
  (list-group-by cmp lst)
  (if (null? lst)
      ()
      (let ((st (span (curry cmp (car lst)) (cdr lst))))
	(cons (cons (car lst) (car st)) (list-group-by cmp (cadr st))))))

;;; --8><----8><----8><--

;;; Miller-Rabin primality test

(define-primitive "wile_mr_primality"
  "expects an integer N and an optional second integer K, and returns whether N is prime or not, based on doing K rounds of probabilistic Miller-Rabin testing"
  (is-prime? n . k)
  (set! k (if (null? k) 100 (car k)))
  (when (negative? n) (set! n (- n)))
  (cond ((<= n 1) #f)
	((<= n 3) #t)
	((even? n) #f)
	(else (letrec*
	       ((nm (- n 1))
		(factor-2 (lambda (d s)
			    (if (even? d)
				(factor-2 (/ d 2) (+ s 1))
				(list d s))))
		(sd (factor-2 nm 0))
		(d (car sd))
		(s (- (cadr sd) 1))
		(wloop (lambda (kk)
			 (if (zero? kk)
			     #t
			     (let* ((a (integer (random-uniform 2 (- n 2))))
				    (x (expmod a d n)))
			       (if (or (= x 1)
				       (= x nm))
				   (wloop (- kk 1))
				   (sloop s x kk))))))
		(sloop (lambda (ss x kk)
			 (if (zero? ss)
			     #f
			     (let ((x2 (expmod x 2 n)))
			       (cond ((= x2 1) #f)
				     ((= x2 nm) (wloop (- kk 1)))
				     (else (sloop (- ss 1) x2 kk))))))))
	       (wloop k)))))

(define-primitive "wile_next_prime"
  "expects one integer N and returns the next prime that is greater than or equal to N, based on Miller-Rabin probabilistic primality test"
  (next-prime n)
  (if (is-prime? n)
      n
      (next-prime (+ n (if (even? n) 1 2)))))

;;; --8><----8><----8><--

;;; TODO: In the absence of working set-c[ad]r!, we need to be a tiny
;;; bit fancy with environments, so that we can do set!: thus define a
;;; bbox structure, for "binding-box": that has a durable pointer to
;;; a location in heap.

;;; TODO: this needs to be in both this segment and the (eval) segment below

(def-struct bbox name value)

;;; TODO: deal with negative-arity macros, wile can't do those yet
;;; several of them are incorporated as special forms already

(define standard-env #f)

(define (make-stdenv-no-macros)
  (map (lambda (p) (make-bbox (car p) (cdr p)))
       (list
;;; TODO: these two are probably wrong for repl: probably want name of
;;; script if any as command-name, and command-line-arguments to not
;;; include said script name
;;;;	      (cons 'command-name command-name)
;;;;	      (cons 'command-line-arguments command-line-arguments)
	(cons 'stdin stdin)
	(cons 'stdout stdout)
	(cons 'stderr stderr)
	(cons 'pi pi)
	(cons 'euler-gamma euler-gamma)
	(cons 'default-show-sign default-show-sign)
	(cons 'default-int-base default-int-base)
	(cons 'default-float-base default-float-base)
	(cons 'default-float-precision default-float-precision)
;;; aliases
	(cons 'agm arithmetic-geometric-mean)
	(cons 'append list-append)
	(cons 'call-with-current-continuation call/cc)
	(cons 'char-lower-case? char-lowercase?)
	(cons 'char-upper-case? char-uppercase?)
	(cons 'cimag imag-part)
	(cons 'complex-conjugate cconj)
	(cons 'conj cconj)
	(cons 'creal real-part)
	(cons 'directory-exists? file-exists?)
	(cons 'filter list-filter)
	(cons 'flatten list-flatten)
	(cons 'last list-last)
	(cons 'length list-length)
	(cons 'list->string char->string)
	(cons 'magnitude abs)
	(cons 'make-bytevector
	      (case-lambic 1 (lambda (a1)
			       (make-bytevector a1))
			   2 (lambda (a1 a2)
			       (make-bytevector a1 a2))))
	(cons 'make-rectangular cmplx)
	(cons 'make-string
	      (case-lambic 1 (lambda (a1)
			       (make-string a1))
			   2 (lambda (a1 a2)
			       (make-string a1 a2))))
	(cons 'make-vector
	      (case-lambic 1 (lambda (a1)
			       (make-vector a1))
			   2 (lambda (a1 a2)
			       (make-vector a1 a2))))
	(cons 'modulo floor-remainder)
	(cons 'number? complex?)
	(cons 'partition list-partition)
	(cons 'phase angle)
	(cons 'quot-rem truncate/)
	(cons 'quotient truncate-quotient)
	(cons 'read-all parse-file)
	(cons 'remainder truncate-remainder)
	(cons 'rename-directory rename-file)
	(cons 'reverse list-reverse)
	(cons 'set-file-position
	      (case-lambic 2 (lambda (a1 a2)
			       (set-file-position a1 a2))
			   3 (lambda (a1 a2 a3)
			       (set-file-position a1 a2 a3))))
	(cons 'sqlite-close close-port)
	(cons 'string->char string->list)
	(cons 'sqlite-open
	      (case-lambic 0 (lambda ()
			       (sqlite-open))
			   1 (lambda (a1)
			       (sqlite-open a1))
			   2 (lambda (a1 a2)
			       (sqlite-open a1 a2))))
	(cons 'substring
	      (case-lambic 1 (lambda (a1)
			       (substring a1))
			   2 (lambda (a1 a2)
			       (substring a1 a2))
			   3 (lambda (a1 a2 a3)
			       (substring a1 a2 a3))))
	(cons 'vector-capacity vector-length)
	(cons 'write-char write-string)
	(cons '* *)
	(cons '+ +)
	(cons '- -)
	(cons '/ /)
	(cons '/= /=)
	(cons '< <)
	(cons '<= <=)
	(cons '= =)
	(cons '> >)
	(cons '>= >=)
	(cons '_cmplx? _cmplx?)
	(cons '_int->cmplx_ _int->cmplx_)
	(cons '_int->rat_ _int->rat_)
	(cons '_int->real_ _int->real_)
	(cons '_int? _int?)
	(cons '_rat->cmplx_ _rat->cmplx_)
	(cons '_rat->real_ _rat->real_)
	(cons '_rat? _rat?)
	(cons '_real->cmplx_ _real->cmplx_)
	(cons '_real? _real?)
	(cons 'UTCtime (case-lambic 0 (lambda ()
					(UTCtime))
				    1 (lambda (a1)
					(UTCtime a1))))
	(cons 'abs abs)
	(cons 'accept accept)
	(cons 'acos acos)
	(cons 'acosh acosh)
	(cons 'all-true? all-true?)
	(cons 'angle angle)
	(cons 'any-true? any-true?)
	(cons 'apply apply-interp)
	(cons 'arithmetic-geometric-mean arithmetic-geometric-mean)
	(cons 'asin asin)
	(cons 'asinh asinh)
	(cons 'assp assp)
	(cons 'assv assv)
	(cons 'atan (case-lambic 1 (lambda (a1)
				     (atan a1))
				 2 (lambda (a1 a2)
				     (atan a1 a2))))
	(cons 'atanh atanh)
	(cons 'bessel-j bessel-j)
	(cons 'bessel-y bessel-y)
	(cons 'bits-and bits-and)
	(cons 'bits-clear bits-clear)
	(cons 'bits-flip bits-flip)
	(cons 'bits-get bits-get)
	(cons 'bits-not bits-not)
	(cons 'bits-or bits-or)
	(cons 'bits-set bits-set)
	(cons 'bits-set? bits-set?)
	(cons 'bits-shift bits-shift)
	(cons 'bits-xor bits-xor)
	(cons 'boolean? boolean?)
	(cons 'bytevector bytevector)
	(cons 'bytevector->string bytevector->string)
	(cons 'bytevector->list bytevector->list)
	(cons 'bytevector-create
	      (case-lambic 1 (lambda (a1)
			       (bytevector-create a1))
			   2 (lambda (a1 a2)
			       (bytevector-create a1 a2))))
	(cons 'bytevector-length bytevector-length)
	(cons 'bytevector-ref bytevector-ref)
	(cons 'bytevector-set! bytevector-set!)
	(cons 'bytevector-swap! bytevector-swap!)
	(cons 'bytevector? bytevector?)
	(cons 'bytevector-for-each bytevector-for-each)
	(cons 'c* c*)
	(cons 'c+ c+)
	(cons 'c- c-)
	(cons 'c/ c/)
	(cons 'caaaar caaaar)
	(cons 'caaadr caaadr)
	(cons 'caaar caaar)
	(cons 'caadar caadar)
	(cons 'caaddr caaddr)
	(cons 'caadr caadr)
	(cons 'caar caar)
	(cons 'cadaar cadaar)
	(cons 'cadadr cadadr)
	(cons 'cadar cadar)
	(cons 'caddar caddar)
	(cons 'cadddddddr cadddddddr)
	(cons 'caddddddr caddddddr)
	(cons 'cadddddr cadddddr)
	(cons 'caddddr caddddr)
	(cons 'cadddr cadddr)
	(cons 'caddr caddr)
	(cons 'cadr cadr)
	(cons 'call/cc call/cc)
	(cons 'car car)
	(cons 'cbrt cbrt)
	(cons 'cconj cconj)
	(cons 'cdaaar cdaaar)
	(cons 'cdaadr cdaadr)
	(cons 'cdaar cdaar)
	(cons 'cdadar cdadar)
	(cons 'cdaddr cdaddr)
	(cons 'cdadr cdadr)
	(cons 'cdar cdar)
	(cons 'cddaar cddaar)
	(cons 'cddadr cddadr)
	(cons 'cddar cddar)
	(cons 'cdddar cdddar)
	(cons 'cddddr cddddr)
	(cons 'cdddr cdddr)
	(cons 'cddr cddr)
	(cons 'cdr cdr)
	(cons 'ceiling ceiling)
	(cons 'ceiling-quotient ceiling-quotient)
	(cons 'ceiling-remainder ceiling-remainder)
	(cons 'ceiling/ ceiling/)
	(cons 'cfft-good-n? cfft-good-n?)
	(cons 'change-file-owner change-file-owner)
	(cons 'change-root-directory change-root-directory)
	(cons 'change-symbolic-link-owner change-symbolic-link-owner)
	(cons 'char->integer char->integer)
	(cons 'char->string char->string)
	(cons 'char-alphabetic? char-alphabetic?)
	(cons 'char-alphanumeric? char-alphanumeric?)
	(cons 'char-ci/=? char-ci/=?)
	(cons 'char-ci<=? char-ci<=?)
	(cons 'char-ci<? char-ci<?)
	(cons 'char-ci=? char-ci=?)
	(cons 'char-ci>=? char-ci>=?)
	(cons 'char-ci>? char-ci>?)
	(cons 'char-control? char-control?)
	(cons 'char-downcase char-downcase)
	(cons 'char-hex-digit? char-hex-digit?)
	(cons 'char-lowercase? char-lowercase?)
	(cons 'char-numeric? char-numeric?)
	(cons 'char-oct-digit? char-oct-digit?)
	(cons 'char-printable? char-printable?)
	(cons 'char-upcase char-upcase)
	(cons 'char-uppercase? char-uppercase?)
	(cons 'char-whitespace? char-whitespace?)
	(cons 'char/=? char/=?)
	(cons 'char<=? char<=?)
	(cons 'char<? char<?)
	(cons 'char=? char=?)
	(cons 'char>=? char>=?)
	(cons 'char>? char>?)
	(cons 'char? char?)
	(cons 'cholesky-decompose cholesky-decompose)
	(cons 'cholesky-solve cholesky-solve)
	(cons 'clear-file-error clear-file-error)
	(cons 'close-port close-port)
	(cons 'cmplx cmplx)
	(cons 'compose compose)
	(cons 'complex? complex?)
	(cons 'connect-to connect-to)
	(cons 'cons cons)
	(cons 'continuation? continuation?)
	(cons 'cos cos)
	(cons 'cosh cosh)
	(cons 'cosine-integral cosine-integral)
	(cons 'cputime cputime)
	(cons 'create-directory
	      (case-lambic 1 (lambda (a1)
			       (create-directory a1))
			   2 (lambda (a1 a2)
			       (create-directory a1 a2))))
	(cons 'create-link create-link)
	(cons 'create-symbolic-link create-symbolic-link)
	(cons 'curry curry)
	(cons 'cxr cxr)
	(cons 'day-of-week day-of-week)
	(cons 'day-of-year day-of-year)
	(cons 'delta-dates delta-dates)
	(cons 'denominator denominator)
	(cons 'describe-system-error describe-system-error)
	(cons 'digamma digamma)
	(cons 'display (case-lambic 1 (lambda (a1)
					(display a1))
				    2 (lambda (a1 a2)
					(display a1 a2))))
	(cons 'display-object-hook display-object-hook)
	(cons 'display-stack-trace display-stack-trace)
	(cons 'elliptic-E elliptic-E)
	(cons 'elliptic-K elliptic-K)
	(cons 'emergency-exit emergency-exit)
	(cons 'epochtime epochtime)
	(cons 'eqv? eqv?)
	(cons 'erfc erfc)
;;; we're trying to build this one!
;;;	(list 'eval eval)
	(cons 'even? even?)
	(cons 'exit exit)
	(cons 'exp exp)
	(cons 'expmod expmod)
	(cons 'expt expt)
	(cons 'factorial factorial)
	(cons 'file-executable? file-executable?)
	(cons 'file-exists? file-exists?)
	(cons 'file-port? file-port?)
	(cons 'file-readable? file-readable?)
	(cons 'file-writable? file-writable?)
	(cons 'finite? finite?)
	(cons 'float float)
	(cons 'floor floor)
	(cons 'floor-quotient floor-quotient)
	(cons 'floor-remainder floor-remainder)
	(cons 'floor/ floor/)
	(cons 'flush-port (case-lambic 0 (lambda ()
					   (flush-port))
				       1 (lambda (a1)
					   (flush-port a1))))
	(cons 'fmod fmod)
	(cons 'foldl foldl)
	(cons 'foldl1 foldl1)
	(cons 'foldr foldr)
	(cons 'for-each for-each)
	(cons 'fork-process fork-process)
	(cons 'fprintf fprintf)
	(cons 'frexp frexp)
	(cons 'fromto fromto)
	(cons 'gc-version gc-version)
	(cons 'gcd gcd)
	(cons 'gensym gensym)
	(cons 'get-current-directory get-current-directory)
	(cons 'get-domain-name get-domain-name)
	(cons 'get-effective-group-id get-effective-group-id)
	(cons 'get-effective-user-id get-effective-user-id)
	(cons 'get-environment-variable get-environment-variable)
	(cons 'get-errno get-errno)
	(cons 'get-file-eof get-file-eof)
	(cons 'get-file-error get-file-error)
	(cons 'get-file-position get-file-position)
	(cons 'get-file-status get-file-status)
	(cons 'get-group-id get-group-id)
	(cons 'get-group-information
	      (case-lambic 0 (lambda ()
			       (get-group-information))
			   1 (lambda (a1)
			       (get-group-information a1))))
	(cons 'get-host-name get-host-name)
	(cons 'get-parent-process-id get-parent-process-id)
	(cons 'get-process-id get-process-id)
	(cons 'get-session-id get-session-id)
	(cons 'get-symbolic-link-status get-symbolic-link-status)
	(cons 'get-user-id get-user-id)
	(cons 'get-user-information
	      (case-lambic 0 (lambda ()
			       (get-user-information))
			   1 (lambda (a1)
			       (get-user-information a1))))
	(cons 'gregorian-date gregorian-date)
	(cons 'hypot hypot)
	(cons 'i* i*)
	(cons 'i+ i+)
	(cons 'i++ i++)
	(cons 'i- i-)
	(cons 'i-- i--)
	(cons 'i/ i/)
	(cons 'ilog ilog)
	(cons 'imag-part imag-part)
	(cons 'infinite? infinite?)
	(cons 'integer integer)
	(cons 'integer->char integer->char)
	(cons 'integer? integer?)
	(cons 'is-block-device? is-block-device?)
	(cons 'is-char-device? is-char-device?)
	(cons 'is-directory? is-directory?)
	(cons 'is-leap-year? is-leap-year?)
	(cons 'is-named-pipe? is-named-pipe?)
	(cons 'is-prime? is-prime?)
	(cons 'is-regular-file? is-regular-file?)
	(cons 'is-socket? is-socket?)
	(cons 'is-symbolic-link? is-symbolic-link?)
	(cons 'julian-day julian-day)
	(cons 'julian-day-of-easter julian-day-of-easter)
	(cons 'lambert-W lambert-W)
	(cons 'lambert-W+ lambert-W+)
	(cons 'lambert-W- lambert-W-)
	(cons 'lcm lcm)
	(cons 'ldexp ldexp)
	(cons 'list list)
	(cons 'list->bytevector list->bytevector)
	(cons 'list->vector list->vector)
	(cons 'list-append list-append)
	(cons 'list-drop-while list-drop-while)
	(cons 'list-filter list-filter)
	(cons 'list-flatten list-flatten)
	(cons 'list-group-by list-group-by)
	(cons 'list-head list-head)
	(cons 'list-last list-last)
	(cons 'list-length list-length)
	(cons 'list-length=? list-length=?)
	(cons 'list-length>=? list-length>=?)
	(cons 'list-length>? list-length>?)
	(cons 'list-length<? list-length<?)
	(cons 'list-length<=? list-length<=?)
	(cons 'list-partition list-partition)
	(cons 'list-ref list-ref)
	(cons 'list-remove-dups list-remove-dups)
	(cons 'list-reverse list-reverse)
	(cons 'list-sort list-sort)
	(cons 'list-tail list-tail)
	(cons 'list-take-while list-take-while)
	(cons 'list-unhead list-unhead)
	(cons 'list-untail list-untail)
	(cons 'list? list?)
	(cons 'listen-on listen-on)
	(cons 'localtime (case-lambic 0 (lambda ()
					  (localtime))
				      1 (lambda (a1)
					  (localtime a1))))
	(cons 'log log)
	(cons 'log-gamma log-gamma)
	(cons 'make-polar make-polar)
	(cons 'make-rational make-rational)
	(cons 'map map)
	(cons 'map map)
	(cons 'max max)
	(cons 'max/i max/i)
	(cons 'max/q max/q)
	(cons 'max/r max/r)
	(cons 'memp memp)
	(cons 'memv memv)
	(cons 'min min)
	(cons 'min/i min/i)
	(cons 'min/q min/q)
	(cons 'min/r min/r)
	(cons 'nan? nan?)
	(cons 'negative negative)
	(cons 'negative? negative?)
	(cons 'next-prime next-prime)
	(cons 'newline (case-lambic 0 (lambda ()
					(newline))
				    1 (lambda (a1)
					(newline a1))))
	(cons 'not not)
	(cons 'null? null?)
	(cons 'number->string
	      (case-lambic 1 (lambda (a1)
			       (number->string a1))
			   2 (lambda (a1 a2)
			       (number->string a1 a2))
			   3 (lambda (a1 a2 a3)
			       (number->string a1 a2 a3))))
	(cons 'number/type number/type)
	(cons 'numerator numerator)
	(cons 'odd? odd?)
	(cons 'offset-date offset-date)
	(cons 'open-file open-file)
	(cons 'open-temporary-file open-temporary-file)
	(cons 'pair? pair?)
	(cons 'parse-file parse-file)
	(cons 'parse-string parse-string)
	(cons 'pipe-port? pipe-port?)
	(cons 'poly-chebyshev1 poly-chebyshev1)
	(cons 'poly-chebyshev2 poly-chebyshev2)
	(cons 'poly-hermite1 poly-hermite1)
	(cons 'poly-hermite2 poly-hermite2)
	(cons 'poly-laguerre poly-laguerre)
	(cons 'poly-legendre poly-legendre)
	(cons 'port? port?)
	(cons 'positive? positive?)
	(cons 'printf printf)
	(cons 'compiled-procedure? compiled-procedure?)
	(cons 'interpreted-procedure? interpreted-procedure?)
	(cons 'promise? promise?)
	(cons 'promote/cmplx promote/cmplx)
	(cons 'promote/rat promote/rat)
	(cons 'promote/real promote/real)
	(cons 'q* q*)
	(cons 'q+ q+)
	(cons 'q- q-)
	(cons 'q/ q/)
	(cons 'r* r*)
	(cons 'r+ r+)
	(cons 'r- r-)
	(cons 'r/ r/)
	(cons 'raise raise)
	(cons 'random-cauchy
	      (case-lambic 0 (lambda ()
			       (random-cauchy))
			   2 (lambda (a1 a2)
			       (random-cauchy a1 a2))))
	(cons 'random-exponential
	      (case-lambic 0 (lambda ()
			       (random-exponential))
			   1 (lambda (a1)
			       (random-exponential a1))))
	(cons 'random-normal-pair
	      (case-lambic 0 (lambda ()
			       (random-normal-pair))
			   2 (lambda (a1 a2)
			       (random-normal-pair a1 a2))))
	(cons 'random-permutation random-permutation)
	(cons 'random-poisson random-poisson)
	(cons 'random-seed!
	      (case-lambic 0 (lambda ()
			       (random-seed!))
			   1 (lambda (a1)
			       (random-seed! a1))))
	(cons 'random-uniform
	      (case-lambic 0 (lambda ()
			       (random-uniform))
			   2 (lambda (a1 a2)
			       (random-uniform a1 a2))))
	(cons 'rational? rational?)
	(cons 'read-bytes read-bytes)
	(cons 'read-char (case-lambic 0 (lambda ()
					  (read-char))
				      1 (lambda (a1)
					  (read-char a1))))
	(cons 'read-directory (case-lambic 0 (lambda ()
					       (read-directory))
					   1 (lambda (a1)
					       (read-directory a1))))
	(cons 'read-line read-line)
	(cons 'real-part real-part)
	(cons 'real? real?)
	(cons 'reciprocal reciprocal)
	(cons 'regex-match regex-match)
	(cons 'root-bisect root-bisect)
	(cons 'root-bracket root-bracket)
	(cons 'root-ridders root-ridders)
	(cons 'remove-directory remove-directory)
	(cons 'remove-file remove-file)
	(cons 'rename-file rename-file)
	(cons 'replicate replicate)
	(cons 'round round)
	(cons 'run-command run-command)
	(cons 'run-read-command run-read-command)
	(cons 'run-write-command run-write-command)
	(cons 'send-signal send-signal)
	(cons 'set-car! set-car!)
	(cons 'set-cdr! set-cdr!)
	(cons 'set-current-directory set-current-directory)
	(cons 'set-effective-group-id set-effective-group-id)
	(cons 'set-effective-user-id set-effective-user-id)
	(cons 'set-environment-variable set-environment-variable)
	(cons 'set-errno! set-errno!)
	(cons 'set-group-id set-group-id)
	(cons 'set-line-buffering! set-line-buffering!)
	(cons 'set-no-buffering! set-no-buffering!)
	(cons 'set-session-id set-session-id)
	(cons 'set-user-id set-user-id)
	(cons 'sha-256-data? sha-256-data?)
	(cons 'sha-256 sha-256)
	(cons 'sha-256-init sha-256-init)
	(cons 'sha-256-update sha-256-update)
	(cons 'sha-256-finish sha-256-finish)
	(cons 'sha-224-data? sha-224-data?)
	(cons 'sha-224 sha-224)
	(cons 'sha-224-init sha-224-init)
	(cons 'sha-224-update sha-224-update)
	(cons 'sha-224-finish sha-224-finish)
	(cons 'sign sign)
	(cons 'sin sin)
	(cons 'sine-integral sine-integral)
	(cons 'sinh sinh)
	(cons 'sleep sleep)
	(cons 'socket-port? socket-port?)
	(cons 'sprintf sprintf)
	(cons 'sqlite-dump-table sqlite-dump-table)
	(cons 'sqlite-meta-schema sqlite-meta-schema)
	(cons 'sqlite-meta-tables sqlite-meta-tables)
	(cons 'sqlite-port? sqlite-port?)
	(cons 'sqlite-run sqlite-run)
	(cons 'sqlite-statement-bind sqlite-statement-bind)
	(cons 'sqlite-statement-cleanup sqlite-statement-cleanup)
	(cons 'sqlite-statement-info sqlite-statement-info)
	(cons 'sqlite-statement-prepare sqlite-statement-prepare)
	(cons 'sqlite-statement-run sqlite-statement-run)
	(cons 'sqlite-statement? sqlite-statement?)
	(cons 'sqlite-version sqlite-version)
	(cons 'sqrt sqrt)
	(cons 'stack-trace stack-trace)
	(cons 'stack-trace-minimal
	      (case-lambic 0 (lambda ()
			       (stack-trace-minimal))
			   1 (lambda (a1)
			       (stack-trace-minimal a1))))
	(cons 'string->list string->list)
	(cons 'string->number string->number)
	(cons 'string->symbol string->symbol)
	(cons 'string-append string-append)
	(cons 'string-ci-hash-32 string-ci-hash-32)
	(cons 'string-ci-hash-64 string-ci-hash-64)
	(cons 'string-ci/=? string-ci/=?)
	(cons 'string-ci<=? string-ci<=?)
	(cons 'string-ci<? string-ci<?)
	(cons 'string-ci=? string-ci=?)
	(cons 'string-ci>=? string-ci>=?)
	(cons 'string-ci>? string-ci>?)
	(cons 'string-copy
	      (case-lambic 1 (lambda (a1)
			       (string-copy a1))
			   2 (lambda (a1 a2)
			       (string-copy a1 a2))
			   3 (lambda (a1 a2 a3)
			       (string-copy a1 a2 a3))))
	(cons 'string-create
	      (case-lambic 1 (lambda (a1)
			       (string-create a1))
			   2 (lambda (a1 a2)
			       (string-create a1 a2))))
	(cons 'string-downcase string-downcase)
	(cons 'string-find-first-char string-find-first-char)
	(cons 'string-find-last-char string-find-last-char)
	(cons 'string-hash-32 string-hash-32)
	(cons 'string-hash-64 string-hash-64)
	(cons 'string-join-by string-join-by)
	(cons 'string-length string-length)
	(cons 'string-pad-center string-pad-center)
	(cons 'string-pad-left string-pad-left)
	(cons 'string-pad-right string-pad-right)
	(cons 'string-port? string-port?)
	(cons 'string-ref string-ref)
	(cons 'string-reverse string-reverse)
	(cons 'string-set! string-set!)
	(cons 'string-split-by string-split-by)
	(cons 'string-split-by-whitespace string-split-by-whitespace)
	(cons 'string-trim string-trim)
	(cons 'string-trim-left string-trim-left)
	(cons 'string-trim-right string-trim-right)
	(cons 'string-upcase string-upcase)
	(cons 'string/=? string/=?)
	(cons 'string<=? string<=?)
	(cons 'string<? string<?)
	(cons 'string=? string=?)
	(cons 'string>=? string>=?)
	(cons 'string>? string>?)
	(cons 'string? string?)
	(cons 'symbol->string symbol->string)
	(cons 'symbol=? symbol=?)
	(cons 'symbol? symbol?)
	(cons 'tan tan)
	(cons 'tanh tanh)
	(cons 'token-source-line token-source-line)
	(cons 'truncate truncate)
	(cons 'truncate-file
	      (case-lambic 1 (lambda (a1)
			       (truncate-file a1))
			   2 (lambda (a1 a2)
			       (truncate-file a1 a2))))
	(cons 'truncate-quotient truncate-quotient)
	(cons 'truncate-remainder truncate-remainder)
	(cons 'truncate/ truncate/)
	(cons 'type-of type-of)
	(cons 'unset-environment-variable unset-environment-variable)
	(cons 'upfrom upfrom)
	(cons 'vector vector)
	(cons 'vector->list vector->list)
	(cons 'vector-cfft! vector-cfft!)
	(cons 'vector-create
	      (case-lambic 1 (lambda (a1)
			       (vector-create a1))
			   2 (lambda (a1 a2)
			       (vector-create a1 a2))))
	(cons 'vector-fill! vector-fill!)
	(cons 'vector-for-each vector-for-each)
	(cons 'vector-length vector-length)
	(cons 'vector-map vector-map)
	(cons 'vector-map! vector-map!)
	(cons 'vector-number/type vector-number/type)
	(cons 'vector-promote/cmplx! vector-promote/cmplx!)
	(cons 'vector-promote/rat! vector-promote/rat!)
	(cons 'vector-promote/real! vector-promote/real!)
	(cons 'vector-ref vector-ref)
	(cons 'vector-set! vector-set!)
	(cons 'vector-sort! vector-sort!)
	(cons 'vector-swap! vector-swap!)
	(cons 'vector? vector?)
	(cons 'wait-process
	      (case-lambic 0 (lambda ()
			       (wait-process))
			   1 (lambda (a1)
			       (wait-process a1))
			   2 (lambda (a1 a2)
			       (wait-process a1 a2))))
	(cons 'wile-basic-build-info wile-basic-build-info)
	(cons 'wile-build-info wile-build-info)
	(cons 'wile-architecture-name wile-architecture-name)
	(cons 'wile-os-name wile-os-name)
	(cons 'write-bytes write-bytes)
	(cons 'write-string write-string)
	(cons 'write-1str (case-lambic 1 (lambda (a1)
					   (write-1str a1))
				       2 (lambda (a1 a2)
					   (write-1str a1 a2))))
	(cons 'zero? zero?))))

;;; Convert a possibly dotted-list into a proper list, counting the
;;; number of entries along the way; return the length as in arity
;;; above plus the proper list: proper lists report positive length,
;;; dotted lists report negative length:
;;;
;;;     (args-list ())		=> (0 ())
;;;     (args-list '(1 2 3))	=> (3 (1 2 3))
;;;     (args-list '(1 2 . 3))	=> (-3 (1 2 3))
;;;     (args-list 'a)		=> (-1 (a))

(define (args-list dotted-list)
  (let loop ((as dotted-list)
	     (pl ())
	     (na 0))
    (cond ((null? as) (list na (list-reverse pl)))
	  ((pair? as) (loop (cdr as) (cons (car as) pl) (+ na 1)))
	  (else (list (- (- na) 1) (list-reverse (cons as pl)))))))

;;; (define (unique-symbols? lst)
;;;   (let ((syms? (let loop ((ls lst))	;;; check symbolness
;;; 		 (cond ((null? ls) #t)
;;; 		       ((symbol? (car ls)) (loop (cdr ls)))
;;; 		       (else #f)))))
;;;     (if syms?			;;; if symbolness, check uniqueness
;;; 	(let loop ((ss (list-sort (lambda (a b) (string<? a b))
;;; 				   (map symbol->string lst))))
;;; 	  (cond ((null? ss) #t)
;;; 		((null? (cdr ss)) #t)
;;; 		((string=? (car ss) (cadr ss)) #f)
;;; 		(else (loop (cdr ss)))))
;;; 	#f)))

;;; this is a private routine, so we'll forego the error checking...

(define (make-macro name args body env)
  (let* ((argies (args-list args))
	 (arity (car argies))
	 (formals (cadr argies)))
;;;    (unless (unique-symbols? formals)
;;;      (ERR "malformed 'make-macro' args list '%v'" args))
    (make-bbox name (make-iproc formals arity body env #t))))

(define (make-standard-env)
  (let* ((std-env1 (make-stdenv-no-macros))
	 (std-env2 (list-append
		    (list
		     (make-macro
		      'begin-breakable '(tag . actions)
		      '((let ((cname (gensym)))
			  `(guard (,cname ((symbol=? ,cname ,tag)))
				  ,@actions)))
		      std-env1)
		     (make-macro
		      'fluid-let '(vals . body)
		      '((let ((svals (map (lambda (ig) (gensym)) vals))
			      (result (gensym))
			      (vars (map car vals))
			      (tvals (map cadr vals)))
			  `(let ,(map (lambda (s v) `(,s ,v)) svals vars)
			     ,@(map (lambda (v t) `(set! ,v ,t)) vars tvals)
			     (let ((,result (begin ,@body)))
			       ,@(map (lambda (v s) `(set! ,v ,s)) vars svals)
			       ,result))))
		      std-env1)
		     (make-macro
		      'namespace '(syms . defs)
		      '((let* ((nm (gensym))
			       (sa (gensym))
			       (s0 (map (lambda (s)
					  (if (symbol? s)
					      (list s s) s)) syms))
			       (s1 (map (lambda (s)
					  (let ((p (cadr s)))
					    `(define ,p #f))) s0))
			       (s2 (map (lambda (s)
					  (let ((p (car s)))
					    `((symbol=? ,sa ',p) ,p))) s0))
			       (s3 (map (lambda (s)
					  (let ((p1 (car s))
						(p2 (cadr s)))
					    `(set! ,p2 (,nm ',p1)))) s0)))
			  `(begin ,@s1
				  (let ((,nm ((lambda ()
						,@defs
						(lambda (,sa) (cond ,@s2))))))
				    ,@s3))))
		      std-env1)
		     (make-macro
		      'def-struct '(name field . fields)
		      '((let* ((fs (cons field fields))
			       (lfs (list-length fs))
			       (nfs (+ 1 lfs))
			       (J0 (lambda strs
				     (apply string-append strs)))
			       (J1 (lambda (pre main)
				     (string->symbol (J0 pre main))))
			       (J2 (lambda (pre main post)
				     (string->symbol (J0 pre main post))))
			       (nstr (symbol->string name))
			       (mstr (J1 "make-" nstr))
			       (msym (gensym))
			       (istr (J2 "isa-" nstr "?"))
			       (gpre (J0 "get-" nstr "-"))
			       (spre (J0 "set-" nstr "-"))
			       (istrs (map (lambda (f i)
					     `(vector-set! ,msym ,i ,f))
					   fs (fromto 1 lfs)))
			       (gstrs (map (lambda (f i)
					     (let ((gfn (J1 gpre (symbol->string f))))
					       `(define (,gfn it)
						  (vector-ref it ,i))))
					   fs (fromto 1 lfs)))
			       (sstrs (map (lambda (f i)
					     (let ((sfn (J2 spre (symbol->string f) "!")))
					       `(define (,sfn it val)
						  (vector-set! it ,i val))))
					   fs (fromto 1 lfs)))
			       (defs `(begin
					(define (,mstr ,@fs)
					  (let ((,msym (vector-create ,nfs)))
					    (vector-set! ,msym 0 ',name)
					    ,@istrs
					    ,msym))
					(define (,istr it)
					  (and (vector? it)
					       (eqv? (vector-ref it 0) ',name)))
					,@gstrs ,@sstrs)))
			  defs))
		      std-env1))
		    std-env1)))
    (set! standard-env std-env2)
    standard-env))

(define-primitive "wile_std_env_no_macros"
  "returns the wile standard environment without macros for use by the interpreter"
  (wile-standard-environment)
  (if standard-env
      standard-env
      (make-standard-env)))

(define-primitive "wile_env_add_macros"
  "expects one environment list and adds several standard macros to it; uses the standard environment if the environment is given as #f"
  (wile-environment-with-macros env)
  (unless env
    (set! env (if standard-env standard-env (make-standard-env))))
  (list-append
   (list
    (make-macro
     'when '(pred . actions)
     '(`(if ,pred (begin ,@actions) #f))
     env)

    (make-macro
     'unless '(pred . actions)
     '(`(if ,pred #f (begin ,@actions)))
     env)

    (make-macro
     'while '(some-cond . some-actions)
     '((let ((mc (gensym)))
	 `(do ((,mc 0 (+ ,mc 1)))
	      ((not ,some-cond) ,mc)
	    ,@some-actions)))
     env)

    (make-macro
     'until '(some-cond . some-actions)
     '((let ((mc (gensym)))
	 `(do ((,mc 0 (+ ,mc 1)))
	      (,some-cond ,mc)
	    ,@some-actions)))
     env)

    (make-macro
     'do-while '(some-cond . some-actions)
     '((let ((mc (gensym)))
	 `(do ((,mc 0 (+ ,mc 1)))
	      ((and (positive? ,mc) (not ,some-cond)) ,mc)
	    ,@some-actions)))
     env)

    (make-macro
     'do-until '(some-cond . some-actions)
     '((let ((mc (gensym)))
	 `(do ((,mc 0 (+ ,mc 1)))
	      ((and (positive? ,mc) ,some-cond) ,mc)
	    ,@some-actions)))
     env)

    (make-macro
     'assert '(some-cond)
     '(`(when (not ,some-cond)
	  (write-string stderr "assertion failure: ")
	  (display ',some-cond stderr)
	  (newline stderr)
	  (raise "assertion failure!")))
     env)

    (make-macro
     'begin-breakable '(tag . actions)
     '((let ((cname (gensym)))
	 `(guard (,cname ((symbol=? ,cname ,tag)))
		 ,@actions)))
     env)

    (make-macro
     'fluid-let '(vals . body)
     '((let ((svals (map (lambda (ig) (gensym)) vals))
	     (result (gensym))
	     (vars (map car vals))
	     (tvals (map cadr vals)))
	 `(let ,(map (lambda (s v) `(,s ,v)) svals vars)
	    ,@(map (lambda (v t) `(set! ,v ,t)) vars tvals)
	    (let ((,result (begin ,@body)))
	      ,@(map (lambda (v s) `(set! ,v ,s)) vars svals)
	      ,result))))
     env)

    (make-macro
     'namespace '(syms . defs)
     '((let* ((nm (gensym))
	      (sa (gensym))
	      (s0 (map (lambda (s)
			 (if (symbol? s) (list s s) s)) syms))
	      (s1 (map (lambda (s)
			 (let ((p (cadr s)))
			   `(define ,p #f))) s0))
	      (s2 (map (lambda (s)
			 (let ((p (car s)))
			   `((symbol=? ,sa ',p) ,p))) s0))
	      (s3 (map (lambda (s)
			 (let ((p1 (car s))
			       (p2 (cadr s)))
			   `(set! ,p2 (,nm ',p1)))) s0)))
	 `(begin ,@s1
		 (let ((,nm ((lambda ()
			       ,@defs
			       (lambda (,sa) (cond ,@s2))))))
		   ,@s3))))
     env)

    (make-macro
     'def-struct '(name field . fields)
     '((let* ((fs (cons field fields))
	      (lfs (list-length fs))
	      (nfs (+ 1 lfs))
	      (J0 (lambda strs
		    (apply string-append strs)))
	      (J1 (lambda (pre main)
		    (string->symbol (J0 pre main))))
	      (J2 (lambda (pre main post)
		    (string->symbol (J0 pre main post))))
	      (nstr (symbol->string name))
	      (mstr (J1 "make-" nstr))
	      (msym (gensym))
	      (istr (J2 "isa-" nstr "?"))
	      (gpre (J0 "get-" nstr "-"))
	      (spre (J0 "set-" nstr "-"))
	      (istrs (map (lambda (f i)
			    `(vector-set! ,msym ,i ,f))
			  fs (fromto 1 lfs)))
	      (gstrs (map (lambda (f i)
			    (let ((gfn (J1 gpre (symbol->string f))))
			      `(define (,gfn it)
				 (vector-ref it ,i))))
			  fs (fromto 1 lfs)))
	      (sstrs (map (lambda (f i)
			    (let ((sfn (J2 spre (symbol->string f) "!")))
			      `(define (,sfn it val)
				 (vector-set! it ,i val))))
			  fs (fromto 1 lfs)))
	      (defs `(begin
		       (define (,mstr ,@fs)
			 (let ((,msym (vector-create ,nfs)))
			   (vector-set! ,msym 0 ',name)
			   ,@istrs
			   ,msym))
		       (define (,istr it)
			 (and (vector? it)
			      (eqv? (vector-ref it 0) ',name)))
		       ,@gstrs ,@sstrs)))
	 defs))
     env))
   env))

;;; --8><----8><----8><--

;;; TODO: validate syntax better, in lots of places

;;; TODO: it's somehow somewhere not properly tail-recursive

;;; TODO: In the absence of working set-c[ad]r!, we need to be a tiny
;;; bit fancy with environments, so that we can do set!: thus define a
;;; bbox structure, for "binding-box": that has a durable pointer to
;;; a location in heap.

(def-struct bbox name value)

(define (ERR fmt . args)
  (flush-port stderr)
  (raise (apply sprintf
		(string-append fmt " at " (token-source-line args))
		args)))

(define (atom? val)
  (or (null? val)
      (boolean? val)
      (char? val)
      (string? val)
      (integer? val)
      (rational? val)
      (real? val)
      (complex? val)
      (file-port? val)
      (pipe-port? val)
      (socket-port? val)
      (string-port? val)
      (sqlite-port? val)
      (sqlite-statement? val)))

(define (special-form? val)
  (and (symbol? val)
       (or (symbol=? val 'quote)
	   (symbol=? val 'and)
	   (symbol=? val 'begin)
	   (symbol=? val 'case)
	   (symbol=? val 'cond)
;;;	   (symbol=? val 'define)	;;; these are special-special forms
;;;	   (symbol=? val 'defmacro)
	   (symbol=? val 'do)
	   (symbol=? val 'guard)
	   (symbol=? val 'if)
	   (symbol=? val 'lambda)
	   (symbol=? val 'let)
	   (symbol=? val 'let*)
	   (symbol=? val 'letrec)
	   (symbol=? val 'letrec*)
	   (symbol=? val 'or)
	   (symbol=? val 'set!)
	   (symbol=? val 'quasiquote)
	   (symbol=? val 'unquote)
	   (symbol=? val 'unquote-splicing)

	   (symbol=? val 'pragma)
	   )))

(define-primitive "wile_eval_define_form"
  "this is part of the wile interpreter; do not use"
  (define-form? expr)
  (and (pair? expr)
       (symbol? (car expr))
       (or (symbol=? (car expr) 'define)
	   (symbol=? (car expr) 'defmacro))))

(define-primitive "wile_eval_load_path"
  "this is part of the wile interpreter; do not use"
  (load-file-path pathy? fname)
  (if pathy?
      (let* ((paths (string-split-by
		     (lambda (c) (eqv? c #\:))
		     (get-environment-variable "WILE_LIBRARY_PATH")))
	     (filepath (let loop ((ps paths))
			 (if (null? ps)
			     #f
			     (let ((fp (string-join-by "/" (car ps) fname)))
			       (if (file-exists? fp) fp (loop (cdr ps))))))))
	(if filepath
	    filepath
	    (raise (string-append "unable to find file '" fname "'"))))
      fname))

(define-primitive "wile_eval_load_form"
  "this is part of the wile interpreter; do not use"
  (load-form? expr)
  (and (pair? expr)
       (symbol? (car expr))
       (or (symbol=? (car expr) 'load)
	   (symbol=? (car expr) 'load-library))
       (list-length=? 2 expr)))

(define-primitive "wile_eval_begin_form"
  "this is part of the wile interpreter; do not use"
  (begin-form? expr)
  (and (pair? expr)
       (symbol? (car expr))
       (symbol=? (car expr) 'begin)
       (list-length>=? 2 expr)))

(define (unique-symbols? lst)
  (let ((syms? (let loop ((ls lst))	;;; check symbolness
		 (cond ((null? ls) #t)
		       ((symbol? (car ls)) (loop (cdr ls)))
		       (else #f)))))
    (if syms?				;;; if symbolness, check uniqueness
	(let loop ((ss (list-sort (lambda (a b) (string<? a b))
				  (map symbol->string lst))))
	  (cond ((null? ss) #t)
		((null? (cdr ss)) #t)
		((string=? (car ss) (cadr ss)) #f)
		(else (loop (cdr ss)))))
	#f)))

(define (symbol-lookup box? env sym)
  (cond ((null? env)
	 (ERR "symbol '%s' lookup failed!" sym))
	((symbol=? sym (get-bbox-name (car env)))
	 (if box? (car env) (get-bbox-value (car env))))
	(else (symbol-lookup box? (cdr env) sym))))

(define (eval-binding env expr)
  (if (and (pair? expr) (symbol? (car expr)))
      (make-bbox (car expr) (eval env (cadr expr)))
      (ERR "malformed binding expression '%v'" expr)))

(define-primitive "wile_eval_define"
  "this is part of the wile interpreter; do not use"
  (eval-define macro env expr)
  (let* ((bbv (cond ((symbol? (car expr))
		     (eval-binding env expr))
		    ((and (pair? (car expr)) (symbol? (caar expr)))
		     (eval-binding env (list (caar expr)
					     (cons 'lambda
						   (cons (cdar expr)
							 (cdr expr))))))
		    (else
		     (ERR "malformed 'define' expression '%v'" expr))))
	 (bv (get-bbox-value bbv))
	 (new-env (cons bbv env)))
    (when (interpreted-procedure? bv)
      (set-iproc-macro! bv macro)
      (set-iproc-env! bv new-env))
    new-env))

(define-primitive "wile_eval_begin"
  "this is part of the wile interpreter; do not use"
  (eval-begin ebox env expr)
  (if (null? expr)
      ()		;;; TODO: this could alternately be an error
      (let loop ((es expr))
	(let ((ev (cond ((define-form? (car es))
			 (set! env (eval-define (symbol=? (caar es) 'defmacro)
						env (cdar es)))
			 (when ebox (set-bbox-value! ebox env))
			 (get-bbox-value (car env)))
			;;; TODO: load -- needs to recursively update env
			(else (eval env (car es))))
		  ))
	  (if (null? (cdr es))
	      ev
	      (loop (cdr es)))))))

(define (eval-if env exprs)
  (if (list-length=? 3 exprs)
      (let ((t (eval env (car exprs))))
	(eval env ((if t cadr caddr) exprs)))
      (ERR "malformed 'if' expression '%v'" exprs)))

(define (eval-and init env exprs)
  (cond ((not init) init)
	((null? exprs) init)
	(else
	 (eval-and (eval env (car exprs)) env (cdr exprs)))))

(define (eval-or init env exprs)
  (cond (init init)
	((null? exprs) init)
	(else
	 (eval-or (eval env (car exprs)) env (cdr exprs)))))

(define (check-let-bindings type llist)
  (if (and (list? llist) (list-length>=? 2 llist))
      (let ((blist (car llist)))
	(unless (list? blist)
	  (ERR "malformed '%s' bindings '%v' are not a list" type blist))
	(for-each (lambda (bv)
		    (unless (list? bv)
		      (ERR "malformed '%s' binding '%v' is not a list"
			   type bv)))
		  blist)
	(let ((vs (map car blist)))
	  (unless (unique-symbols? vs)
	    (ERR "malformed '%s' bindings list '%v'" type vs))))
      (ERR "malformed '%s' expression '%v'" type llist)))

(define (eval-let rec? star? env exprs)
  (cond ((null? exprs)
	 (ERR "malformed 'let' expression '%v'" exprs))
	((and (list? exprs) (symbol? (car exprs)))
	 (check-let-bindings "named-let" (cdr exprs))
	 (let* ((args (map car (cadr exprs)))
		(vals (map (lambda (v) (eval env (cadr v))) (cadr exprs)))
		(fn (make-iproc args (list-length args) (cddr exprs) () #f))
		(new-env (cons (make-bbox (car exprs) fn) env)))
	   (set-iproc-env! fn new-env)
	   (apply-lambda fn vals)))
	(else
	 (check-let-bindings
	  (string-append "let" (if rec? "rec" "") (if star? "*" "")) exprs)
	 (let* ((new-env env)
		(lvs (map (lambda (iex)
			    (let ((lv (eval-binding new-env iex)))
			      (when star? (set! new-env (cons lv new-env)))
			      lv))
			  (car exprs))))
	   (unless star?
	     (set! new-env (list-append (list-reverse lvs) env)))
	   (when rec?
	     (for-each (lambda (box)
			 (let ((bval (get-bbox-value box)))
			   (when (interpreted-procedure? bval)
			     (set-iproc-env! bval new-env))))
		       lvs))
	   (eval-begin #f new-env (cdr exprs))))))

(define (eval-qq-olist level env expr)
  (let* ((lh (eval-qq level env (car expr)))
	 (lt (eval-qq level env (cdr expr)))
	 (splice? (and (pair? (car expr))
		       (symbol? (caar expr))
		       (symbol=? (caar expr) 'unquote-splicing))))
    (if splice? (list-append lh lt) (cons lh lt))))

(define (eval-qq level env expr)
  (cond ((atom? expr) expr)
	((symbol? expr)
	 (if (positive? level) expr (symbol-lookup #f env expr)))
	((pair? expr)
	 (if (symbol? (car expr))
	     (cond ((or (symbol=? (car expr) 'unquote)
			(symbol=? (car expr) 'unquote-splicing))
		    (if (= level 1)
			(eval env (cadr expr))
			(eval-qq-olist (- level 1) env expr)))
		   ((symbol=? (car expr) 'quasiquote)
		    (eval-qq-olist (+ level 1) env expr))
		   (else
		    (eval-qq-olist level env expr)))
	     (eval-qq-olist level env expr)))
	((vector? expr)
	 (list->vector (eval-qq-olist level env (vector->list expr))))

;;; bytevectors are a little problematic, because they assume the contents
;;; are always bytes, and thus only have space for bytes. so storing some
;;; arbitrary expression in there is not possible, even if it evaluates to
;;; a byte at the end. probably not going to support this for a while if
;;; ever. instead, use a vector, generate only bytes, and then convert it
;;; to a bytevector

;;;	((bytevector? expr)
;;;	 (list->bytevector (eval-qq-olist
;;;			    level env (bytevector->list expr))))
	(else				;;; unimplemented or impossible cases
	 (ERR "malformed 'quasiquote' expression '%v'" expr))))

;;; TODO: this still has one tiny syntactic flaw: if 'else occurs in a
;;; clause before the last, it'll still be evaluated as true, and none
;;; of the others will matter

(define (eval-cond env expr)
  (cond ((null? expr) #f)
	((pair? (car expr))
	 (if (eval env (caar expr))
	     (eval-begin #f env (cdar expr))
	     (eval-cond env (cdr expr))))
	(else (ERR "malformed 'cond' expression '%v'" expr))))

(define (eval-set! env expr)
  (if (and (pair? expr)
	   (symbol? (car expr))
	   (list-length=? 2 expr))
      (let ((box (symbol-lookup #t env (car expr))))
	(set-bbox-value! box (eval env (cadr expr)))
	(get-bbox-value box))
      (ERR "malformed 'set!' expression '%v'" expr)))

(define (eval-guard env expr)
  (let* ((re-raise #f)
	 (re-err #f)
	 (guard-val
	  (guard (err (#t (let ((err-env
				 (cons (make-bbox (caar expr) err) env)))
			    (let loop ((cs (cdar expr)))
			      (cond ((null? cs)
				     (set! re-raise #t)
				     (set! re-err err))
				    ((eval err-env (caar cs))
				     (eval-begin #f err-env (cdar cs)))
				    (else (loop (cdr cs))))))))
		 (eval-begin #f env (cdr expr)))))
    (if re-raise
	(raise re-err)
	guard-val)))

;;; TODO: implement => syntax

(define (eval-case env expr)
  (if (and (list? expr) (list-length>=? 2 expr))
      (let ((val (eval env (car expr))))
	(let loop ((cs (cdr expr)))
	  (if (null? cs)
	      #f
	      (if (and (list? (car cs))
		       (or (and (list? (caar cs))
				(not (null? (caar cs))))
			   (eqv? (caar cs) 'else))
		       (not (null? (cdar cs))))
		  (if (or (eqv? (caar cs) 'else)
			  (memv val (caar cs)))
		      (eval-begin #f env (cdar cs))
		      (loop (cdr cs)))
		  (ERR "malformed 'case' clause '%v'" (car cs))))))
      (ERR "malformed 'case' expression '%v'" expr)))

;;; Convert a possibly dotted-list into a proper list, counting the
;;; number of entries along the way; return the length as in arity
;;; above plus the proper list: proper lists report positive length,
;;; dotted lists report negative length:
;;;
;;;     (args-list ())		=> (0 ())
;;;     (args-list '(1 2 3))	=> (3 (1 2 3))
;;;     (args-list '(1 2 . 3))	=> (-3 (1 2 3))
;;;     (args-list 'a)		=> (-1 (a))

(define (args-list dotted-list)
  (let loop ((as dotted-list)
	     (pl ())
	     (na 0))
    (cond ((null? as) (list na (list-reverse pl)))
	  ((pair? as) (loop (cdr as) (cons (car as) pl) (+ na 1)))
	  (else (list (- (- na) 1) (list-reverse (cons as pl)))))))

(define (eval-lambda env expr)
  (let* ((args (car expr))
	 (argies (args-list args))
	 (arity (car argies))
	 (formals (cadr argies)))
    (unless (unique-symbols? formals)
      (ERR "malformed 'lambda' args list '%v'" args))
    (make-iproc formals arity (cdr expr) env #f)))

(define (check-syntax-do expr)
  (and (list? expr)
       (list-length>=? 2 expr)
       (list? (car expr))
       (list? (cadr expr))
       (list-length>? 0 (cadr expr))))

(define (eval-do env expr)
  (if (check-syntax-do expr)
      (let ((var-info (car expr))
	    (test (cadr expr))
	    (body (cddr expr)))
	(for-each (lambda (vdef)
		    (unless (and (list? vdef)
				 (list-length>=? 2 vdef)
				 (symbol? (car vdef)))
		      (ERR "malformed 'do' variable-expr '%v'" vdef)))
		  var-info)
	(unless (and (list? test)
		     (list-length>=? 1 test))
	  (ERR "malformed 'do' test-expr '%v'" test))

	(let loop ((new-bindings
		    (map (lambda (vdef)
			   (make-bbox (car vdef) (eval env (cadr vdef))))
			 var-info)))
	  (let ((new-env (list-append new-bindings env)))
	    (if (eval new-env (car test))
		(eval-begin #f new-env (cdr test))
		(begin
		  (for-each (lambda (e) (eval new-env e)) body)
		  (loop (map (lambda (vdef)
			       (let ((step (if (null? (cddr vdef))
 					       (car vdef)
					       (caddr vdef))))
				 (make-bbox (car vdef)
					    (eval new-env step))))
			     var-info)))))))
      (ERR "malformed 'do' expression '%v'" expr)))

(define-primitive "wile_eval_apply_lambda"
  "expects one procedure and a list of arguments, applies the procedure to the arguments, and returns the result"
  (apply-lambda fn args)
  (let* ((formals (get-iproc-args fn))
	 (arity (get-iproc-arity fn))
	 (a-len (list-length args)))
    (if (negative? arity)
	(let ((nreq (- (- arity) 1)))
	  (unless (>= a-len nreq)
	    (ERR "malformed 'lambda' evaluation: %d required vs %d given args"
		 nreq a-len))
	  (set! args (list-append (list-head args nreq)
				  (list (list-tail args nreq)))))
	(unless (= a-len arity)
	  (ERR "malformed 'lambda' evaluation: %d formal vs %d actual args"
	       arity a-len)))
    (eval-begin
     #f
     (let loop ((fs formals)
		(as args)
		(env (get-iproc-env fn)))
       (if (null? fs)
	   env
	   (loop (cdr fs) (cdr as)
		 (cons (make-bbox (car fs) (car as)) env))))
     (get-iproc-body fn))))

(define-primitive "wile_eval_apply_interp"
  "expects one procedure and any number of arguments and applies the procedure to the arguments"
  (apply-interp fn . args)
  (let* ((a1 (list-reverse args))
	 (a2 (let loop ((acc (car a1))
			(lst (cdr a1)))
	       (if (null? lst)
		   acc
		   (loop (cons (car lst) acc) (cdr lst))))))
    (if (interpreted-procedure? fn)
	(apply-lambda fn a2)
	(apply fn a2))))

(define-primitive "wile_eval"
  "expects one environment list and and expression and returns the result of evaluating the expression in that environment"
  (eval env expr)
  (cond ((or (atom? expr) (vector? expr) (bytevector? expr)) expr)
	((symbol? expr)
	 (symbol-lookup #f env expr))
	((pair? expr)
	 (cond ((special-form? (car expr))
		(case (car expr)
		  ((quote) (cadr expr))
		  ((begin) (eval-begin #f env (cdr expr)))
		  ((if) (eval-if env (cdr expr)))
		  ((and) (eval-and #t env (cdr expr)))
		  ((or) (eval-or #f env (cdr expr)))
		  ((let) (eval-let #f #f env (cdr expr)))
		  ((let*) (eval-let #f #t env (cdr expr)))
		  ((letrec) (eval-let #t #f env (cdr expr)))
		  ((letrec*) (eval-let #t #t env (cdr expr)))
		  ((cond) (eval-cond
			   (cons (make-bbox 'else #t) env) (cdr expr)))
		  ((set!) (eval-set! env (cdr expr)))
		  ((guard) (eval-guard env (cdr expr)))
		  ((case) (eval-case env (cdr expr)))
		  ((lambda) (eval-lambda env (cdr expr)))
		  ((do) (eval-do env (cdr expr)))
		  ((pragma) (fprintf stderr ";;; ignoring %v\n" expr))
		  ((quasiquote)
		   (if (list-length=? 2 expr)
		       (eval-qq 1 env (cadr expr))
		       (ERR "malformed 'quasiquote' expression '%v'"
			    (cdr expr))))
		  ((unquote)
		   (ERR "naked 'unquote' expression '%v'" expr))
		  ((unquote-splicing)
		   (ERR "naked 'unquote-splicing' expression '%v'" expr))
		  (else 'unimplemented-special-form)))
	       ((define-form? expr)
		(ERR "misplaced 'define' expression '%v'" expr))
	       (else
		(let ((ator (eval env (car expr))))
		  (if (interpreted-procedure? ator)
		      (if (get-iproc-macro ator)
			  (eval env (apply-lambda ator (cdr expr)))
			  (apply-lambda ator (map (lambda (e)
						    (eval env e))
						  (cdr expr))))
		      (apply ator (map (lambda (e) (eval env e))
				       (cdr expr))))))))))

;;;	((promise? expr) 'promise)
;;;	((procedure? expr) 'procedure)
;;;	((continuation? expr) 'continuation)
