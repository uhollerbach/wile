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

(define-primitive "wile_list2bytevector" ""
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
  "expects three non-negative integer inputs a n m and returns (a^n) modulo m computed in an efficient manner that avoids extremely large numbers"
  (expmod a n m)
  (cond ((negative? n) #f)  ;;; (raise "expmod got a negative exponent"))
	((zero? n) (modulo 1 m))
	((even? n) (expmod (modulo (i* a a) m) (bits-shift n -1) m))
	(else (modulo (i* a (expmod a (i- n 1) m)) m))))

;;; --8><----8><----8><--

(define-primitive "wile_string_join_by"
  "expects one or more strings, and returns the result of using the first string as a separator between the concatenation of all the rest"
  (string-join-by join . strs)
  (let ((ss (if (and (= 1 (list-length strs)) (list? (car strs)))
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

(define-primitive "wile_string_split_by"
  "expects a character-testing predicate indicating which characters to drop and a string, and returns a list of sub-strings whose characters did not get dropped"
  (string-split-by drop? str)
  (let loop ((l (list-drop-while drop? (string->list str)))
	     (a ()))
    (let* ((proto
	    (list-take-while (lambda (c) (not (drop? c))) l))
	   (rest (list-drop-while drop? (cadr proto)))
	   (app (cons (char->string (car proto)) a)))
      (if (zero? (list-length rest))
	  (list-reverse app)
	  (loop rest app)))))

(define-primitive "wile_string_split_by_whitespace"
  "expects a string and returns a list of the non-whitespace sub-strings"
  (string-split-by-whitespace str)
  (string-split-by char-whitespace? str))

;;; --8><----8><----8><--

(define-primitive "wile_map1"
  "expects one procedure of one argument and one list, applies the procedure to each element of the list, and returns the list of results"
  (map1 proc lst)
  (let loop ((l1 lst)
	     (l2 ()))
    (if (null? l1)
	(list-reverse l2)
	(loop (cdr l1) (cons (proc (car l1)) l2)))))

;;; --8><----8><----8><--

(define-primitive "wile_foldl1"
  "expects a values-combining function and a non-empty list, and returns the left fold of that function over that list, using the first element of the list as the initial value"
  (foldl1 proc lst)
  (foldl proc (car lst) (cdr lst)))

;;; --8><----8><----8><--

(define-primitive "wile_map"
  "expects a procedure of N arguments and N lists all of the same length, where N is at least 1; applies the procedure to each tuple consisting of taking the jth entry from each of the lists, and returns the list of results"
  (map proc lst . lsts)
  (define (map2 proc acc ls)
    (if (null? (car ls))
	(list-reverse acc)
	(map2 proc (cons (apply proc (map1 car ls)) acc) (map1 cdr ls))))
  (let* ((ls (cons lst lsts))
	 (ll (map1 list-length ls)))
    (if (/= (foldl1 min/i ll) (foldl1 max/i ll))
	(raise "map got lists of different lengths")
	(map2 proc () ls))))

;;; --8><----8><----8><--

(define-primitive "wile_for_each"
  "expects a procedure of N arguments and N lists all of the same length, where N is at least 1, and applies the procedure to each tuple consisting of taking the jth entry from each of the lists; but does not build any list of results"
  (for-each proc lst . lsts)
  (define (fore2 proc ls)
    (if (null? (car ls))
	#t
	(begin (apply proc (map1 car ls))
	       (fore2 proc (map1 cdr ls)))))
  (let* ((ls (cons lst lsts))
	 (ll (map1 list-length ls)))
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
	   (let ((type (foldl max/i 0 (map1 number/type vs))))
	     (if (< type 3)
		 (foldl1 (list-ref m-ops type) (map1 (list-ref p-ops type) vs))
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
	   (let ((type (foldl max/i 0 (map1 number/type vs))))
	     (if (< type 3)
		 (foldl1 (list-ref m-ops type) (map1 (list-ref p-ops type) vs))
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
	(vs (map1 list lst))
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

(define-primitive "wile_assp" ""
  (assp test? lst)
  (cond ((null? lst) #f)
	((test? (caar lst)) (car lst))
	(else (assp test? (cdr lst)))))

(define-primitive "wile_assv"
  ""
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

(define-primitive "wile_list_take_while" ""
  (list-take-while keep? lst)
  (let loop ((keep? keep?)
	     (lst lst)
	     (acc ()))
    (if (and (not (null? lst)) (keep? (car lst)))
	(loop keep? (cdr lst) (cons (car lst) acc))
	(list (list-reverse acc) lst))))

;;; --8><----8><----8><--

(define-primitive "wile_list_remove_dups" "" (list-remove-dups lst)
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

;;; TODO: should really redo these to use sqlite bind stuff instead of
;;; plain run... little Bobby Tables

(define-primitive "wile_sql_meta_schema"
  "expects one sqlite port and one string which is the name of a table, and returns the schema for that table in the form of an SQL CREATE statement"
  (sqlite-meta-schema port tbl)
  (caar
   (sqlite-run port
	       (string-join-by ""
			       "select sql from sqlite_schema where (name = '"
			       tbl "')"))))

;;; --8><----8><----8><--

(define-primitive "wile_sql_dump_table"
  "expects one sqlite port, one string which is the name of a table, and one output port, and dumps the named table into the output port in SQL format"
  (sqlite-dump-table sport tbl oport)
  (let* ((cols (sqlite-run sport
			   (string-join-by "" "pragma table_info('" tbl "')")))
	 (names (map cadr cols))
	 (types (map caddr cols))
	 (vals (sqlite-run sport (string-join-by " " "select * from" tbl)))
	 (nl (string-join-by "," names))
	 ;;; TODO: also check for real-number match
	 (squo (lambda (s)
		 (if (regex-match "^[-+]?[0-9]+$" s)
		     s
		     (string-join-by "" "'" s "'")))))
    (write-string oport "DROP TABLE IF EXISTS " tbl
		  ";\n\nCREATE TABLE " tbl " ("
		  (string-join-by ", "
				  (map (lambda (n t) (string-join-by " " n t))
				       names types))
		  ");\n\nBEGIN TRANSACTION;\n\n")
    (map (lambda (v)
	   (write-string
	    oport "INSERT INTO " tbl " VALUES ("
	    (string-join-by "," (map squo v)) ");\n"))
	 vals)
    (write-string oport "\nCOMMIT;\n")))

;;; --8><----8><----8><--

;;; Some utility routines to manipulate dates

;;; This returns an integer which is the Julian day number at noon + eps, ie,
;;; an instant after it has incremented to a new (Julian) day. Inputs are
;;; 4-digit (or whatever is appropriate) year, month from 1 to 12, day from
;;; 1 to 28/31 as appropriate.

; Jan 1 2000 = 2451545

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

(define-primitive "wile_day_of_year"
  "expects three integers, year month day, and returns the day of the year, from 1 to 365 (or 366)"
  (day-of-year y m d)
  (i++ (vector-ref (vector 0 31 59 90 120 151 181 212 243 273 304 334)
		   (i- m 1))
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
	((procedure? v) 'procedure)
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

;;; TODO: fix this! need to inline this below, this doesn't get
;;; run in library mode, so regex-all is uninitialized

(define regex-all
  (string-join-by "|" (list r-pct r-chr r-obj r-str r-exact r-float)))

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
  (let loop ((acc ())
	     (fstr fstr)
	     (vs vals))
;;;    (let ((rr (regex-match regex-all fstr)))
    (let ((rr (regex-match "%[0-9]*%|%c|%v|%[lrm]?[0-9]*s|%[lrm]?[ +]?[0-9]*[bodx]|%[lrm]?[ +]?[0-9]*(\.[0-9]*)?[fe]" fstr)))
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

(define-primitive "wile_vector_map" ""
  (vector-map proc vec . vecs)
  (let* ((vs (cons vec vecs))
	 (ls (map1 vector-length vs)))
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

(define-primitive "wile_build_info"
  "expects no arguments and returns an list of various build configuration items"
  (wile-build-info)
  (let* ((binfo (wile-basic-build-info)))
    `((compiler-version (0 9 1))
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
      (garbage-collection? ,(not (zero? (bits-and binfo #b0000001))))
      (sqlite? ,(not (zero? (bits-and binfo #b0000100)))))))

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
