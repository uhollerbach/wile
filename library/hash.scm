;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; A pure-scheme implementation of some of SRFI-125? hash-table stuff

;;; (make-hash-table hash-proc equiv-pred? . size) alias hash-table-create
;;; (hash-table? obj)
;;; (hash-table-hash-function ht)
;;; (hash-table-equivalence-function ht)
;;; (hash-table-size ht)
;;; (hash-table-copy ht)		TODO test this
;;; (hash-table-keys ht)
;;; (hash-table-values ht)
;;; (hash-table-entries ht)		TODO test this
;;; (hash-table-clear! ht . size)
;;; (hash-table-set! ht key val)
;;; (hash-table-delete! ht key)
;;; (hash-table-contains? ht key)
;;; (hash-table-ref ht key def)
;;; (hash-table-dump ht)

;;; TODO
;;;
;;; wrap everything in a (namespace), make only selected routines visible
;;;
;;; implement
;;; (hash-table-update! ht key proc default)
;;; (hash-table-entries ht)
;;; (hash-table-mutable? ht)
;;; (equal-hash obj)
;;;
;;; not doing mutable stuff just yet... everything is mutable

;;; these use eq? and eqv? respectively as their equiv?-proc
;;; keys are arbitrary, some internal routine will presumably generate hashes
;;; (make-eq-hash-table ?k?)
;;; (make-eqv-hash-table ?k?)

;;; a hash-table is a self-identifying vector
;;; * element 0 is the symbol 'hash-table
;;; * element 1 is the hash proc
;;; * element 2 is the equality predicate
;;; * element 3 is the size of the hash-table vector
;;; * element 4 is number of keys stored in the hash-table
;;; * element 5 is the table proper, another vector

;;; maximum hash-table size is ~2^61... should be enough for most applications?

(define hash-table-int-sizes
  '(11 23 47 97 191 379 751 1481 2917 5749 11329 22343 44017 86719
      170837 336551 663007 1306133 2573099 5069011 9985961 19672361
      38754559 76346497 150402611 296293157 583697531 1149884137
      2265271793 4462585433 8791293307 17318847823 34118130259 67212716639
      132409051783 260845832021 513866289143 1012316589623 1994263681561
      3928699452761 7739537921977 15246889706317 30036372721541
      59171654261441 116568158895061 229639273023287 452389367855879
      891207054676093 1755677897711911 3458685458492471 6813610353230237
      13422812395863581 26442940419851257 52092592627107007
      102622407475400809 202166142726539647 398267301171283073
      784586583307427611 1545635569115632391 3044902071157795853))

;;; Maximum insertion ratio we'll allow; values in the range 5-10 are reasonable

(define hash-table-int-max-ins 4.0)

(define (hash-table-int-size-find n)
  (let loop ((vs hash-table-int-sizes))
    (let ((v (car vs))
	  (vr (cdr vs)))
      (if (or (>= v n) (null? vr))
	  v
	  (loop vr)))))

(define (hash-table-int-slot ht key)
  (modulo ((hash-table-hash-function ht) key) (vector-ref ht 3)))

(define (make-hash-table hash-proc equiv-pred? . size)
  (let* ((sz (hash-table-int-size-find (if (null? size) 0 (car size))))
	 (ht (make-vector 6)))
    (vector-set! ht 0 'hash-table)		;;; the symbol 'hash-table
    (vector-set! ht 1 hash-proc)		;;; the hash proc
    (vector-set! ht 2 equiv-pred?)		;;; the equality predicate
    (vector-set! ht 3 sz)			;;; the size of the hash-table
    (vector-set! ht 4 0)			;;; number of keys stored
    (vector-set! ht 5 (make-vector sz))		;;; the table proper
    ht))

(define hash-table-create make-hash-table)

(define (hash-table? obj)
  (and (vector? obj) (eqv? (vector-ref obj 0) 'hash-table)))

(define (hash-table-hash-function ht)
  (if (hash-table? ht)
      (vector-ref ht 1)
      (raise "hash-table-hash-function got a non-hash-table object")))

(define (hash-table-equivalence-function ht)
  (if (hash-table? ht)
      (vector-ref ht 2)
      (raise "hash-table-equivalence-function got a non-hash-table object")))

(define (hash-table-size ht)
  (if (hash-table? ht)
      (vector-ref ht 4)
      (raise "hash-table-size got a non-hash-table object")))

(define (hash-table-copy ht)
  (if (hash-table? ht)
      (let ((hc (make-hash-table (vector-ref ht 1)
				 (vector-ref ht 2)
				 (vector-ref ht 3))))
	(for-each (lambda (kv)
		    (hash-table-set! hc (car kv) (cdr kv)))
		  (hash-table-entries ht))
	hc)
      (raise "hash-table-copy got a non-hash-table object")))

;;; TODO: this returns a list, r6rs-lib says a vector

(define (hash-table-keys ht)
  (if (hash-table? ht)
      (let* ((ks ())
	     (sz (vector-ref ht 3))
	     (cv (vector-ref ht 5))
	     (iter (lambda (ch)
		     (until (null? ch)
			    (set! ks (cons (caar ch) ks))
			    (set! ch (cdr ch))))))
	(do ((i 0 (+ i 1)))
	    ((= i sz) ks)
	  (iter (vector-ref cv i))))
      (raise "hash-table-keys got a non-hash-table object")))

;;; not in r6rs-lib; analogous to hash-table-keys, but returns values

(define (hash-table-values ht)
  (if (hash-table? ht)
      (let* ((vs ())
	     (sz (vector-ref ht 3))
	     (cv (vector-ref ht 5))
	     (iter (lambda (ch)
		     (until (null? ch)
			    (set! vs (cons (cdar ch) vs))
			    (set! ch (cdr ch))))))
	(do ((i 0 (+ i 1)))
	    ((= i sz) vs)
	  (iter (vector-ref cv i))))
      (raise "hash-table-values got a non-hash-table object")))

;;; ditto, but returns (key . value) entries

(define (hash-table-entries ht)
  (if (hash-table? ht)
      (let* ((es ())
	     (sz (vector-ref ht 3))
	     (cv (vector-ref ht 5))
	     (iter (lambda (ch)
		     (until (null? ch)
			    (set! es (cons (car ch) es))
			    (set! ch (cdr ch))))))
	(do ((i 0 (+ i 1)))
	    ((= i sz) es)
	  (iter (vector-ref cv i))))
      (raise "hash-table-entries got a non-hash-table object")))

(define (hash-table-clear! ht . size)
  (if (hash-table? ht)
      (begin
	(unless (null? size)
		(vector-set! ht 3 (hash-table-int-size-find (car size))))
	(vector-set! ht 4 0)
	(vector-set! ht 5 (make-vector (vector-ref ht 3)))
	#t)
      (raise "hash-table-clear! got a non-hash-table object")))

(define (hash-table-set! ht key val)
  (if (hash-table? ht)
      (let* ((slot (hash-table-int-slot ht key))
	     (eqf (vector-ref ht 2))
	     (cv (vector-ref ht 5))
	     (found #f)
	     (fkvs (let loop ((kvs (vector-ref cv slot)))
		     (cond ((null? kvs) ())
			   ((eqf key (caar kvs))
			    (set! found (car kvs))
			    (cdr kvs))
			   (else (cons (car kvs) (loop (cdr kvs))))))))
	(vector-set! cv slot (cons (cons key val) fkvs))
	(unless found
		(vector-set! ht 4 (+ 1 (vector-ref ht 4))))
;;; TODO: check chain length and possibly regrow

	(when (> (vector-ref ht 4)
		 (* hash-table-int-max-ins (vector-ref ht 3)))
	      (let* ((szo (vector-ref ht 3))
		     (cvo (vector-ref ht 5))
		     (sz (hash-table-int-size-find (+ szo 1)))
		     (cv (make-vector sz)))
		(vector-set! ht 3 sz)
		(vector-set! ht 5 cv)
		(do ((i 0 (+ i 1)))
		    ((>= i szo) ())
		  (let loop ((kvs (vector-ref cvo i)))
		    (if (null? kvs)
			()
			(let ((j (hash-table-int-slot ht (caar kvs))))
			  (vector-set! cv j (cons (car kvs) (vector-ref cv j)))
			  (loop (cdr kvs))))))))
	(if found (cons #t (cdr found)) (cons #f ())))
      (raise "hash-table-set! got a non-hash-table object")))

(define (hash-table-delete! ht key)
  (if (hash-table? ht)
      (let* ((slot (hash-table-int-slot ht key))
	     (eqf (vector-ref ht 2))
	     (cv (vector-ref ht 5)))
	(vector-set! cv slot (let loop ((kvs (vector-ref cv slot)))
			       (cond ((null? kvs) ())
				     ((eqf key (caar kvs))
				      (vector-set! ht 4 (- (vector-ref ht 4) 1))
				      (cdr kvs))
				     (else (cons (car kvs) (loop (cdr kvs))))))))
      (raise "hash-table-delete! got a non-hash-table object")))

(define (hash-table-contains? ht key)
  (if (hash-table? ht)
      (let ((slot (hash-table-int-slot ht key))
	    (eqf (vector-ref ht 2)))
	(let loop ((kvs (vector-ref (vector-ref ht 5) slot)))
	  (cond ((null? kvs) #f)
		((eqf key (caar kvs)) #t)
		(else (loop (cdr kvs))))))
      (raise "hash-table-contains? got a non-hash-table object")))

(define (hash-table-ref ht key def)
  (if (hash-table? ht)
      (let ((slot (hash-table-int-slot ht key))
	    (eqf (vector-ref ht 2)))
	(let loop ((kvs (vector-ref (vector-ref ht 5) slot)))
	  (cond ((null? kvs) def)
		((eqf key (caar kvs)) (cdar kvs))
		(else (loop (cdr kvs))))))
      (raise "hash-table-contains? got a non-hash-table object")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hash-table-dump ht)
  (if (hash-table? ht)
      (let ((sz (vector-ref ht 3))
	    (nv (vector-ref ht 4))
	    (cv (vector-ref ht 5)))
	(printf "#### hash-table %d/%d\n" sz nv)
	(do ((i 0 (+ i 1)))
	    ((= i sz) #t)
	  (printf "  %d\n" i)
	  (let loop ((vs (vector-ref cv i)))
	    (if (null? vs)
		#t
		(begin (printf "    %v:\t%v\n" (caar vs) (cdar vs))
		       (loop (cdr vs)))))))
      (raise "hash-table-dump got a non-hash-table object")))
