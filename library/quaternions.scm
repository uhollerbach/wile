;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2024, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; play around with quaternions

;;; ... quaternions appear to exude an air of nineteenth century
;;; decay, as a rather unsuccessful species in the struggle-for-life
;;; of mathematical ideas. Mathematicians, admittedly, still keep a
;;; warm place in their hearts for the remarkable algebraic properties
;;; of quaternions but, alas, such enthusiasm means little to the
;;; harder-headed physical scientist.
;;;     - Simon L. Altmann (1986)

(def-struct quaternion a b c d)

(define quaternion? isa-quaternion?)

(define quaternion-create make-quaternion)

(display-object-hook
 'quaternion
 (lambda (q port)
   (write-string
    port "[quaternion "
    (number->string (get-quaternion-a q))
    " : " (number->string (get-quaternion-b q))
    " : " (number->string (get-quaternion-c q))
    " : " (number->string (get-quaternion-d q)))))

(define (quaternion-zero? q)
  (if (isa-quaternion? q)
      (and (zero? (get-quaternion-a q))
	   (zero? (get-quaternion-b q))
	   (zero? (get-quaternion-c q))
	   (zero? (get-quaternion-d q)))
      (raise "quaternion-zero? expects a quaternion!")))

(define (quaternion-neg q)
  (if (isa-quaternion? q)
      (quaternion-create (- (get-quaternion-a q)) (- (get-quaternion-b q))
			 (- (get-quaternion-c q)) (- (get-quaternion-d q)))
      (raise "quaternion-neg expects a quaternion!")))

(define (quaternion-recip q)
  (if (isa-quaternion? q)
      (let* ((a (get-quaternion-a q))
	     (b (get-quaternion-b q))
	     (c (get-quaternion-c q))
	     (d (get-quaternion-d q))
	     (n (+ (* a a) (* b b) (* c c) (* d d)))
	     (nn (- n)))
	(quaternion-create (/ a n) (/ b nn)) (/ c nn) (/ d nn))
      (raise "quaternion-recip expects a quaternion!")))

(define (quaternion-conj q)
  (if (isa-quaternion? q)
      (quaternion-create (get-quaternion-a q) (- (get-quaternion-b q))
			 (- (get-quaternion-c q)) (- (get-quaternion-d q)))
      (raise "quaternion-conj expects a quaternion!")))

(define (quaternion-norm q)
  (if (isa-quaternion? q)
      (let ((a (get-quaternion-a q))
	    (b (get-quaternion-b q))
	    (c (get-quaternion-c q))
	    (d (get-quaternion-d q)))
	(sqrt (+ (* a a) (* b b) (* c c) (* d d))))
      (raise "quaternion-norm expects a quaternion!")))

(define (quaternion=? q1 q2)
  (if (and (isa-quaternion? q1)
	   (isa-quaternion? q2))
      (and (= (get-quaternion-a q1) (get-quaternion-a q2))
	   (= (get-quaternion-b q1) (get-quaternion-b q2))
	   (= (get-quaternion-c q1) (get-quaternion-c q2))
	   (= (get-quaternion-d q1) (get-quaternion-d q2)))
      (raise "quaternion=? expects quaternions!")))

(define (quaternion-add q1 q2)
  (if (and (isa-quaternion? q1)
	   (isa-quaternion? q2))
      (quaternion-create
       (+ (get-quaternion-a q1) (get-quaternion-a q2))
       (+ (get-quaternion-b q1) (get-quaternion-b q2))
       (+ (get-quaternion-c q1) (get-quaternion-c q2))
       (+ (get-quaternion-d q1) (get-quaternion-d q2)))
      (raise "quaternion-add expects quaternions!")))

(define (quaternion-sub q1 q2)
  (if (and (isa-quaternion? q1)
	   (isa-quaternion? q2))
      (quaternion-create
       (- (get-quaternion-a q1) (get-quaternion-a q2))
       (- (get-quaternion-b q1) (get-quaternion-b q2))
       (- (get-quaternion-c q1) (get-quaternion-c q2))
       (- (get-quaternion-d q1) (get-quaternion-d q2)))
      (raise "quaternion-sub expects quaternions!")))

(define (quaternion-scale q r)
  (cond ((and (isa-quaternion? q) (real? r))
	 (quaternion-create
	  (* (get-quaternion-a q) r) (* (get-quaternion-b q) r)
	  (* (get-quaternion-c q) r) (* (get-quaternion-d q) r)))
	((and (real? q) (isa-quaternion? r))
	 (quaternion-create
	  (* (get-quaternion-a r) q) (* (get-quaternion-b r) q)
	  (* (get-quaternion-c r) q) (* (get-quaternion-d r) q)))
	(else
	 (raise "quaternion-scale expects a quaternion and a real!"))))

(define (quaternion-mul q1 q2)
  (if (and (isa-quaternion? q1)
	   (isa-quaternion? q2))
      (let* ((a1 (get-quaternion-a q1))
	     (b1 (get-quaternion-b q1))
	     (c1 (get-quaternion-c q1))
	     (d1 (get-quaternion-d q1))
	     (a2 (get-quaternion-a q2))
	     (b2 (get-quaternion-b q2))
	     (c2 (get-quaternion-c q2))
	     (d2 (get-quaternion-d q2))
	     (a1a2 (* a1 a2))
	     (a1b2 (* a1 b2))
	     (a1c2 (* a1 c2))
	     (a1d2 (* a1 d2))
	     (b1a2 (* b1 a2))
	     (b1b2 (* b1 b2))
	     (b1c2 (* b1 c2))
	     (b1d2 (* b1 d2))
	     (c1a2 (* c1 a2))
	     (c1b2 (* c1 b2))
	     (c1c2 (* c1 c2))
	     (c1d2 (* c1 d2))
	     (d1a2 (* d1 a2))
	     (d1b2 (* d1 b2))
	     (d1c2 (* d1 c2))
	     (d1d2 (* d1 d2)))
	(quaternion-create
	 (- a1a2 b1b2 c1c2 d1d2)
	 (+ b1a2 a1b2 c1d2 (- d1c2))
	 (+ c1a2 a1c2 d1b2 (- b1d2))
	 (+ d1a2 a1d2 b1c2 (- c1b2))))
      (raise "quaternion-mul expects quaternions!")))

(define (quaternion-div q1 q2)
  (if (and (isa-quaternion? q1)
	   (isa-quaternion? q2))
      (quaternion-mul q1 (quaternion-recip q2))
      (raise "quaternion-div expects quaternions!")))
