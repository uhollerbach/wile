;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

(load "stats.scm")
(load "test.scm")

(test-title "stats tests")

;;; 'off 'summary 'report-failed 'report

(test-mode 'report-failed)

(test 5.0 (average 5.0 3.0 9.0 1.0 7.0))

(test (sqrt 10) (stdev 5.0 3.0 9.0 1.0 7.0))

(test 5.0 (median 5.0 3.0 9.0 1.0 7.0))

(define anscombe-1 '((10.0 8.04) (8.0 6.95) (13.0 7.58) (9.0 8.81)
		     (11.0 8.33) (14.0 9.96) (6.0 7.24) (4.0 4.26)
		     (12.0 10.84) (7.0 4.82) (5.0 5.68)))

(define anscombe-2 '((10.0 9.14) (8.0 8.14) (13.0 8.74) (9.0 8.77)
		     (11.0 9.26) (14.0 8.10) (6.0 6.13) (4.0 3.10)
		     (12.0 9.13) (7.0 7.26) (5.0 4.74)))

(define anscombe-3 '((10.0 7.46) (8.0 6.77) (13.0 12.74) (9.0 7.11)
		     (11.0 7.81) (14.0 8.84) (6.0 6.08) (4.0 5.39)
		     (12.0 8.15) (7.0 6.42) (5.0 5.73)))

(define anscombe-4 '((8.0 6.58) (8.0 5.76) (8.0 7.71) (8.0 8.84)
		     (8.0 8.47) (8.0 7.04) (8.0 5.25) (19.0 12.50)
		     (8.0 5.56) (8.0 7.91) (8.0 6.89)))

;;; probably need to supply some kind of tolerance to make this robust

(test 9.0 (apply average (map car anscombe-1)))
(test 9.0 (apply average (map car anscombe-2)))
(test 9.0 (apply average (map car anscombe-3)))
(test 9.0 (apply average (map car anscombe-4)))

(test (sqrt 11) (apply stdev (map car anscombe-1)))
(test (sqrt 11) (apply stdev (map car anscombe-2)))
(test (sqrt 11) (apply stdev (map car anscombe-3)))
(test (sqrt 11) (apply stdev (map car anscombe-4)))

(define (avg-eqv? a b) (< (abs (- a b)) 1.0e-3))

(test-pred avg-eqv? 7.50 (apply average (map cadr anscombe-1)))
(test-pred avg-eqv? 7.50 (apply average (map cadr anscombe-2)))
(test-pred avg-eqv? 7.50 (apply average (map cadr anscombe-3)))
(test-pred avg-eqv? 7.50 (apply average (map cadr anscombe-4)))

(define (stdev-eqv? a b) (< (abs (- a b)) 1.2e-3))

(test-pred stdev-eqv? (sqrt 4.123) (apply stdev (map cadr anscombe-1)))
(test-pred stdev-eqv? (sqrt 4.123) (apply stdev (map cadr anscombe-2)))
(test-pred stdev-eqv? (sqrt 4.123) (apply stdev (map cadr anscombe-3)))
(test-pred stdev-eqv? (sqrt 4.123) (apply stdev (map cadr anscombe-4)))

(define (corr-eqv? a b) (< (abs (- a b)) 2.0e-4))

(test-pred corr-eqv? 0.8164 (correlation anscombe-1))
(test-pred corr-eqv? 0.8164 (correlation anscombe-2))
(test-pred corr-eqv? 0.8164 (correlation anscombe-3))
(test-pred corr-eqv? 0.8164 (correlation anscombe-4))

(define (best-line-eqv? a b)
  (and (< (abs (- (car a) (car b))) 1.5e-3)
       (< (abs (- (cadr a) (cadr b))) 3.0e-4)))

(test-pred best-line-eqv? (list 3.001 0.5) (best-fit-line anscombe-1))
(test-pred best-line-eqv? (list 3.001 0.5) (best-fit-line anscombe-2))
(test-pred best-line-eqv? (list 3.001 0.5) (best-fit-line anscombe-3))
(test-pred best-line-eqv? (list 3.001 0.5) (best-fit-line anscombe-4))

(define (best-para-eqv? a b)
  (and (< (abs (- (car a) (car b))) 2.7e-13)
       (< (abs (- (cadr a) (cadr b))) 2.6e-13)
       (< (abs (- (caddr a) (caddr b))) 1.5e-14)))

(test-pred best-para-eqv?
	   (list 0.7550675990676 1.069251748252 -0.03162004662005)
	   (best-fit-parabola anscombe-1))
(test-pred best-para-eqv?
	   (list -5.995734265734 2.780839160839 -0.1267132867133)
	   (best-fit-parabola anscombe-2))
(test-pred best-para-eqv?
	   (list 5.111766899767 -0.03502797202797 0.02970862470862)
	   (best-fit-parabola anscombe-3))

(define ex
  (map (lambda (x) (list x (* 1.27646587 (exp (* 0.105364532 x)))))
       (fromto -10 10)))

(define (best-ex-eqv? a b)
  (and (< (abs (- (car a) (car b))) 1.0e-20)
       (< (abs (- (cadr a) (cadr b))) 1.0e-20)))

(test-pred best-ex-eqv? (list 1.27646587 0.105364532)
	   (best-fit-exponential ex))

;;; TODO: this test doesn't work yet because expt assumes its inputs are
;;; integers; need to generalize that
;;;
;;; (define po
;;;   (map (lambda (x) (list x (* pi (expt x -1/3)))) (fromto 1 30)))
;;;
;;; (test-pred best-ex-eqv? (list pi (/ -1.0 3.0)) (best-fit-power po))

(define (model pt)
  (let ((x (car pt))
	(y (cadr pt)))
    (list y 1 x (* x x))))

(test-pred best-para-eqv?
	   (list -5.995734265734 2.780839160839 -0.1267132867133)
	   (best-fit-general model anscombe-2))

(test-report "stats test final result")

(exit (if (test-expected 32 0 0 0) 0 1))
