;;; Solve the Lotka-Volterra equation
;;;
;;; dy1                   dy2
;;; --- = y1*(a - b*y2)   --- = -y2*(c - d*y1)
;;;  dt                    dt
;;;
;;; Represent the solution as a tuple (y1,y2), then we need three functions
;;; to manipulate these: a summer, a scaler, and a derivative operator: all
;;; very trivial functions.
;;;
;;; Y1 is the prey population, Y2 the predator

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generic Runge-Kutta stuff

;;; summer and scaler for states which are lists of numbers

(define (runge-kutta-+ v1 v2)
  (if (null? v1)
      ()
      (cons (+ (car v1) (car v2))
	     (runge-kutta-+ (cdr v1) (cdr v2)))))

(define (runge-kutta-* s v)
  (if (null? v)
      ()
      (cons (* s (car v)) (runge-kutta-* s (cdr v)))))

;;; Kutta's other classic fourth-order method... "The first [above] is
;;; more popular, the second is more precise." (Hairer, Norsett, Wanner)

;;; routine 4b
;;; cs = (0 1/3 2/3 1)
;;; as = (() (1/3) (-1/3 1) (1 -1 1))
;;; bs = (1/8 3/8 3/8 1/8)

(define (runge-kutta-4b-setup rk+ rk* fn)
  (let ((ks (vector-create 4)))
    (lambda (dt to yo)
      (vector-set! ks 0 (fn to yo))
      (let ((dti (* dt 1/3)))
	(vector-set! ks 1
		     (fn (+ to dti) (rk+ yo (rk* dti (vector-ref ks 0))))))
      (let ((aux yo))
	(set! aux (rk+ aux (rk* (* dt -1/3) (vector-ref ks 0))))
	(set! aux (rk+ aux (rk* dt (vector-ref ks 1))))
	(vector-set! ks 2 (fn (+ to (* dt 2/3)) aux)))
      (let ((aux yo))
	(set! aux (rk+ aux (rk* dt (vector-ref ks 0))))
	(set! aux (rk+ aux (rk* (- dt) (vector-ref ks 1))))
	(set! aux (rk+ aux (rk* dt (vector-ref ks 2))))
	(vector-set! ks 3 (fn (+ to dt) aux)))
      (let ((aux yo))
	(set! aux (rk+ aux (rk* (* dt 1/8) (vector-ref ks 0))))
	(set! aux (rk+ aux (rk* (* dt 3/8) (vector-ref ks 1))))
	(set! aux (rk+ aux (rk* (* dt 3/8) (vector-ref ks 2))))
	(set! aux (rk+ aux (rk* (* dt 1/8) (vector-ref ks 3))))
	(list (+ to dt) aux)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Volterra stuff

;;; Parameters of the model

(define a 1.1)		;;; prey population growth parameter
(define b 0.4)		;;; prey decay due to presence of predator
(define c 0.4)		;;; predator starvation parameter
(define d 0.1)		;;; predator growth due to presence of prey

(define y1s 10.0)	;;; initial prey population
(define y2s 10.0)	;;; initial predator population

(define step 0.01)

(cond ((null? command-line-arguments))		;;; relax, use defaults
      ((and (list-length=? 1 command-line-arguments)
	    (string=? (car command-line-arguments) "-help"))
       (fprintf stderr
		"usage: %s {0 | 2 | 4 | 6} numeric arguments\nwhere 0 uses all defaults, 2 specifies start conditions,\n4 specifies model parameters, and 5 specifies model\nparameters followed by start conditions\n"
		command-name)
       (exit 0))
      ((list-length=? 2 command-line-arguments)
       (set! y1s (string->number (car command-line-arguments)))
       (set! y2s (string->number (cadr command-line-arguments))))
      ((list-length=? 4 command-line-arguments)
       (set! a (string->number (car command-line-arguments)))
       (set! b (string->number (cadr command-line-arguments)))
       (set! c (string->number (caddr command-line-arguments)))
       (set! d (string->number (cadddr command-line-arguments))))
      ((list-length=? 6 command-line-arguments)
       (set! a (string->number (car command-line-arguments)))
       (set! b (string->number (cadr command-line-arguments)))
       (set! c (string->number (caddr command-line-arguments)))
       (set! d (string->number (cadddr command-line-arguments)))
       (set! y1s (string->number (caddddr command-line-arguments)))
       (set! y2s (string->number (cadddddr command-line-arguments))))
      (else (raise "error: bad command-line arguments, use -help")))

(define (deriv t state)
  (let ((y1 (car state))
	(y2 (cadr state)))
    (list (* y1 (- a (* b y2))) (* y2 (- (* d y1) c)))))

;;; create the stepper function: this will create
;;; and depend on first-class closures

(define stepper (runge-kutta-4b-setup runge-kutta-+ runge-kutta-* deriv))

(define (gen-solution stepper t-start state step t-end chirpy?)
  (let* ((ns (stepper step t-start state))
	 (tn (car ns))
	 (sn (cadr ns)))
    (when chirpy?
	  (printf "%.6f %.9e %.9e\n" t-start (car state) (cadr state)))
    (if (<= tn t-end)
	(gen-solution stepper tn sn step t-end chirpy?)
	(begin (when chirpy? (printf "# All done!\n"))
	       sn))))

(gen-solution stepper 0.0 (list y1s y2s) step 50.0 #t)
