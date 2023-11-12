;;; probe floating-point numbers, regardless of which version
;;; wile is configured for

;;; double		eps 1.11e-16 nbits 53
;;; long-double		eps 5.42e-20 nbits 64
;;; quad-double		eps 9.63e-35 nbits 113

(let* ((cdata (wile-build-info #f))
       (dtype (cxr "cadadddr" cdata))
       (eps 1.0)
       (nbits 0)
       (eeps #f)
       (enbits #f))
  (printf "%v\n" dtype)
  (until (= 1.0 (+ 1.0 eps))
	 (set! eps (* eps 0.5))
	 (set! nbits (+ nbits 1)))
  (printf "final eps %.6e nbits %d\n" eps nbits)
  (cond ((symbol=? dtype 'double)
	 (set! eeps 1.11e-16)
	 (set! enbits 53))
	((symbol=? dtype 'long-double)
	 (set! eeps 5.42e-20)
	 (set! enbits 64))
	((symbol=? dtype 'quad-double)
	 (set! eeps 9.63e-35)
	 (set! enbits 113))
	(else (raise (sprintf "error! unknown double type %v" dtype))))
  (if (and (< (abs (- eps eeps)) (* 0.01 eeps)) (= nbits enbits))
      (printf "eps-test pass: %.9e %d\n" eps nbits)
      (printf "eps-test FAIL: %.9e %d expect %.9e %d\n"
	       eps nbits eeps enbits)))
