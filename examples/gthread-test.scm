;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

(load-library "gthread.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(thread-mbox-setup chan1 "go")		;;; or set it up empty

(thread-spawn (lambda ()
		(printf "got initial msg %s\n" (thread-mbox-read chan1))
		(do ((i 0 (+ i 1)))
		    ((> i 9) #t)
		  (printf "tick %d\n" i)
		  (printf "got msg %v\n" (thread-mbox-read chan1))
		  (sleep 0.5)
		  (thread-yield)
		  (sleep 0.5)
		  (thread-yield))))

(write-string "first thread created\n")

(thread-spawn (lambda ()
		(do ((i 0 (+ i 1)))
		    ((> i 15) #t)
		  (printf "tock     %d\n" i)
		  (when (> i 5)
			(let ((msg (string-join-by "" "go" (number->string i))))
			  (printf "send msg %v\n" msg)
			  (thread-mbox-write chan1 msg)))
		  (sleep 0.5)
		  (thread-yield))))

(write-string "second thread created\n")

(thread-spawn (lambda ()
		(do ((i 0 (+ i 1)))
		    ((> i 30) #t)
		  (printf "tack         %d\n" i)
		  (printf "mbox is %v\n" chan1)
		  (sleep 0.5)
		  (thread-yield))
		(printf "shutting down NOW!\n")
		(exit 0)))

(write-string "third thread created\n")

(thread-run)

(write-string "gthread code finished running\n")
