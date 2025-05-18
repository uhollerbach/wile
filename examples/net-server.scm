;;; Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: GPLv3 or later, see file 'LICENSE' for details
;;;
;;; Demo program for wile: a teeny-tiny low-accuracy time server
;;;
;;; Compile and run it:
;;;     wile -x net-server.scm tims
;;;     tims [port]
;;; then use your favorite tool to connect to the port and get a timestamp

(let* ((port (if (null? command-line-arguments)
		 2500
		 (string->number (car command-line-arguments))))
       (sock (listen-on port)))
  (unless sock
    (fprintf stderr "error! unable to listen on port %d\n" port)
    (exit 1))
  (let ((pid (fork-process)))
    (if pid
	(if (zero? pid)
	    (begin		;;; child: interesting stuff starts here
	      (set-current-directory "/")
	      (set-session-id)
	      ;;; would really like to redirect rather than closing
	      (close-port stdin)
	      (close-port stdout)
	      (close-port stderr)
	      (let loop ()
		(let* ((remote (car (accept sock)))
		       (now1 (list-head (localtime) 6))
		       ;;; number->string takes 1-3 arguments, compiler isn't
		       ;;; smart enough yet to figure out which version to use
		       ;;; if we just use (map number->string now1) directly
		       (now2 (map (lambda (v) (number->string v)) now1)))
		  (write-string remote (string-join-by " " now2) #\newline)
		  (close-port remote)
		  (loop))))
	    (printf "time server %d launched on port %d\n" pid port))
	(fprintf stderr "error! unable to fork server process\n"))))
