;;; Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: GPLv3 or later, see file 'LICENSE' for details
;;;
;;; Demo program for wile: a client to go with the server
;;;
;;; Compile and run it:
;;;     wile -x net-client.scm timc
;;;     timc [--set] [host [port]]

(let ((host "localhost")
      (port 2500)
      (set #f))
  (when (and (not (null? command-line-arguments))
	     (or (string=? (car command-line-arguments) "-s")
		 (string=? (car command-line-arguments) "--set")))
    (set! set #t)
    (set! command-line-arguments (cdr command-line-arguments)))
  (unless (null? command-line-arguments)
    (set! host (car command-line-arguments))
    (set! command-line-arguments (cdr command-line-arguments)))
  (unless (null? command-line-arguments)
    (set! port (string->number (car command-line-arguments)))
    (set! command-line-arguments (cdr command-line-arguments)))
  (let ((remote (connect-to host port)))
    (if remote
	(let* ((rtime (read-line remote))
	       (tval1 (string-split-by char-whitespace? rtime))
	       (tval2 (map (lambda (v) (string-pad-left v #\0 2)) tval1)))
	  (close-port remote)
	  (if set
	      (let ((cmd (sprintf "/usr/bin/date %s%s%s%s%s.%s"
				  (list-ref tval2 1) (list-ref tval2 2)
				  (list-ref tval2 3) (list-ref tval2 4)
				  (list-ref tval2 0) (list-ref tval2 5))))
		(write-string cmd #\newline)
;;;	  (run-command cmd)
;;;	  (run-command "/usr/sbin/hwclock -w")
		)
	      (apply printf "%s time is\t%s-%s-%s %s:%s:%s\n" host tval2)))
	(fprintf stderr "error! unable to contact server %s:%d\n" host port))))
