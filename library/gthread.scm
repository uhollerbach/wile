;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; call (thread-spawn <thunk>) any number of times;
;;; each <thunk> does its thing and calls (thread-yield)
;;; whenever it's ready to pass control to the next guy.
;;; run (thread-run) when all the threads are set up.

;;; call (thread-mbox-setup) to create mailboxes, Maybe FIFOs
;;; which are manipulated by (thread-mbox-read) (yield until
;;; there's something in the mbox, pop the first message off the
;;; FIFO & return it) and (thread-mbox-write) (append a message
;;; to the mbox)

;;; see gthread-test.scm for an example

(define *thread-list* '())

(define (thread-schedule)
  (unless (null? *thread-list*)
	  (let ((next-job (car *thread-list*)))
	    (set! *thread-list* (cdr *thread-list*))
	    (next-job '()))))

(define (thread-spawn job)
  (set! *thread-list*
	(append *thread-list*
		(cons (lambda (_) (job) (thread-schedule)) ()))))

(define (thread-yield)
  (call/cc
   (lambda (c)
     (set! *thread-list* (append *thread-list* (cons c ())))
     (thread-schedule))))

(define (thread-run)
  (while (positive? (list-length *thread-list*))
	 (thread-yield)))

;;; a mailbox is a Maybe list: Just list | Nothing, like in monad-maybe.scm
;;; TODO need module system!

(define (maybe-yea val) (cons 'Just val))
(define maybe-nay 'Nothing)

(define (maybe-yea? mv)
  (and (pair? mv) (eqv? 'Just (car mv))))

(defmacro (thread-mbox-setup mbox . ival)
  (if (null? ival)
      `(define ,mbox maybe-nay)
      `(define ,mbox (maybe-yea (list ,(car ival))))))

(defmacro (thread-mbox-read mbox)
  `(begin (until (maybe-yea? ,mbox)
		 (thread-yield))
	  (let ((m (cadr ,mbox))
		(ms (cddr ,mbox)))
	    (if (null? ms)
		(set! ,mbox maybe-nay)
		(set! ,mbox (maybe-yea ms)))
	    m)))

(defmacro (thread-mbox-write mbox val)
  `(if (maybe-yea? ,mbox)
       (set! ,mbox (maybe-yea (append (cdr ,mbox) (list ,val))))
       (set! ,mbox (maybe-yea (list ,val)))))
