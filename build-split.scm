#!/home/uwe/tools/skeem
;;; -*- mode: scheme; -*-

;;; $Id: build-split,v 1.2 2023/05/08 17:50:37 uwe Exp $

;;; usage: <me> [-g] file-to-split output-dir
;;;
;;; look for lines containing split-here directives, split the input
;;; at those directives. the section before the first split-here is
;;; a header which gets attached to each split-file

(define (read-all-lines port)
  (let loop ((acc ()))
    (let ((line (read-line port)))
      (if line
	  (loop (cons line acc))
	  (list-reverse acc)))))

(define do-debug #f)

(when (and (not (null? command-line-arguments))
	   (string=? (car command-line-arguments) "-g"))
  (set! do-debug #t)
  (set! command-line-arguments (cdr command-line-arguments)))

(when (or (null? command-line-arguments)
	  (null? (cdr command-line-arguments)))
  (write-string stderr "usage: " command-name " [-g] file-to-split output-dir\n")
  (exit 0))

(define (take-section cut-pattern lines)
  (let loop ((ls lines)
	     (acc ()))
    (cond ((null? ls)
	   (list (list-reverse acc) ()))
	  ((regex-match cut-pattern (car ls))
	   (list (list-reverse acc) (cdr ls)))
	  (else (loop (cdr ls) (cons (car ls) acc))))))

(define (run-cmd str . strs)
  (let* ((cmd (apply string-append str " " strs))
	 (status (run-command cmd)))
    (unless (zero? status)
      (fprintf stderr "command '%s' failed with status %d\n" cmd status)
      (exit 1)))
  #t)

(unless (is-directory? (cadr command-line-arguments))
  (write-string stderr "output directory doesn't exist or isn't a directory\n")
  (exit 1))

(let* ((file (car command-line-arguments))
       (out-dir (string-append (cadr command-line-arguments) "/"))
       (port (open-file file "r"))
       (data1 (read-all-lines port))
       (dix (string-find-last-char file #\.))
       (prefix1 (if dix (string-copy file 0 dix) file))
       (suffix (if dix (string-copy file dix) ""))
       (cut-pat "(--8><--)+$")
       (dh (take-section cut-pat data1))
       (header (car dh))
       (data2 (cadr dh))
       (count 0))
  (close-port port)
  (when (null? data2)
    (write-string "there is no split directive!\n")
    (exit 1))
  (unless (or (string=? suffix ".scm") (string=? suffix ".c"))
    (write-string "error: don't know how to handle " suffix " files yet!\n")
    (exit 1))
  (let loop ((ls data2)
	     (ix 0))
    (if (null? ls)
	(write-string "all done!\n")
	(let* ((cstr (string-pad-left (number->string ix) #\0 6))
	       (prefix2 (string-append out-dir prefix1 "-"cstr))
	       (ofile (string-append prefix2 suffix))
	       (oport (open-file ofile "w"))
	       (sd (take-section cut-pat ls))
	       (section (car sd)))
	  (write-string "writing " ofile #\newline)
	  (for-each (lambda (l) (write-string oport l #\newline)) header)
	  (for-each (lambda (l) (write-string oport l #\newline)) section)
	  (flush-port oport)
	  (close-port oport)
	  (when (string=? suffix ".scm")
	    (run-cmd "wile -v -c " (if do-debug "-g" "") " "
		     prefix2 ".scm " prefix2 ".c"))
	  (run-cmd "rm -f" out-dir "*.h")
	  (run-cmd "wile -v -o " (if do-debug "-g" "") " "
		   prefix2 ".c " prefix2 ".o")
	  (loop (cadr sd) (+ ix 1))))))
