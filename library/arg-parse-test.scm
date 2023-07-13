;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; TODO: at some point, test failures as well:
;;; pass in bad command line, catch & examine exception

(load "struct.scm")
(load "hash.scm")
(load "arg-parse.scm")
(load "test.scm")

(test-title "arg-parse tests")

(test-mode 'report-failed)

(define opt-def
  (list '("-s1d" string "1 string with default" "s1def")
	'("-s1n" string "1 string no default")
	'("-s2" strings "multiple strings")
	'("-i1d" int "1 int with default" 177)
	'("-i1n" int "1 int no default")
	'("-i2" ints "multiple ints")
	;;; note computed/evaluated default value here;
	;;; need to not use '() notation
	(list "-r1d" 'real "1 real with default" (* 0.5 7))
	'("-r1n" real "1 real no default")
	'("-r2" reals "multiple reals")
	'("-b1d" bool "1 bool with default" #t)
	'("-b1n" bool "1 bool no default")
	'("-b2" bools "multiple bools")
	'("-f1" flag "1 flag")
	'("-f2" flags "multiple flags")))

(define (canonic-hash)
  (list-sort (lambda (a b) (string<? (car a) (car b)))
	     (hash-table-entries (apply parse-command-line-flags opt-def))))

(set! command-line-arguments (list "foo" "bar" "-i1d" "14" "zotz"))

(test (list '("-b1d" . #t)
	    '("-i1d" . 14)
	    '("-r1d" . 3.5)
	    '("-s1d" . "s1def"))
      (canonic-hash))

(test '("foo" "bar" "zotz") command-line-arguments)

(set! command-line-arguments (list "foo" "bar" "--" "-i1d" "14" "zotz"))

(test (list '("-b1d" . #t)
	    '("-i1d" . 177)
	    '("-r1d" . 3.5)
	    '("-s1d" . "s1def"))
      (canonic-hash))

(test '("foo" "bar" "-i1d" "14" "zotz") command-line-arguments)

(set! command-line-arguments
      (list "foo" "-i2" "3" "bar" "-i1d" "14" "-i2" "2"
	    "-r1d" "11" "zotz" "-i2" "1"))

(test (list '("-b1d" . #t)
	    '("-i1d" . 14)
	    '("-i2" . (3 2 1))
	    '("-r1d" . 11)
	    '("-s1d" . "s1def"))
      (canonic-hash))

(test '("foo" "bar" "zotz") command-line-arguments)

(set! command-line-arguments
      (list "foo" "-i2" "3" "bar" "-i1d" "14" "-f1" "-i2" "2" "-f2"
	    "-r1d" "11" "zotz" "-i2" "1" "-f2" "-b1n" "no"))

(test (list '("-b1d" . #t)
	    '("-b1n" . #f)
	    '("-f1" . #t)
	    '("-f2" . 2)
	    '("-i1d" . 14)
	    '("-i2" . (3 2 1))
	    '("-r1d" . 11)
	    '("-s1d" . "s1def"))
      (canonic-hash))

(test '("foo" "bar" "zotz") command-line-arguments)



(test-report "arg-parse test final result")

(unless (test-expected 8 0 0 0) (exit 1))
