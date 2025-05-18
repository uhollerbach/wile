;;; Wile -- the extremely stable scheming genius compiler
;;; Copyright 2023 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: LGPLv3 or later, see file 'LICENSE-LGPL' for details

;;; Test macros & functons to make testing more systematic
;;; These tests are less regular because we're exercising some of the
;;; testing machinery itself -- we really need higher-order tests

(load "test.scm")

(test-title "test self-tests")

(define (always-pass e1 e2) #t)
(define (always-fail e1 e2) #f)

(test-mode 'report)
(test 3 (+ 1 2))
(test 5 (+ 1 2))
(test-pred always-fail 3 (+ 1 2))
(test-pred always-pass 5 (+ 1 2))
;;; (test-report "check 1")

(test-mode 'off)
(test-pred always-pass #t (write-string "test 0p"))
(test-pred always-fail #t (write-string "test 0f"))

;;; (test-report "check 2")
(test-mode 'summary)
(test-pred always-pass #t (write-string "test 1p" #\newline))
(test-pred always-fail #t (write-string "test 1f" #\newline))

;;; (test-report "check 3")
(test-mode 'report-failed)
(test-pred always-pass #t (write-string "test 2p" #\newline))
(test-pred always-fail #t (write-string "test 2f" #\newline))

;;; (test-report "check 4")
(test-mode 'report)
(test-pred always-pass #t (write-string "test 3p" #\newline))
(test-pred always-fail #t (write-string "test 3f" #\newline))

(unless (test-expected 10 2 5 0) (exit 1))

;;; (test-report "check 5")
(test-mode 'off)
(test-pred always-pass #t (raise "test 4p"))
(test-pred always-fail #t (raise "test 4f"))

;;; (test-report "check 6")
(test-mode 'summary)
(test-pred always-pass #t (raise "test 5p"))
(test-pred always-fail #t (raise "test 5f"))

;;; (test-report "check 7")
(test-mode 'report-failed)
(test-pred always-fail #t (raise "test 6f"))
(test-pred always-pass "test 6p" (raise "test 6p"))

;;; (test-report "check 8")
(test-mode 'report)
(test-pred always-fail #t (raise "test 7f"))
(test-pred always-pass #t (raise "test 7p"))

;;; interpreter reports undefined at run time...
;;; compiler reports undefined at compile time, and barfs

;;;;	;;; (test-report "check 9")
;;;;	(test-mode 'off)
;;;;	(test #t (this-is-undefined "test 8a"))
;;;;	(test-mode 'summary)
;;;;	(test #t (this-is-undefined "test 8b"))
;;;;	(test-mode 'report-failed)
;;;;	(test #t (this-is-undefined "test 8c"))
;;;;	(test-mode 'report)
;;;;	(test #t (this-is-undefined "test 8d"))

;;;;    (test-group "test test final report" 19 5 5 9)

(test-group "test test final report" 16 4 5 6)
(write-string "all done\n")
