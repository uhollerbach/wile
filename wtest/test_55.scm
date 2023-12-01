(define (sha-test show msg cmp)
  (let ((tst (sha-256 msg))
	(fp (open-file "sha-test.txt" "wb+")))
    (write-string "\"" (if show msg "[big]") "\"\n"
		  "<string> -> "
		  (if (string=? tst cmp) "pass" "FAIL") #\newline)
    (write-string fp msg)
    (set-file-position fp 0)
    (set! tst (sha-256 fp))
    (close-port fp)
    (write-string "<file> ->   "
		  (if (string=? tst cmp) "pass" "FAIL") #\newline)
    (remove-file "sha-test.txt")))

(sha-test #t ""
	  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(sha-test #t "abc"
	  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

(sha-test #t "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
	  "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")

(sha-test #t "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
	  "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1")

(sha-test #f (string-create 1000000 #\a)
	  "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")

;;; (let ((str "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"))
;;;   (do ((i 0 (+ i 1)))
;;;       ((= i 8) #t)
;;;     (printf "%d\n" i)
;;;     (set! str (string-append str str str str str str str str)))
;;;   (sha-test #f str
;;; 	    "50e72a0e26442fe2552dc3938ac58658228c0cbfb1d2ca872ae435266fcd055e"))
