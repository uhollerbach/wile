(define (sha-t1 show msg cmp)
  (let ((tst (sha-256 msg))
	(fp (open-file "sha-test.txt" "wb+"))
	(shad (sha-256-init)))
    (write-string "\"" (if show msg "[big]") "\"\n"
		  "<string> -> "
		  (if (string=? tst cmp) "pass" "FAIL") #\newline)
    (write-string fp msg)
    (set-file-position fp 0)
    (set! tst (sha-256 fp))
    (close-port fp)
    (write-string "<file> ->   "
		  (if (string=? tst cmp) "pass" "FAIL") #\newline)
    (remove-file "sha-test.txt")
    (sha-256-update shad msg)
    (set! tst (sha-256-finish shad))
    (write-string "<bitsy> ->  "
		  (if (string=? tst cmp) "pass" "FAIL") #\newline)))

(define (sha-t2 str rep cmp)
  (let ((shad (sha-256-init)))
    (do ((i 0 (i+ i 1)))
	((>= i rep) #t)
      (sha-256-update shad str))
    (let ((tst (sha-256-finish shad)))
      (printf "%s x %d -> %s\n"
	      str rep (if (string=? tst cmp) "pass" "FAIL")))))

(define (sha-t3 show msg cmp)
  (let ((tst (sha-224 msg))
	(fp (open-file "sha-test.txt" "wb+"))
	(shad (sha-224-init)))
    (write-string "\"" (if show msg "[big]") "\"\n"
		  "<string> -> "
		  (if (string=? tst cmp) "pass" "FAIL") #\newline)
    (write-string fp msg)
    (set-file-position fp 0)
    (set! tst (sha-224 fp))
    (close-port fp)
    (write-string "<file> ->   "
		  (if (string=? tst cmp) "pass" "FAIL") #\newline)
    (remove-file "sha-test.txt")
    (sha-224-update shad msg)
    (set! tst (sha-224-finish shad))
    (write-string "<bitsy> ->  "
		  (if (string=? tst cmp) "pass" "FAIL") #\newline)))

(define (sha-t4 str rep cmp)
  (let ((shad (sha-224-init)))
    (do ((i 0 (i+ i 1)))
	((>= i rep) #t)
      (sha-224-update shad str))
    (let ((tst (sha-224-finish shad)))
      (printf "%s x %d -> %s\n"
	      str rep (if (string=? tst cmp) "pass" "FAIL")))))

(sha-t1 #t ""
	"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(sha-t1 #t "abc"
	"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

(sha-t1 #t "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
	"248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")

(sha-t1 #t "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
	"cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1")

(sha-t1 #f (string-create 1000000 #\a)
	"cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")

(sha-t2 "a" 1000000
	"cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")

(sha-t2 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"
	16777216
	"50e72a0e26442fe2552dc3938ac58658228c0cbfb1d2ca872ae435266fcd055e")

(sha-t3 #t ""
	"d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f")

(sha-t3 #t "abc"
	"23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7")

(sha-t3 #t "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
	"75388b16512776cc5dba5da1fd890150b0c6455cb4f58b1952522525")

(sha-t3 #t "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
	"c97ca9a559850ce97a04a96def6d99a9e0e0e2ab14e6b8df265fc0b3")

(sha-t3 #f (string-create 1000000 #\a)
	"20794655980c91d8bbb4c1ea97618a4bf03f42581948b2ee4ee7ad67")

(sha-t4 "a" 1000000
	"20794655980c91d8bbb4c1ea97618a4bf03f42581948b2ee4ee7ad67")

(sha-t4 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"
	16777216
	"b5989713ca4fe47a009f8621980b34e6d63ed3063b2a0a2c867d8a85")
