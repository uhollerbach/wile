(define (run-cmd-or-die cmd)
  (write-string "@@@@@ " cmd "\n")
  (unless (zero? (run-command cmd))
    (fprintf stderr "command %s failed!\n" cmd)
    (exit 1)))

(define (rm-file-or-chirp file)
  (write-string "@@@@@ rm " file "\n")
  (when (file-exists? file)
    (unless (remove-file file)
      (fprintf stderr "rm-file %s failed!\n" file))))

(define (rename-file-or-die file1 file2)
  (write-string "@@@@@ mv " file1 " " file2 "\n")
  (unless (rename-file file1 file2)
    (fprintf stderr "rename-file %s -> %s failed!\n" file1 file2)
    (exit 1)))

(define (symlink-or-die file)
  (rm-file-or-chirp "wile")
  (write-string "@@@@@ ln -s " file " wile\n")
  (unless (create-symbolic-link file "wile")
    (fprintf stderr "symlink %s -> wile failed!\n" file)
    (exit 1)))

(define (run-wile args)
  (run-cmd-or-die (string-append "./wile -CF ./wile-config.dat " args)))

(define (make-stage n)
  (let* ((scur (string-append "stage" (number->string n)))
	 (name1-exe (string-append "wilec." scur))
	 (rtl-fileish (list "wile-sql.c" "alloc.c" "print.c" "location.c"
			    "wile-parse.c" "wile-lex.c" "swll-cfft.c"
			    "continuations.c" "fsi_set.c" "nfa.c" "regex.c"
			    "ulexlib.c" "sha256.c" "isocline.c"
			    "-s" "wile-rtl1.c" "wile-rtl2.scm" "math-funcs.c"))
	 (rtl-flist (apply string-join-by " " rtl-fileish))
;;;	 (name2-exe (string-append "wilec-dbg." scur))
	 (name-c (string-append "wilec." scur ".c"))
	 (name1-lib (string-append "libwrtl." scur ".a"))
	 (name2-lib (string-append "libwrtl-dbg." scur ".a"))
	 (name3-lib (string-append "libwrtl-pg." scur ".a")))
    (write-string "######## make " scur "\n")
    (run-cmd-or-die "make realclean")
    (run-wile "-c wile-rtl2.scm wile-rtl2.c")
    (run-cmd-or-die "rm -rf bld-rtl-dir")
    (run-cmd-or-die (string-join-by " " "build-rtl libwrtl.a" rtl-flist))
    (run-cmd-or-die "nm -a libwrtl.a | grep wile_config")

    (run-cmd-or-die "rm -rf bld-rtl-dir")
    (run-cmd-or-die (string-join-by
		     " " "build-rtl -g libwrtl-dbg.a" rtl-flist))
    (run-cmd-or-die "nm -a libwrtl-dbg.a | grep wile_config")

    (run-cmd-or-die "rm -rf bld-rtl-dir")
    (run-cmd-or-die (string-join-by
		     " " "build-rtl -p libwrtl-pg.a" rtl-flist))
    (run-cmd-or-die "nm -a libwrtl-pg.a | grep wile_config")

    (run-wile "-c wile-main.scm wilec.c")
    (run-wile "-x wilec.c wilec")
;;;    (run-wile "-x -g wilec.c wilec-dbg")
    (rm-file-or-chirp "wile")
    (rm-file-or-chirp name1-exe)
    (rm-file-or-chirp name-c)
    (rename-file-or-die "wilec.c" name-c)
    (rename-file-or-die "wilec" name1-exe)
;;;    (rename-file-or-die "wilec-dbg" name2-exe)
    (rename-file-or-die "libwrtl.a" name1-lib)
    (rename-file-or-die "libwrtl-dbg.a" name2-lib)
    (rename-file-or-die "libwrtl-pg.a" name3-lib)
    (symlink-or-die name1-exe)))

(define (get-hash file)
  (let* ((port (open-file file "rb"))
	 (hash (sha-256 port)))
    (close-port port)
    hash))

(define (print-hash templates ids)
  (let ((sids (map (lambda (i) (number->string i)) ids)))
    (for-each (lambda (tmpl)
		(let* ((vs (regex-match "%" tmpl))
		       (pre (car vs))
		       (suf (caddr vs))
		       (fname (apply string-append
				     (flatten (list pre "[" sids "]" suf)))))
		  (printf "%s\n" fname)
		  (for-each (lambda (s)
			      (printf "  %s %s\n" s
				      (get-hash (string-append pre s suf))))
			    sids)))
	      templates)))

(define m1x #f)
(define m1c #f)
(define m1l #f)
(define m2x #f)
(define m2c #f)
(define m2l #f)

(define files (list "wilec.stage%.c" "wilec.stage%"
;;;		    "wilec-dbg.stage%"
		    "libwrtl.stage%.a" "libwrtl-dbg.stage%.a"
		    "libwrtl-pg.stage%.a"))

(write-string "######## setup\n")

(when (or (file-exists? "wilec") (file-exists? "wilec.c"))
  (raise "build-stages: wilec and/or wilec.c exist!"))
(when (and (not (null? command-line-arguments))
	   (file-executable? (car command-line-arguments)))
  (symlink-or-die (car command-line-arguments)))

(make-stage 1)
(set! m1x (get-hash "wilec.stage1"))
(set! m1c (get-hash "wilec.stage1.c"))
(set! m1l (get-hash "libwrtl.stage1.a"))
(make-stage 2)
(set! m2x (get-hash "wilec.stage2"))
(set! m2c (get-hash "wilec.stage2.c"))
(set! m2l (get-hash "libwrtl.stage2.a"))

(if (and (string=? m1x m2x)
	 (string=? m1c m2c)
	 (string=? m1l m2l))
    (begin
      (write-string "######## early comparison succeeded\n")
      (print-hash files '(1 2))
      (create-symbolic-link "libwrtl.stage2.a" "libwrtl.a")
      (create-symbolic-link "libwrtl-dbg.stage2.a" "libwrtl-dbg.a")
      (create-symbolic-link "libwrtl-pg.stage2.a" "libwrtl-pg.a")
      (write-string "## clean up redundant stage1\n")
      (rm-file-or-chirp "wilec.stage1.c")
      (rm-file-or-chirp "wilec.stage1")
;;;      (rm-file-or-chirp "wilec-dbg.stage1")
      (rm-file-or-chirp "libwrtl.stage1.a")
      (rm-file-or-chirp "libwrtl-dbg.stage1.a")
      (rm-file-or-chirp "libwrtl-pg.stage1.a"))
    (begin
      (make-stage 3)
      (write-string "######## compare\n")
      (print-hash files '(1 2 3))
      (create-symbolic-link "libwrtl.stage3.a" "libwrtl.a")
      (create-symbolic-link "libwrtl-dbg.stage3.a" "libwrtl-dbg.a")
      (create-symbolic-link "libwrtl-pg.stage3.a" "libwrtl-pg.a")))
