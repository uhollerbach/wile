;;; update an existing install from the optimized build, not autotools

(define (dn v p)
  (display v p)
  (newline p))

(define (install-to may-skip dirs . files)
  (printf "\ninstall files %v\n" files)
  (let ((done #f))
    (for-each (lambda (d)
		(unless (string=? d ".")
		  (printf "install to %s? [Y/n] " d)
		  (flush-port stdout)
		  (let ((line (read-line stdin)))
		    (when (and line (or (eqv? line "")
					(regex-match "^[yY]" line)))
		      (let* ((ll (list-flatten (list "/bin/cp" files d)))
			     (cmd (apply string-join-by " " ll)))
			(run-command cmd)
			(set! done #t))))))
	      dirs)
    (if (or done may-skip)
	done
	(begin
	  (write-string
	   "################\nno target selected! retry\n################\n")
	  (apply install-to may-skip dirs files)))))

(when (and (not (null? command-line-arguments))
	   (or (string=? (car command-line-arguments) "-h")
	       (string=? (car command-line-arguments) "-help")))
  (write-string "usage: " command-name " [-skip-build]\n")
  (exit 0))

(let* ((port1 (run-read-command "wile -V"))
       (wile (let* ((p0 (run-read-command "which wile"))
		    (d0 (read-line p0)))
	       (close-port p0)
	       (list d0)))
       (data1 (read-all port1))
       (_1 (close-port port1))
       (config-file (assv 'wile-config-file data1))
       (skip (and (not (null? command-line-arguments))
		  (string=? (car command-line-arguments) "-skip-build"))))
  (unless config-file
    (write-string
     stderr "error! 'wile -V' did not produce a good configuration\n")
    (for-each (lambda (v) (dn v stderr)) data1)
    (exit 1))
  (set! config-file (cadr config-file))
  (when (symbol? config-file)
    (set! config-file (symbol->string config-file)))
  (let ((data2 (read-all config-file))
	(port2 (open-file "local-config.dat" "w")))
    (dn (assv 'c-compiler data2) port2)
    (let* ((tag 'c-compiler-flags)
	   (val (assv tag data2)))
      (fprintf port2 "(%s \"%s -DWILE_CONFIG_FILE=%s\")\n"
	       tag (cadr val) config-file))
    (let* ((tag 'c-include-directories)
	   (val (assv tag data2)))
      (fprintf port2 "(%s %v)\n" tag (cons "." (cadr val))))
    (let* ((tag 'c-link-directories)
	   (val (assv tag data2)))
      (fprintf port2 "(%s %v)\n" tag (cons "." (cadr val))))
    (let* ((tag 'scheme-include-directories)
	   (val (assv tag data2)))
      (fprintf port2 "(%s %v)\n" tag (cons "./library" (cadr val))))
    (dn (assv 'c-link-libraries data2) port2)
    (dn (assv 'wile-config data2) port2)
    (close-port port2)
    (set-environment-variable "WILE_CONFIG_FILE" "./local-config.dat")
    (unless skip
      (run-command "build-stages"))
    (run-command "/bin/ls -lFtr *.stage[123]*")
    (write-string "proceed with install? [Y/n] ")
    (flush-port stdout)
    (let ((line (read-line stdin)))
      (unless (and line (or (eqv? line "") (regex-match "^[yY]" line)))
	(write-string "ok, stopping now\n")
	(exit 0)))
    (install-to #f (cadr (assv 'c-link-directories data2))
		"libwrtl.a" "libwrtl-dbg.a" "wrtl.sch")
    (install-to #f (cadr (assv 'c-include-directories data2))
		"array_builder.h" "config.h" "sha256.h" "ulexlib.h"
		"wile-lex.h" "wile-parse.h" "wile-rtl1.h" "wile-rtl2.h")

    (when (and (file-exists? "wilec.stage3")
	       (install-to #t wile "wilec.stage3"))
      (exit 0))
    (when (and (file-exists? "wilec.stage2")
	       (install-to #t wile "wilec.stage2"))
      (exit 0))
    (when (and (file-exists? "wilec.stage1")
	       (install-to #t wile "wilec.stage1"))
      (exit 0))
    (write-string stderr "wile was *not* installed!\n")))
