;;; make a lower-case directory
;;; check errno
;;; change name to uppercase
;;; chdir to that
;;; check errno

(let* ((name1 "lowercase-name")
       (name2 (string-upcase name1))
       (res1 (create-directory name1)))
  (printf "create-directory '%s' returned %v\nerrno is %s\n"
	  name1 res1 (describe-system-error (get-errno)))
  (let ((res2 (set-current-directory name2)))
    (printf "now attempting to chdir to '%s' ... %v\n" name2 res2)
    (if res2
	(begin
	  (printf "it appears this filesystem is case-INsensitive\n")
	  (set-current-directory "..")
	  (let ((res3 (remove-directory name2)))
	    (printf "rmdir '%s' reports %v\n" name2 res3)))
	(begin
	  (printf "it appears this filesystem is case-sensitive\n")
	  (let ((res3 (remove-directory name1)))
	    (printf "rmdir '%s' reports %v\n" name1 res3))))))
