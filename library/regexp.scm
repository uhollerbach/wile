;;; Regular expressions: AST -> NFA -> DFA (-> C or to graph)

;;; AST operators:
;;; (... ...)		concatenation - ...1 followed by ...2 etc
;;; (^ ...)		OR - alternative 1 or alternative 2 or ...
;;; (? ...)		optional - ... is allowed 0 or 1 times
;;; (* ...)		Kleene star - ... is allowed 0 or more times
;;; (+ ...)		Kleene plus - ... is allowed 1 or more times
;;; (# N ...)		count - ... is required to be present exactly N times
;;; (## N1 N2 ...)	count range - ... is required to be present N times,
;;;			where N is in the range [N1, N2]
;;; (: <class> ...)	character classes: <class> can be any one of
;;;			alnum alpha blank cntrl digit graph lower print
;;;			punct space upper xdigit ascii word odigit
;;;			if any ... is present, those chars get subtracted
;;;			from the char class

;;; TODO if I manually edit the generated C file to remove all type checks
;;; and strlen() calls out of string-ref and string-set!, the runtime for
;;; mini-lex-gen drops from 7:39 to 5:47... that's not trivial.
;;;
;;; Can we get that from scheme code? change strings to bytevectors?
;;; Also removing checks for vector-ref and vector-set! gets another 6-7
;;; seconds... removing checks in inner loop of cxr gets very little.
;;;
;;; ==> big win is removing strlen ==> how?

(load-library "hash.scm")

;;; not sure yet what this will be for... try to find a generic member of
;;; a character class, run dfa-generation only for that member, and then
;;; expand the dfa to the whole class at the end?

(define (ast-entities ast)
  (letrec* ((acc ())
	    (tree-walk
	     (lambda (ast)
	       (cond ((null? ast)
		      #t)
		     ((or (char? ast)
			  (symbol? ast)
			  (integer? ast))
		      (set! acc (cons ast acc)))
		     ((string? ast)
		      (tree-walk (string->list ast)))
		     ((list? ast)
		      (let ((first (car ast))
			    (rest (cdr ast)))
			(cond ((or (eqv? first '^)
				   (eqv? first '?)
				   (eqv? first '*)
				   (eqv? first '+))
			       (tree-walk rest))
			      ((eqv? first '#)
			       (tree-walk (cdr rest)))
			      ((eqv? first '##)
			       (tree-walk (cddr rest)))
			      ((eqv? first ':)
			       (set! acc (cons (list ': (car rest)) acc))
			       (set! acc (list-append (cdr rest) acc)))
			      (else (for-each tree-walk ast)))))
		     (else (raise (list "bad input!" ast))))))
	    (ecmp (lambda (e1 e2)
		    (cond ((and (char? e1) (char? e2))
			   (char<? e1 e2))
			  ((char? e1) #t)
			  ((char? e2) #f)
			  (else (let ((s1 (symbol->string (cadr e1)))
				      (s2 (symbol->string (cadr e2))))
				  (string<? s1 s2)))))))
    (tree-walk ast)
    (map car (list-group-by eqv? (list-sort ecmp acc)))))

;;; Convert an AST in the format above into an NFA via Thompson construction

(define (ast-to-nfa ast)
  (letrec*
      ((state-id 1)
       (mk-state
	(lambda (eps-ids input next-id)
	  (set! state-id (i+ state-id 1))
	  (if next-id
	      (list state-id eps-ids (list next-id input))
	      (list state-id eps-ids))))
       (add-eps
	(lambda (state id)
	  (cons (car state)
		(cons (cons id (cadr state))
		      (cddr state)))))
       (concat-nfas
	(lambda (nfa1 nfa2)
	  (let ((s2 (car nfa2)))
	    (cons (car nfa1)
		  (cons (cadr nfa2)
			(cons (add-eps (cadr nfa1) (car s2))
			      (cons s2 (list-append (cddr nfa1)
						    (cddr nfa2)))))))))
       (filter-class
	(lambda (class-cs rem-cs)
	  (cons '^ (filter (lambda (c) (not (memv c rem-cs))) class-cs))))
       (awork
	(lambda (ast)
	  (cond ((null? ast)
		 (let* ((end (mk-state () #f #f))
			(start (mk-state
				(list (car end)) #f #f)))
		   (list start end)))
		((or (char? ast)
		     (symbol? ast)
		     (integer? ast))
		 (let* ((end (mk-state () #f #f))
			(start (mk-state () ast (car end))))
		   (list start end)))
		((string? ast)
		 (awork (string->list ast)))
		((list? ast)
		 (let ((first (car ast))
		       (rest (cdr ast)))
		   (cond ((eqv? first '^)
			  (if (list-length<=? 1 rest)
			      (awork rest)
			      (let* ((end (mk-state () #f #f))
				     (eid (car end))
				     (n1 (map awork rest))
				     (n2 (map (lambda (n)
						(cons (car n)
						      (cons
						       (add-eps (cadr n) eid)
						       (cddr n))))
					      n1)))
				(cons (mk-state (map caar n2) #f #f)
				      (cons end (apply list-append n2))))))
			 ((or (eqv? first '?)
			      (eqv? first '*)
			      (eqv? first '+))
			  (let ((n1 (awork rest)))
			    (if (null? rest)
				n1
				(cond ((eqv? first '?)
				       (cons (add-eps
					      (car n1) (caadr n1))
					     (cdr n1)))
				      ((eqv? first '+)
				       (cons (car n1)
					     (cons (add-eps
						    (cadr n1) (caar n1))
						   (cddr n1))))
				      (else
				       (cons (add-eps
					      (car n1) (caadr n1))
					     (cons (add-eps
						    (cadr n1) (caar n1))
						   (cddr n1))))))))
			 ((eqv? first '#)
			  (if (and (list-length>=? 2 rest)
				   (integer? (car rest))
				   (not (negative? (car rest))))
			      (let loop ((nfa (cdr rest))
					 (count (car rest))
					 (acc ()))
				(if (positive? count)
				    (loop nfa (i- count 1)
					  (cons (awork nfa) acc))
				    (if (null? acc)
					(awork ())
					(foldl1 concat-nfas acc))))
			      (raise (list "bad count regexp!" ast))))
			 ((eqv? first '##)
			  (if (and (list-length>=? 3 rest)
				   (integer? (car rest))
				   (not (negative? (car rest)))
				   (integer? (cadr rest))
				   (not (negative? (cadr rest))))
			      (let* ((lo (min (car rest) (cadr rest)))
				     (hi (max (car rest) (cadr rest)))
				     (op (i- hi lo))
				     (nfa1 (list '# lo (cddr rest)))
				     (nfa2 (list '# op (list '? (cddr rest)))))
				(cond ((and (zero? lo) (zero? hi))
				       (awork ()))
				      ((zero? lo)
				       (awork nfa2))
				      ((zero? hi)
				       (awork nfa1))
				      (else (awork (list nfa1 nfa2)))))
			      (raise (list "bad count-range regexp!" ast))))
			 ((eqv? first ':)
			  (if (and (list-length>=? 1 rest)
				   (symbol? (car rest)))
			      (let* ((digit '(#\0 #\1 #\2 #\3 #\4
					      #\5 #\6 #\7 #\8 #\9))
				     (lc '(#\a #\b #\c #\d #\e #\f #\g
					   #\h #\i #\j #\k #\l #\m #\n
					   #\o #\p #\q #\r #\s #\t #\u
					   #\v #\w #\x #\y #\z))
				     (UC '(#\A #\B #\C #\D #\E #\F #\G
					   #\H #\I #\J #\K #\L #\M #\N
					   #\O #\P #\Q #\R #\S #\T #\U
					   #\V #\W #\X #\Y #\Z))
				     (alnum (list-append lc UC digit))
				     (rrest (cdr rest))
				     (awfc (lambda (cl)
					     (awork (filter-class cl rrest)))))
				(cond
				 ((symbol=? (car rest) 'alnum)
				  (awfc alnum))
				 ((symbol=? (car rest) 'alpha)
				  (awfc (list-append lc UC)))
				 ((symbol=? (car rest) 'blank)
				  (awfc '(#\space #\tab)))
				 ((symbol=? (car rest) 'cntrl)
				  (awfc (map integer->char
					     (cons 127 (fromto 0 31)))))
				 ((symbol=? (car rest) 'digit)
				  (awfc digit))
				 ((symbol=? (car rest) 'graph)
				  (awfc (map integer->char (fromto 33 126))))
				 ((symbol=? (car rest) 'lower)
				  (awfc lc))
				 ((symbol=? (car rest) 'print)
				  (awfc (map integer->char (fromto 32 126))))
				 ((symbol=? (car rest) 'punct)
				  (awfc '(#\! #\" #\# #\$ #\% #\& #\' #\(
					  #\) #\* #\+ #\, #\- #\. #\/ #\:
					  #\; #\< #\= #\> #\? #\@ #\[ #\\
					  #\] #\^ #\_ #\` #\{ #\| #\} #\~)))
				 ((symbol=? (car rest) 'space)
				  (awfc '(#\space #\tab #\newline
					  #\return #\page #\vtab)))
				 ((symbol=? (car rest) 'upper)
				  (awfc UC))
				 ((symbol=? (car rest) 'xdigit)
				  (awfc
				   (list-append digit
						'(#\A #\B #\C #\D #\E #\F
						  #\a #\b #\c #\d #\e #\f))))
				 ((symbol=? (car rest) 'all)
				  (awfc (map integer->char (fromto 0 255))))
				 ((symbol=? (car rest) 'ascii)
				  (awfc (map integer->char (fromto 0 127))))
				 ((symbol=? (car rest) 'word)
				  (awfc (cons #\_ alnum)))
				 ((symbol=? (car rest) 'odigit)
				  (awfc '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))
				 ((symbol=? (car rest) 'sign)
				  (awfc '(#\+ #\-)))
				 (else (raise
					(list "bad char-class!" ast)))))
			      (raise
			       (list "bad char-class regexp!" rest))))
			 (else (foldl1 concat-nfas
				       (map awork ast))))))
		(else (raise (list "bad input!" ast)))))))
    (let ((n1 (awork ast)))
      (cons (caar n1)
	    (cons (caadr n1)
		  (list-sort (lambda (v1 v2) (< (car v1) (car v2))) n1))))))

;;; Return a list of all the inputs that the NFA responds to

(define (nfa-alphabet nfa)
  (let* ((type (lambda (val)
		 (cond ((char? val) 1)
		       ((string? val) 2)
		       ((symbol? val) 3)
		       ((integer? val) 4)
		       (else (raise (sprintf "bad input type! %v" val))))))
	 (cmp (lambda (v1 v2)
		(let ((t1 (type v1))
		      (t2 (type v2)))
		  (if (= t1 t2)
		      (case t1
			((1) (char<? v1 v2))
			((2) (string<? v1 v2))
			((3) (string<? (symbol->string v1)
				       (symbol->string v2)))
			((4) (< v1 v2)))
		      (< t1 t2))))))
    (let loop ((states (cddr nfa))
	       (acc ()))
      (if (null? states)
	  (map car (list-group-by eqv? (list-sort cmp acc)))
	  (let loop2 ((trs (cddar states))
		      (acc2 ()))
	    (if (null? trs)
		(loop (cdr states) (list-append acc2 acc))
		(loop2 (cdr trs) (list-append (cdar trs) acc2))))))))

;;; Take a list of NFAs which all have just one end state, the usual output
;;; of the Thompson construction, and convert them into a multi-NFA that
;;; has multiple distinct end states, one for each of the input NFAs

(define (nfa-merge nfas)
  (cond ((null? nfas)
	 (ast-to-nfa ()))
	((null? (cdr nfas))
	 (car nfas))
	(else
	 (letrec*
	     ((nr1 (lambda (nfa incr)
		     (cons (i+ (car nfa) incr)
			   (cons (i+ (cadr nfa) incr)
				 (map (lambda (s)
					(cons (i+ (car s) incr)
					      (cons (map (lambda (et)
							   (i+ et incr))
							 (cadr s))
						    (map (lambda (ts)
							   (cons
							    (i+ (car ts) incr)
							    (cdr ts)))
							 (cddr s)))))
				      (cddr nfa))))))
	      (nr2 (lambda (ns offset)
		     (if (null? ns)
			 ()
			 (let* ((n1 (car ns))
				(n1c (i- (list-length n1) 2)))
			   (cons (nr1 n1 offset)
				 (nr2 (cdr ns) (i+ offset n1c)))))))
	      (mns (nr2 nfas 1)))
	   (cons 2 (cons (map cadr mns)
			 (cons (list 2 (map car mns))
			       (apply list-append (map cddr mns)))))))))

;;; Convert an NFA into an equivalent DFA

(defmacro (string-set-unsafe! s i v)
  (let ((s1 (gensym))
	(i1 (gensym))
	(v1 (gensym)))
    `(let ((,s1 ,s)
	   (,i1 ,i)
	   (,v1 ,v))
       (wile-c "@1.v.str[@2.v.iv] = @3.v.chr;\n" ,s1 ,i1 ,v1)
       #t)))

(defmacro (string-ref-unsafe s i)
  (let ((s1 (gensym))
	(i1 (gensym))
	(r1 (gensym)))
    `(let ((,s1 ,s)
	   (,i1 ,i)
	   (,r1 #f))
       (wile-c "@1 = LVI_CHAR(@2.v.str[@3.v.iv]);\n" ,r1 ,s1 ,i1)
       ,r1)))

;;; (defmacro (vector-set-unsafe s i v)
;;;   (let ((s1 (gensym))
;;; 	(i1 (gensym))
;;; 	(v1 (gensym)))
;;;     `(let ((,s1 ,s)
;;; 	   (,i1 ,i)
;;; 	   (,v1 ,v))
;;;        (wile-c
;;; 	#<< HERE_DOC
;;; 	>@1.v.vec.arr[@2.v.iv] = new_lv(LV_NIL);
;;; 	>*(@1.v.vec.arr[@2.v.iv]) = @3;
;;; 	>HERE_DOC
;;; 	,s1 ,i1 ,v1)
;;;        #t)))

(define (nfa-to-dfa nfa)
  (let* ((alphabet (nfa-alphabet nfa))
	 (nfa (list->vector nfa))
	 (vl (vector-length nfa))
	 (eps-cache (vector-create vl))
	 (nfacc1 (vector-ref nfa 1))
	 (nfacc2 (if (integer? nfacc1) (list nfacc1) nfacc1))
	 (nfacc (map cons nfacc2 (fromto 1 (list-length nfacc2))))
	 (dfa-states (hash-table-create string-hash-64 string=?))
	 (next-id 0)
	 (work-list ())
	 (trans ())
	 (accept-states ())
	 (eps-closure-org
	  (lambda (nfa svec)
	    (let ((work-list ()))
	      (do ((i 2 (i+ i 1)))
		  ((>= i vl) #t)
		(unless (char=? #\0 (string-ref-unsafe svec i))
		  (set! work-list (cons i work-list))
		  (string-set-unsafe! svec i #\0)))
	      (until (null? work-list)
		     (let ((cur (car work-list)))
		       (set! work-list (cdr work-list))
		       (when (char=? #\0 (string-ref-unsafe svec cur))
			 (string-set-unsafe! svec cur #\1)
			 (set! work-list
			       (list-append (cadr (vector-ref nfa cur))
					    work-list))))))))
	 (eps-closure-cache
	  (lambda (cache svec)
	    (let ((evec (string-create vl #\0)))
	      (do ((i 2 (i+ i 1)))
		  ((>= i vl) evec)
		(when (char=? #\1 (string-ref-unsafe svec i))
		  (string-or! evec (vector-ref cache i)))))))
	 (find-dfa-state
	  (lambda (dstates cur)
	    (let loop ((ds dstates))
	      (if (null? ds)
		  #f
		  (if (string=? (cdar ds) cur)
		      (caar ds)
		      (loop (cdr ds)))))))
	 (is-nak?
	  (lambda (s)
	    (all-true?
	     (map (lambda (i) (char=? #\0 (string-ref-unsafe (cdr s) (car i))))
		  nfacc))))
	 (map-accept-states
	  (lambda (dfas dacc)
	    (let loop1 ((ds dfas)
			(as dacc)
			(acc ()))
	      (cond ((or (null? ds) (null? as))
		     (map (lambda (v1) (map (lambda (v2) (car v2)) v1))
			  (list-group-by
			   (lambda (v1 v2) (= (cdr v1) (cdr v2)))
			   (list-sort
			    (lambda (v1 v2) (< (cdr v1) (cdr v2))) acc))))
		    ((= (caar ds) (car as))
		     (loop1
		      (cdr ds) (cdr as)
		      (cons
		       (cons
			(caar ds)
			(let loop2 ((sbits (cdar ds))
				    (ns nfacc))
			  (if (char=? #\0 (string-ref-unsafe sbits (caar ns)))
			      (loop2 sbits (cdr ns))
			      (cdar ns))))
		       acc)))
		    (else
		     (loop1 (cdr ds) as acc))))))
	 (find-acc-tag
	  (lambda (s accs)
	    (cond ((null? accs) 0)
		  ((integer? (car accs)) (if (memv s accs) 1 0))
		  (else
		   (let loop ((accs accs)
			      (i 1))
		     (cond ((null? accs) 0)
			   ((memv s (car accs)) i)
			   (else (loop (cdr accs) (i+ i 1))))))))))
    (do ((i 2 (i+ i 1)))
	((>= i vl) #t)
      (let ((svec (string-create vl #\0)))
	(string-set-unsafe! svec i #\1)
	(eps-closure-org nfa svec)
	(vector-set! eps-cache i svec)))
    (let ((svec (vector-ref eps-cache (vector-ref nfa 0))))
      ;;; start at dfa state 1; 0 will be reserved for the error state
      (set! next-id (i+ next-id 1))
      (set! svec (cons next-id svec))
      (unless (is-nak? svec)
	(set! accept-states (cons (car svec) accept-states)))
      (set! work-list (cons svec work-list))
      (hash-table-set! dfa-states (cdr svec) (car svec)))
    (until (null? work-list)
	   (let* ((cw (car work-list))
		  (cur-id (car cw))
		  (cur (cdr cw)))
	     (set! work-list (cdr work-list))
	     (for-each
	      (lambda (input)
		(let ((p (string-create vl #\0))
		      (pset #f))
		  (do ((s 2 (i+ s 1)))
		      ((>= s vl) #t)
		    (unless (char=? #\0 (string-ref-unsafe cur s))
		      (for-each (lambda (tr)
				  (when (memv input (cdr tr))
				    (string-set-unsafe! p (car tr) #\1)
				    (set! pset #t)))
				(cddr (vector-ref nfa s)))))
		  (when pset
		    (set! p (eps-closure-cache eps-cache p))
		    (let ((to-id (hash-table-ref dfa-states p #f)))
		      (unless to-id
			(set! next-id (i+ next-id 1))
			(set! to-id next-id)
			(set! p (cons to-id p))
			(unless (is-nak? p)
			  (set! accept-states (cons to-id accept-states)))
			(set! work-list (cons p work-list))
			(hash-table-set! dfa-states (cdr p) (car p)))
		      (set! trans (cons (list cur-id to-id input) trans))))))
	      alphabet)))
    (let ((es (map (lambda (e) (cons (cdr e) (car e)))
		   (hash-table-entries dfa-states))))
      (set! dfa-states (list-sort (lambda (e1 e2) (> (car e1) (car e2))) es)))
    (let* ((max-state (max (apply max (map car trans))
			   (apply max (map cadr trans))))
	   (dvec (vector-create (i+ max-state 1)))
	   (avec (vector-create (i+ max-state 1) 0))
	   (accs (map-accept-states dfa-states accept-states)))
      (set! trans (list-sort (lambda (t1 t2) (< (car t1) (car t2))) trans))
      (set! trans (list-group-by (lambda (t1 t2) (= (car t1) (car t2))) trans))
      (set! trans (map (lambda (tg) (cons (caar tg) (map cdr tg))) trans))
      (for-each (lambda (i)
		  (let ((t1 (memp (lambda (t) (= (car t) i)) trans)))
		    (when t1
		      (let ((t3 (list-sort
				 (lambda (s t) (< (car s) (car t)))
				 (cdar t1))))
			(set! trans (cdr t1))
			(vector-set! dvec i t3))))
		  (vector-set! avec i (find-acc-tag i accs)))
		(fromto 1 max-state))
      (vector-set! dvec 0 avec)
      dvec)))

(define (dfa-alphabet dfa)
  (let* ((type (lambda (val)
		 (cond ((char? val) 1)
		       ((string? val) 2)
		       ((symbol? val) 3)
		       ((integer? val) 4)
		       (else (raise (sprintf "bad input type! %v" val))))))
	 (cmp (lambda (v1 v2)
		(let ((t1 (type v1))
		      (t2 (type v2)))
		  (if (= t1 t2)
		      (case t1
			((1) (char<? v1 v2))
			((2) (string<? v1 v2))
			((3) (string<? (symbol->string v1)
				       (symbol->string v2)))
			((4) (< v1 v2)))
		      (< t1 t2)))))
	 (vl (vector-length dfa))
	 (acc ()))
    (do ((i 1 (i+ i 1)))
	((>= i vl) #t)
      (set! acc (list-append (list-flatten (map cdr (vector-ref dfa i))) acc)))
    (map car (list-group-by eqv? (list-sort cmp acc)))))

;;; Reduce DFA - this does not minimize it, and it may not reduce anything

(define (dfa-reduce1 dfa)
  (let* ((vl (vector-length dfa))
	 (as (vector-ref dfa 0))
	 (augs ())
	 (smap (vector-create vl)))
    (do ((i 1 (i+ i 1)))
	((>= i vl) #t)
      (let* ((acc (vector-ref as i))
	     (ts (list-sort (lambda (t1 t2) (< (car t1) (car t2)))
			    (vector-ref dfa i)))
	     (h2 (sprintf "%d:%v" acc ts)))
	(set! augs (cons (list h2 i acc ts) augs))))
    (let* ((a1 (list-group-by
		(lambda (t1 t2) (string=? (car t1) (car t2)))
		(list-sort
		 (lambda (t1 t2) (string<? (car t1) (car t2)))
		 augs)))
	   (a2 (list-sort (lambda (t1 t2) (< (caar t1) (caar t2)))
			  (map (lambda (ag)
				 (list (list-sort < (map cadr ag))
				       (caddar ag) (cadddar ag)))
			       a1)))
	   (lr (list-length a2))
	   (a3 (upfrom 1 lr))
	   (dfar (vector-create (i+ lr 1) #f))
	   (accr (vector-create (i+ lr 1) #f)))
      (for-each (lambda (i sg)
		  (vector-set! accr i (cadr sg))
		  (for-each (lambda (s) (vector-set! smap s i)) (car sg)))
		a3 a2)
      (for-each (lambda (i sg)
		  (vector-set!
		   dfar i
		   (map (lambda (tr)
			  (cons (vector-ref smap (car tr)) (cdr tr)))
			(caddr sg))))
		  a3 a2)
      (vector-set! dfar 0 accr)
      dfar)))

;;; Iterated DFA reduction

(define (dfa-reduce2 dfa)
  (let ((dfr (dfa-reduce1 dfa)))
    (if (eqv? dfa dfr)
	dfa
	(begin
	  (printf "%d -> %d\n" (vector-length dfa) (vector-length dfr))
	  (dfa-reduce2 dfr)))))

;;; Attempt to group DFA transition characters into equivalence classes
;;; Does not work yet(?): I think it needs a minimized DFA
;;; Seems to work at least for small DFAs that come out of dfa-reduce2

(define (dfa-classes dfa)
  (let* ((vl (vector-length dfa))
	 (ts ())
	 (is-lt? (lambda (t1 t2)
		   (cond ((char<? (car t1) (car t2)) #t)
			 ((char>? (car t1) (car t2)) #f)
			 (else (cond ((< (cadr t1) (cadr t2)) #t)
				     ((> (cadr t1) (cadr t2)) #f)
				     (else (cond ((< (caddr t1) (caddr t2)) #t)
						 ((> (caddr t1) (caddr t2)) #f)
						 (else #f))))))))
	 (ceq? (lambda (t1 t2) (char=? (car t1) (car t2))))
	 (grp (lambda (tg) (cons (caar tg) (map cdr tg))))
	 (teq? (lambda (t1 t2) (eqv? (cdr t1) (cdr t2))))
	 (trs (do ((i 1 (i+ i 1)))
		  ((>= i vl)
		   (map grp (list-group-by ceq? (list-sort is-lt? ts))))
		(for-each (lambda (tr)
			    (set! ts (cons (list (cadr tr) i (car tr)) ts)))
			  (vector-ref dfa i)))))
    (map (lambda (tg) (map car tg)) (list-group-by teq? trs))))

;;; Expand a DFA: treating character ch as the template, add transitions for
;;; all characters in chs to the DFA

(define (dfa-augment dfa ch chs)
  (let* ((vl (vector-length dfa))
	 (dfa2 (vector-create vl))
	 (aug (lambda (st) (map (lambda (c) (list st c)) chs)))
	 (is-lt? (lambda (t1 t2)
		   (cond ((< (car t1) (car t2)) #t)
			 ((> (car t1) (car t2)) #f)
			 (else (cond ((char<? (cadr t1) (cadr t2)) #t)
				     ((char>? (cadr t1) (cadr t2)) #f)
				     (else #f)))))))
    (vector-set! dfa2 0 (vector-ref dfa 0))
    (do ((i 1 (i+ i 1)))
	((>= i vl) dfa2)
      (let loop ((ts1 (vector-ref dfa i))
		 (ts2 ()))
	(cond ((null? ts1)
	       (vector-set! dfa2 i (list-sort is-lt? ts2)))
	      ((char=? (cadar ts1) ch)
	       (loop (cdr ts1) (list-append (cons (car ts1) ts2)
					    (aug (caar ts1)))))
	      (else (loop (cdr ts1) (cons (car ts1) ts2))))))))

(define (run-dfa dfa input start)
  (when (string? input)
    (set! input (string->list input)))
  (unless (vector? input)
    (set! input (list->vector input)))
  (let ((cur-state 1)
	(last-acc #f)
	(last-suffix 0)
	(input-len (vector-length input))
	(accs (vector-ref dfa 0))
	(cont #t)
	(index start)
	(find-state
	 (lambda (trans input)
	   (let loop ((trs trans))
	     (cond ((null? trs) #f)
		   ((memv input (cdar trs))
		    (caar trs))
		   (else (loop (cdr trs))))))))
    (while (and cont (< index input-len))
	   (let ((cur-acc (vector-ref accs cur-state)))
	     (unless (zero? cur-acc)
	       (set! last-suffix index)
	       (set! last-acc cur-acc)))
	   (let* ((cur-in (vector-ref input index))
		  (next (find-state (vector-ref dfa cur-state) cur-in)))
	     (if next
		 (begin (set! cur-state next)
			(set! index (i+ index 1)))
		 (set! cont #f))))
    (when cont
      (let ((cur-acc (vector-ref accs cur-state)))
	(unless (zero? cur-acc)
	  (set! last-suffix #f)
	  (set! last-acc cur-acc))))
    (if last-acc
	(cons last-acc last-suffix)
	#f)))

;;; optional argument bits8 to say whether to generate a DFA
;;; for 8-bit characters (default: true) or for 7-bit characters

(define (dfa-to-c dfa fout name . bits8)
  (set! bits8 (if (null? bits8) #t (car bits8)))
  (let* ((n-chars (if bits8 256 128))
	 (max-state (i- (vector-length dfa) 1))
	 (alph (dfa-alphabet dfa))
	 (state-type (cond ((< max-state 256) "uint8_t")
			   ((< max-state 65536) "uint16_t")
			   (else "uint32_t")))
	 (states (fromto 1 max-state))
	 (accs (vector-ref dfa 0))
	 (own-port #f))
    (unless (all-true? (map char? alph))
      (display alph stderr)
      (newline stderr)
      (raise "dfa-to-c error! alphabet is not entirely chars"))
    (when (string? fout)
      (set! fout (open-file fout "w"))
      (set! own-port #t))
    (unless (port? fout)
      (raise "dfa-to-c error! output is not a port or string"))
    (write-string fout
     "#include <stddef.h>\n"
     "#include <stdint.h>\n"
     "\n"
     "typedef struct { " state-type " accept; " state-type
     " next["
     (number->string n-chars)
     "]; } dfa_state_t;\n"
     "\n"
     "static const dfa_state_t dfa_states["
     (number->string max-state)
     "] = {\n")
    (for-each (lambda (s)
		(let ((sv (vector-create n-chars 0)))
		  (for-each (lambda (t)
			      (let ((ns (car t))
				    (ix (char->integer (cadr t))))
				(vector-set! sv ix ns)))
			    (vector-ref dfa s))
		  (fprintf fout "  { %d, { " (vector-ref accs s))
		  (vector-for-each (lambda (v) (fprintf fout "%d," v)) sv)
		  (write-string fout " } },\n")))
	      states)

    (write-string fout
     "};\n"
     "\n"
     "uint32_t " name "(const char* input, char** suffix)\n"
     "{\n"
     "    uint32_t lacc = 0, cur_state = 0, next_state;\n"
     "    uint8_t inch;\n"
     "    char* lsuf = (char*) input;\n"
     "\n"
     "    while ((inch = (uint8_t) (*input)) != '\\0') {\n"
     "	if (dfa_states[cur_state].accept) {\n"
     "	    lsuf = (char*) input;\n"
     "	    lacc = dfa_states[cur_state].accept;\n"
     "	}\n")
    (unless bits8
      (write-string fout "	if (inch >= 128) { goto ret_suf; }\n"))
    (write-string fout
     "	next_state = dfa_states[cur_state].next[inch];\n"
     "	if (next_state == 0) { goto ret_suf; }\n"
     "	++input;\n"
     "	cur_state = next_state - 1;\n"
     "    }\n"
     "    if (dfa_states[cur_state].accept) {\n"
     "	lsuf = NULL;\n"
     "	lacc = dfa_states[cur_state].accept;\n"
     "    }\n"
     "  ret_suf:\n"
     "    if (suffix) { *suffix = lsuf; }\n"
     "    return lacc;\n"
     "}\n")

    (write-string fout
     "\n"
     "#ifdef USE_DRIVER\n"
     "#include <stdio.h>\n"
     "#include <string.h>\n"
     "\n"
     "int main(int argc, char** argv)\n"
     "{\n"
     "    char buf[256], *suf;\n"
     "    uint32_t acc;\n"
     "\n"
     "    while (fgets(buf, sizeof(buf), stdin) != NULL) {\n"
     "	char* p = strchr(buf, '\\n');\n"
     "	if (p) { *p = '\\0'; }\n"
     "	acc = " name "(buf, &suf);\n"
     "	printf(\"%s is %s:%d\\n\", buf, acc ? \"good\" : \"bad\", acc);\n"
     "	if (suf) {\n"
     "	    printf(\"unrecognized suffix '%s'\\n\", suf);\n"
     "	}\n"
     "    }\n"
     "    return 0;\n"
     "}\n"
     "#endif // USE_DRIVER\n")
    (when own-port
      (close-port fout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; draw NFA or DFA graphs using graphviz

(define (draw-nfa nfa)
  (let ((port (run-write-command "dot -Tps > dot-nfa.ps")))
    (write-string port "digraph {\n  rankdir=LR;\n")
    (let loop ((ss (cddr nfa)))
      (if (null? ss)
	  (begin
	    (write-string port "}\n")
	    (flush-port port)
	    (close-port port))
	  (let* ((s (car ss))
		 (sid (car s)))
	    (fprintf port "  n%d [ label=\"%d\" ];\n" sid sid)
	    (for-each (lambda (id)
			(fprintf port "  n%d -> n%d [ label=\"eps\", fontname=\"Ubuntu:Italic\", fontcolor=\"gray80\" ];\n" sid id))
		      (cadr s))
	    (for-each (lambda (tr)
			(fprintf port "  n%d -> n%d [ label=\"%v\" ];\n"
				 sid (car tr) (cadr tr)))
		      (cddr s))
	    (loop (cdr ss)))))))

(define (draw-dfa dfa)
  (let ((max-s (i- (vector-length dfa) 1))
	(accs (vector-ref dfa 0))
	(port (run-write-command "dot -Tps > dot-dfa.ps")))
    (write-string port "digraph {\n  rankdir=LR;\n")
    (do ((i 1 (i+ i 1)))
	((> i max-s) #t)
      (let ((tag (vector-ref accs i)))
	(if (zero? tag)
	    (fprintf port "  n%d [ label=\"%d\" ];\n" i i)
	    (fprintf port "  n%d [ label=\"%d:%d\" peripheries=2];\n"
		     i i tag))))
    (for-each (lambda (i)
		(for-each (lambda (tr)
			    (fprintf port "  n%d -> n%d [ label=\"%v\" ];\n"
				     i (car tr) (cadr tr)))
			  (vector-ref dfa i)))
	      (fromto 1 max-s))
    (write-string port "}\n")
    (flush-port port)
    (close-port port)))
