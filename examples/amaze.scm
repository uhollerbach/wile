;;; Copyright 2024 - 2025, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: GPLv3 or later, see file 'LICENSE' for details
;;;
;;; Demo program for wile: a random-maze generator
;;;
;;; Compile and run it:
;;;     wile -x amaze.scm
;;;     amaze > maze.dat
;;; then use your favorite tool to plot the line segments

;;; set up default size and declare the visited variable, which will
;;; later get set to be a vector

(define nx 21)
(define ny 21)
(define visited #f)

;;; mark a cell in the visited array as visited

(define (set-visited ix iy)
  (vector-set! visited (i+ ix (i* nx iy)) #t))

;;; get the status of a cell

(define (get-visited ix iy)
  (vector-ref visited (i+ ix (i* nx iy))))

;;; the "interface" to graphics-land

(define (draw-from-to x1 y1 x2 y2)
  (printf "%d %d\n%d %d\n\n" x1 y1 x2 y2))

;;; the heart of the maze generator: given some site, mark it as visited,
;;; then examine its neighbors: up to 3 can be still visitable, so loop 3
;;; times - but since visitable could disappear in the meantime, recheck
;;; everything every time. pick one of the remaining visitable cells at
;;; random and recursively process that cell

(define (maze-step cx cy)
  (set-visited cx cy)
  (do ((i 0 (i+ i 1)))
      ((>= i 3) #t)
    (let* ((ns (list (list (+ cx 1) cy)
		     (list (- cx 1) cy)
		     (list cx (+ cy 1))
		     (list cx (- cy 1))))
	   (ls (filter (lambda (cc) (not (apply get-visited cc))) ns))
	   (lls (list-length ls)))
      (when (positive? lls)
	(let* ((ix (integer (floor (random-uniform 0 (- lls 1.0e-10)))))
	       (nc (list-ref ls ix))
	       (nx (car nc))
	       (ny (cadr nc)))
	  (draw-from-to cx cy nx ny)
	  (maze-step nx ny))))))

;;; read maze size from the command line, if applicable

(when (list-length>=? 2 command-line-arguments)
  (set! nx (string->number (car command-line-arguments)))
  (set! ny (string->number (cadr command-line-arguments))))

;;; create the visited array

(set! visited (vector-create (i* nx ny) #f))

;;; mark the borders as already-visited; this simplifies the neighbors
;;; checking in maze-step above

(do ((i 0 (i+ i 1)))
    ((>= i nx) #t)
  (set-visited i 0)
  (set-visited i (i- ny 1)))

(do ((i 0 (i+ i 1)))
    ((>= i ny) #t)
  (set-visited 0 i)
  (set-visited (i- nx 1) i))

;;; just because we can, draw an impassable wall partway through the middle
;;; of the maze

(let* ((nq1 (quotient nx 4))
       (nq3 (i- nx nq1))
       (nyh (quotient ny 2)))
  (do ((i nq1 (i+ i 1)))
      ((>= i nq3) #t)
    (set-visited i nyh)))

;;; draw the entry and exit paths, then generate the maze starting from (1 1)

(draw-from-to 0 0 1 1)
(draw-from-to (i- nx 2) (i- ny 2) (i- nx 1) (i- ny 1))
(maze-step 1 1)
