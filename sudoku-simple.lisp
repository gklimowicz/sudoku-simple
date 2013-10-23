;;;; ____________________________________________________________
;;;; Sudoku solver

;;;; Puzzle representation is a linear array 81 or 256 elements long,
;;;; corresponding to the row-major order of the square 9x9 or 16x16 matrix.
;;;; In the initial puzzle representation, blank spaces are represented by "nil".

(in-package #:sudoku)

(defparameter *size* 81)
(defparameter *side* 9)

(declaim (optimize (debug 3)))

(let ((elt-to-affected-elts-81
       (let* ((submatrices-81
               (append
                (loop for row below 9
                   collect (loop for col below 9 collect (+ (* row 9) col)))
                (loop for col below 9
                   collect (loop for row below 9 collect (+ (* row 9) col)))
                ;; 3 x 3 submatrices
                (loop for ul in '(0 3 6 27 30 33 54 57 60)
                   collect (loop for offset in '(0 1 2 9 10 11 18 19 20) collect (+ ul offset)))))
              (elt-to-submatrices-81
               (apply #'vector (loop for i below 81 collect
                                    (loop for m in submatrices-81
                                       when (member i m)
                                       collect m)))))
         (apply #'vector
                (loop for i below 81
                   as sub-matrices = (svref elt-to-submatrices-81 i)
                   collect (sort (remove i (union (first sub-matrices)
                                                  (union (second sub-matrices)
                                                         (third sub-matrices))))
                                 #'<))))))

  (defun elts-affected-by (elt)
    (svref elt-to-affected-elts-81 elt)))

(defun print-sudoku (puzzle)
  "Print a sudoku puzzle, substituting dots for nil entries."
  (fresh-line)
  (loop for i below *size*
     do (format t "~2@A "
                (if (null (svref puzzle i)) "." (mod (svref puzzle i) 16)))
       (if (eql (mod i *side*) (1- *side*))
           (format t "~%"))))

(defun valid-sudoku-p (puzzle)
  "Determine whether this is a valid representation for a sudoku puzzle. It's got to be a nine-by-nine square, with no duplicate values in the rows. columns or sub-matrices."
  ;; must be 9 by 9
  (let* ((size (length puzzle))
         (side (isqrt size))
	 (validp t))
    (when (/= 81 *size*)
      (format t "Invalid sudoku puzzle size ~D (expected 81)~%" size)
      (setq validp nil))

    (loop for i below size
       as elt = (svref puzzle i)
       do (when (and elt (integerp elt) (< elt 1) (> elt side))
            (format t "Invalid sudoku puzzle at position ~D (row ~D col ~D): invalid number in box: ~D~%"
                    i (truncate i side) (mod i side) elt)
            (setq validp nil))
         (when (and elt (not (integerp elt)) (not (consp elt)))
           (format t "Position ~D (row ~D col ~D): not nil or cons: ~A~%"
                   i (truncate i side) (mod i side)elt)
           (setq validp nil)))
    (loop for i below size
       when (integerp (svref puzzle i))
       do (loop for x in (elts-affected-by i )
             do (when (and (integerp (svref puzzle x))
                           (< i x)
                           (eql (svref puzzle i) (svref puzzle x)))
                  (format t "Invalid sudoku puzzle: Conflict between location ~D (row ~D col ~D) and ~D (row ~D col ~D): both contain ~D~%"
                          i (truncate i side) (mod i side)
                          x (truncate x side) (mod x side) (svref puzzle i))
                  (setq validp nil))))
    validp))


(defun find-fewest-choices (puzzle)
  "Find a spot in the puzzle with the least number of possibilities.
When there are multiple entries with the same number of elements, choose one randomly."
  ;; (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop with low-length = *side*
     with low-spot = nil
     for i below *size*
     as elt = (svref puzzle i)
     do (when (and (consp elt) (< (length elt) low-length))
          (setq low-spot (list i))
          (setq low-length (length elt)))
       (when (and (consp elt) (= (length elt) low-length))
         (push i low-spot))
     finally (return (nth (random (length low-spot)) low-spot))))


(defun solve-sudoku-1 (puzzle find-best-spot elts-placed)
  "Do the heavy lifting to advance a potential sudoku solution one step.
Find the best spot to try to advance, the one with the least possibilities.
Then for each possibility, try it and recurse to advance further."
  ;; (declare (optimize (speed 3) (safety 1) (debug 1)))
  (when (eql elts-placed *size*)
    (throw 'solution-found puzzle))
  (let ((spot (funcall find-best-spot puzzle)))
    (when spot
      (loop with affected = (cons spot (elts-affected-by spot))
            with saved-elts = (loop for i in affected collect (svref puzzle i))
            for guess in (svref puzzle spot)
	    do
            ;; remove the value from other affected puzzle entries
            (loop for i in affected
	      when (listp (svref puzzle i))
	      do (setf (svref puzzle i) (remove guess (svref puzzle i) :test #'= :count 1))
               )
            ;; apply the value
            (setf (svref puzzle spot) guess)
            ;; solve the remaining sudoku
            (solve-sudoku-1 puzzle find-best-spot (1+ elts-placed))
            ;; put the affected puzzle values back
            (loop for elt in saved-elts as i in affected
               do (setf (svref puzzle i) elt))))))


(defun solve-sudoku (puzzle-template)
  "Solve a sudoku puzzle.

Copy the puzzle, replacing the 'nil's in the representation
with the list of all possible values that remain for that element."
  (let ((puzzle (copy-seq puzzle-template)))

    ;; Fill the nil spots with the potential candidate values
    (loop for i below *size*
       when (null (svref puzzle i))
       do (setf (svref puzzle i)
                (loop with candidates = (loop for i from 1 to *side* collect i)
                   for k in (elts-affected-by i)
                   as elt = (svref puzzle k)
                   when (integerp elt)
                   do (setq candidates (remove elt candidates :test #'= :count 1))
                   finally (return candidates))))

    ;; Now try to recursively solve the puzzle
    (let ((solution (catch 'solution-found
                      (solve-sudoku-1 puzzle #'find-fewest-choices
                                      (loop for i below (length puzzle)
                                         count (integerp (svref puzzle i)))))))
      solution)))


;;; Solve selected puzzles.
(defun solve-some-puzzles ()
  (with-open-file (puzzles-stream "../sudoku-samples/all-samples.lisp")
    (loop for i below 1000
         as puzzle = (read puzzles-stream nil nil)
         as puzzle-vector = (apply #'vector (getf puzzle :board))
         as puzzle-columns = (getf puzzle :columns)
         as puzzle-rows = (getf puzzle :rows)
         while puzzle
         when (and (eql puzzle-columns 9) (eql puzzle-rows 9))
         do (format t "Puzzle ~D (valid ~A):~%" i (valid-sudoku-p puzzle-vector))
         (print-sudoku puzzle-vector)
         (terpri)
         (let ((solution (solve-sudoku puzzle-vector)))
           if (null solution)
               (format t "Solution:~%")
               (progn
                 (format t "Solution:~%")
                 (assert (valid-sudoku-p solution))
                 (print-sudoku solution)
                 (terpri))))))
