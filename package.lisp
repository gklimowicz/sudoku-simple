;;;; sudoku puzzle solver
(defpackage :com.garyklimowicz.sudoku-simple
  (:nicknames :sudoku-simple)
  (:use :common-lisp)
  (:export :solve-sudoku :print-sudoku)
  (:documentation "This package contains all the software to solve 9x9 sudoku puzzles by brute force. It's fine for solving simple puzzles."))
