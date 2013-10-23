;;; ASDF file for simple brute-force 9x9 sudoku solver.
(defpackage #:sudoku-system
  (:use :common-lisp :asdf))

(in-package #:sudoku-system)

(defsystem #:sudoku-simple
  :version "0.1"
  :depends-on ("pathnames")
  :components ((:file "package")
               (:file "sudoku-simple"
                      :depends-on ("package"))))
