sudoku-simple
=============

A simple sudoku solver that validates puzzles first before solving them by elimination and recursion.

# Introduction
This is a very simple sudoku solver. It tracks what potential entries can be eliminated by the rules of exclusion (rows, columns, segments), and then resorts to recursion on cells with the fewest choices remaining when we run out of elimination we can do.

It's not a clever strategy, and doesn't work super-well on 16x16 Sudoku problems.

Gary Klimowicz
2013-10-22