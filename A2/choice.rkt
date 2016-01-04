#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 3
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#

#|This problem is solved recursively, each subset is either the first element of the list or the empty list
combined with every member of the subset of the rest of the list. The base case is when we have no elements
in the list, return the empty list.|#
(define (subsets lst)
  (if (equal? (length lst) 0) lst
   (flatten (append (list(-< '() (first lst))) (subsets(rest lst))))))

#|This helper function acts very similarily to -< but takes in a list of indeterminate size instead.
So for examples, calling subsetsHelper on (1 2 3) would return 1, then, when (next) is entered 2,
and once it's entered again 3|#
(define (subsetsHelper lst) (if (equal? (length lst) 1) (first lst) (-< (first lst) (subsetsHelper (rest lst)))))


; QUESTION 4
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#
#|The implementation of this algorithm is a brute force approach to sudoku solving, we try
each value in each empty space until we end up with a valid board.|#
(define (sudoku-4 board)(?- sudoku-checker  
  (map (lambda (lst)(map(lambda (x) (if (string? x) (-< 1 2 3 4) x)) lst ))board)))

#|This helper function takes in a list of lists representing a completed sudoku board and determines
if it is valid within the rules of sudoku. Nothing clever here, just checking each row column and box
to make sure the numbers 1, 2, 3 and 4 appear in it|#
(define (sudoku-checker lst)
    (if (equal? '(1 2 3 4) (sort (list (first (first lst)) (first (second lst)) (
                              first (third lst)) (first (fourth lst))) < )) (
     if (equal? '(1 2 3 4) (sort (list (second (first lst)) (second (second lst)) (
                              second (third lst)) (second (fourth lst))) < )) (
     if (equal? '(1 2 3 4) (sort (list (third (first lst)) (third (second lst)) (
                              third (third lst)) (third (fourth lst))) < )) (
      if (equal? '(1 2 3 4) (sort (list (fourth (first lst)) (fourth (second lst)) (
                              fourth (third lst)) (fourth (fourth lst))) < )) (
      if (equal? '(1 2 3 4) (sort (list (first (first lst)) (first (second lst)) (
                              second (first lst)) (second (second lst))) < )) (
       if (equal? '(1 2 3 4) (sort (list (first (third lst)) (first (fourth lst)) (
                              second (third lst)) (second (fourth lst))) < )) (
        if (equal? '(1 2 3 4) (sort (list (third (first lst)) (third (second lst)) (
                              fourth (first lst)) (fourth (second lst))) < )) (
         if (equal? '(1 2 3 4) (sort (list (third (third lst)) (third (fourth lst)) (
                              fourth (third lst)) (fourth (fourth lst))) < )) (
         if (equal? '(1 2 3 4) (sort (first lst) < )) (
           if (equal? '(1 2 3 4) (sort (second lst) < )) (
             if (equal? '(1 2 3 4) (sort (third lst) < )) (
               if (equal? '(1 2 3 4) (sort (fourth lst) < ))
               #t #f) #f) #f) #f)#f)#f)#f)#f)#f)#f) #f) #f
                              ))

; QUESTION 5
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
;(fold-< max 0 (sin (* (-< 1 2 3 4) (+ (-< 100 200) (-< 1 2)))))
(define-syntax fold-<
  (syntax-rules ()
    [(fold-< <combine> <init> <lst>)
     (foldl <combine> <init> (all <lst>)) ;Calls all on the expression, getting all results of the next, and then fold to apply the rest
     ]
    ))

#|
;(get-all-nexts (sin (* (-< 1 2 3 4) (+ (-< 100 200) (-< 1 2)))))
(define (get-all-nexts choice)
    (cond
      [(equal? choice "false.") all-nexts]
      [else (add-next! choice)(get-all-nexts (next))]
      )
  )

(define all-nexts '())


(define (add-next! choice)(
            set! all-nexts (flatten (cons choice all-nexts))))
|#