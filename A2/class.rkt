#lang racket
#| Assignment 2 - Classes

This file contains your work for Questions 1 and 2, extending the basic
class macro to include support for traits and some basic introspection.
|#
(provide class-meta class-trait)

; QUESTION 1 (metaprogramming).
(define-syntax class-meta
  (syntax-rules ()
    [(class-meta <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [(equal? msg "_attributes") (sort (list (list (id->string <attr>) <attr>) ...) #:key car string<?) ] ;makes a list of every attribute name and it's value, and sorts it by the name
               [(equal? msg "_methods") (sort (list (list (id->string <method>) (lambda (<param> ...) <body>)) ...) #:key car string<?) ] ;does the same but for every function name and it's body
               [else "Unrecognized message!"]))
       )]
    ))

;(class-meta A (z x y) [(g k) (+ x k)] [(f) (* x y)])
;(define a (A 1 2 3))
;(a "_attributes")
;(a "_methods")
;(a "x")
;((a "f") 3)



; QUESTION 2 (traits).
(define-syntax class-trait
  (syntax-rules (with)
    [(class-trait <Class> (<attr> ...) (with <trait> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (foldl (lambda (x y) (x y))  ;folds the lambda expression for the class into each trait
              (lambda (msg)
                (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
              (list <trait> ...)) ;list of traits for the foldl
       )]
    ))

; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
(define-syntax class
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))