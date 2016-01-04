#lang racket
#|
The class-constructor syntax is a modified class structure, with the key addition being
the (<arg> ...) field, and the [(<loc> <body>) ...] field.

The first field is a list of arguments to the constructor. In the example provided, this would be (a b).
When you define a new instance of a class, these take the values that you input.

Then, you define your class attributes, except, unlike a regular class,
you define a body to each of these attributes. The body can be anything; a function, a value, local variables,
arguments to the constructor, or any combination of the four.

Then, methods are defined as usual for the class structure.

Finally, local variables are defined. These are defined just like attributes, however, are inaccassable
to the outside of the class, so they are only useful for functions or assigning values to attributes.

Otherwise, the functionality is the same. Instatiate the class by passing in a value for each arguement
that the constructor has, and call attributes and methods by passing strings to the result.

The expressing that implements the provided class definition is at the bottom of this file.

Implementation-wise, the key difference between this and the usual class structure is that the only values
defined explicitly in this structure are the arguments. Then, the local values and the attributes are
locally assigned before the the message body of the class. Attributes are reachable from the outside,
since calling their name invokes a body that then draws from the defined arguments, however, the local
variables are not since they have no clause in condition block, and are only locally bound.
|#





(define-syntax class-constructor
  (syntax-rules ()
          [(class-constructor <Class>
                              (<arg> ...) ;arguements to the constructor of the class
                              [(<attr> <body1>) ...] ;attributes of the class, body defines their value, which can be drawn from the arguments 
                              [((<method> <param> ...) <body2>) ...];methods of the class
                              [(<loc> <body>) ...] ;local name bindings
                              )
           (define (<Class> <arg> ... ) 
            (let ([<loc> <body>] ...) (let ([<attr> <body1>] ...) (lambda (msg)
             (cond [(equal? msg (id->string <attr>)) <body1>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body2>)]
               ...
               [else "Unrecognized message!"])
            )))

             )]
          ))

;(class-constructor A (a b c) ([x a] [y (+ b c)]) ([(f k) (* x k)]))
;(define a (A 1 2 3))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))

;/////////////////////////////////THIS IS THE CODE THAT CREATES THE CLASS DEFINED IN PYTHON
(define (f r)(+ r 5))
(class-constructor MyClass
                   (a b)                                               ;Arguments a and b
                   [(x (f a)) (y (list b 100 r)) (z "you are cool")]   ;attributes x, y, and z
                   []                                                  ;no methods
                   [(r (f a))])      