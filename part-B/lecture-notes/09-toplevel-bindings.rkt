#lang racket

(provide (all-defined-out))

; the bindings in a file work like local defines, i.e., letrec
; like ML, you can refer to later bindings
; unlike ML, you can also refer to later bindings
; but refer to later bindings only in function bodies
; because bindings are evaluated in order
; an error to use an undefined variable
; unlike ML, cannot define the same variable twice in module
; would make no sense: cannot have both in environment

; ! beginning in Racket v6.1 (2014), letrec's semantics is slightly different
; it now doesproduce an error if you use a variable before it is defined in letrec
; so letrec and the top-level behave the same

(define (f x) (+ x (* x b))) ; forward reference okay here
(define b 3)
(define c (+ b 4)) ; backward reference okay
; (define d (+ e 4)) ; not okay (get an error instead of #<undefined>)
(define e 5)
; (define f 17) ; not okay: f already defined in this module

