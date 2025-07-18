#lang racket

(provide (all-defined-out))

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))

; let expression can bind any number of local variables
; notice where all the parantheses are

; the expressions are all evaluated in the environment from before the let-expression
; except the body can use all the local variables from course
; this is not how ML let-expressions work
; convenient for things like (let ([x y] [y x]) ...)

(define (silly-double x)
  (let ([x (+ x 3)]
        [y (+ x 2)])
    (+ x y -5)))

; let*
; this is much more like ML's let expression

(define (silly-double2 x)
  (let* ([x (+ x 3)]
         [ y (+ x 2)])
    (+ x y -8)))

; letrec
; the expressions are evaluated in the environment that includes all the bindings

(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda (z) (+ z y w x))]
           [w (+ x 7)])
    (f -9)))

; in certain positions, like the beginning of function bodies, you can put defines
; for defining local variables, same semantics as letrec

(define (silly-mod2 x)
  (define (even? x) (if (zero? x) #t (odd? (- x 1))))
  (define (odd? x) (if (zero? x) #f (even? (- x 1))))
  (if (even? x) 0 1))

; local defines is preferred Racket style, but course materials will
; avoid them to emphasize let, let*, letrec distincion
; you can choose to use them on homework or not




