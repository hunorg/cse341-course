#lang racket

(provide (all-defined-out))

; thunks let you skip expensive computations if they are not needed
; great if you take the true-branch:

; (define (f th)
; (if (..) 0 (... (th) ...)))

; but worse if you end up using the thunk more than once:

; (define (f th)
; (... (if (...) 0 (... (th) ...))
; (if (...) 0 (... (th) ...))
; ...
; (if (...) 0 (... (th) ...))))
; this is a silly addition function that purposely runs slows for
; demonstration purposes

(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

; multiplies x and result of y-thunk, calling y-thunk x times

(define (my-mult x y-thunk) ; assumes x is >= 0
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

(define my-mult-test (my-mult 99 (lambda () (slow-add 3 4))))
(define my-mult-test2 (my-mult 99 (let ([x (slow-add 3 4)]) (lambda () x))))

; Best of both worlds

; assuming some expensive computation has no side effects, ideally
; we would:
; not compute until needed
; remember the answer so future uses complete immediately
; called lazy evaluation

; languages where most constructs, including function arguments,
; work this way are lazy languages
; Haskell, Nix

; Racket predefines support for promises, but we can make our own
; thunks and mutable pairs are enough
