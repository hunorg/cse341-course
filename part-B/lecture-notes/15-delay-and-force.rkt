#lang racket

(provide (all-defined-out))

; Delay and force

(define (my-delay th)
  (mcons #f th)) ; a "promise"

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

; Using promises

; (define (f p)
; (... (if (...) 0 (... (my-force p) ...))
; (if (...) 0 (... (my-force p) ...))
; ...
; (if (...) 0 (... (my-force p) ...))))

; (f (my-delay (lambda () e)))

; multiplies x and result of y-thunk, calling y-thunk x times
(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

(define (my-mult x y-thunk) ; assumes x is >= 0
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

(my-mult 100 (let ([p (my-delay (lambda () (slow-add 3 4)))])
               (lambda () (my-force p))))

; thanks to lazy evaluation, after the first time we updated the promise
; to have the answer 7

; Lessons From Example

; see example that does multiplication using a very slow
; addition helper function

; with thunking second argument:
; great if first argument 0
; okay if first argument 1
; worse otherwise

; with precomputing second argument:
; okay in all cases

; with thunk that uses a promise for second argument:
; great if first argument 0
; okay otherwise

