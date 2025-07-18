#lang racket

; list processing: null, cons, null?, car, cdr
; we won't use pattern matching in Racket

(provide (all-defined-out))

; sum all the numbers in a list
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

(define test-sum
  (sum (list 3 4 5 6)))

; append
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

(define test-my-append
  (my-append (list 1 2 3) (list 1 2 3))
  )

; map
(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map f (cdr xs)))))

(define foo
  (my-map (lambda (x) (+ x 1))
          (cons 3 (cons 4 (cons 5 null)))))

