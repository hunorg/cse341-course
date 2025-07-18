#lang racket

; rewrite previous sum1 and sum2 with cond

(provide (all-defined-out))

(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))

(define (sum4 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum4 (cdr xs)))]
        [(list? (car xs)) (+ (sum4 (car xs)) (sum4 (cdr xs)))]
        [#t (sum4 (cdr xs))]))

; it is not an error if the result is not #t or #f

; semantics of if and cond: treat anything other than #f as true (in some languages, other things are false, not in Racket)

; this feature makes no sense in a statically typed language

; some consider using this feature poor style, but it can be convenient

(define test1
  (if 34 14 15))

(define test2
  (if null 14 15))

(define test3
  (if #f 14 15))

(define (count-falses xs)
  (cond [(null? xs) 0]
        [(car xs) (count-falses (cdr xs))]
        [#t (+ 1 (count-falses (cdr xs)))]))

(define test4
  (count-falses (list 34 #t "hi")))

(define test5
  (count-falses (list #f 34 #t #f "hi" #f #f)))

