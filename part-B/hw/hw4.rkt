
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low ( sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t
         (let* ([len (length xs)]
                [i (remainder n len)])
           (car (list-tail xs i)))]))

(define ones (lambda () (cons 1 ones)))

(define (stream-for-n-steps s n)
  (if (<= n 0)
      null
      (let ([next (s)])
        (cons (car next)
              (stream-for-n-steps (cdr next) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([value (if (= (remainder x 5) 0)
                                 (- x)
                                 x)])
                  (cons value (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x)
                (let ([value (if (string=? x "dan.jpg")
                                 "dog.jpg"
                                 "dan.jpg")])
                  (cons x (lambda () (f value)))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (lambda ()
    (let ([next (s)])
      (cons (cons 0 (car next))
            (stream-add-zero (cdr next))))))










