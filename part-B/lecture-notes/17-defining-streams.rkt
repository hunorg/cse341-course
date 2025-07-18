#lang racket

(provide (all-defined-out))

; 1 1 1 1 1 1 1 ...

(define ones (lambda () (cons 1 ones)))
; (define ones-really-bad (cons 1 ones-really-bad))
(define ones-bad (lambda () (cons 1 (ones-bad))))

; 1 2 3 4 5 ...

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


; 2 4 8 16 ...

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

; (define (stream-maker fn arg) ...)
; (define nats (stream-maker + 1))
; (define powers-of-two (stream-maker * 2))


