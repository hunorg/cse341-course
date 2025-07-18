#lang racket

(provide (all-defined-out))

; Streams

; a stream is an infinite sequence of values
; so cannot make a stream by making all the values
; key idea: use a thunk to delay creating most of the sequence
; just a programming idiom

; a powerful concept for division of labor:
; stream producer knows how create any number of values
; stream consumer decides how many values to ask for

; some examples of stream you might (not) be familiar with:
; user actions (mouse clicks, etc.)
; UNIX pipes: cmd1 | cmd2 has cmd2 "pull" data from cmd1
; output values from a sequential feedback circuit

; Using streams

; we will represent streams using pairs and thunks
; let a stream be a thunk that when called returns a pair:
; '(next-answer . next-thunk)

; so given a stream st, the client can get any number of elements
; first: (car (s))
; second (car ((cdr (s))))
; third: (car ((cdr ((cdr (s)))))
; (usually bind (cdr (s)) to a variable or pass to a recursive
; function)

(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))
