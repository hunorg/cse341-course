#lang racket

(provide (all-defined-out))

; cons cells are immutable

; what if youw anted to mutate the contents of a cons cell?
; in racket, you cannot (major change from Scheme)
; this is good
; list-aliasing irrevelant
; implementation can make list? fast since listness is
; determined when cons cell is created

(define x (cons 14 null))
(define y x)
(set! x (cons 42 null))
(define z x)

; (set! (car x) 45) - this does not work in Racket

(define mpr (mcons 1 (mcons #t "hi")))
(define mpr-test (mcar mpr))
(define mpr-test2 (mcdr mpr))
(define mpr-test3 (mcar (mcdr mpr)))

(set-mcar! (mcdr mpr) 47)
(define mpr-test4 (mcdr mpr))
(set-mcdr! (mcdr mpr) 14)

(define length-test (length (cons 2 (cons 3 null))))
; (define length-test2 (length (mcons 2 (mcons 3 null)))) - doesn't work

; since mutable pairs are sometimes useful (will use them soon),
; Racket provides them too:
; mcons
; mcar
; mcdr
; mpair?
; set-mcar!
; set-mcdr!

; run-time error to use mcar on a cons cell or car on an mcons cell





