#lang racket

(provide (all-defined-out))

; Delayed evaluation

; for each language construct, the semantics specifies when
; subexpressions get evaluated - in ML, Racket, Java, C:
; function arguments are eager (call-by-value)
; evaluated once before calling the function
; conditional brancher are not eager
; it matters:calling factorial-bad never terminates:

(define (my-if-bad x y z)
  (if x y z))

(define (factorial-bad x)
  (my-if-bad (= x 0)
             1
             (* x (factorial-bad (- x 1)))))

(define (factorial-normal x)
  (if (= x 0)
      1
      (* x (factorial-normal (- x 1)))))

; e2 and e3 should be zero-argument functions (delays evaluation)
; we know how to delay evaluation: put expression in a function!
; thanks to closures, can use all the same variables later
; a zero-argument function used to delay evaluation is called a thunk
; as a verb: thunk the expression
; this works (but it's silly to wrap if like this):

(define (my-if-strange-but-works e1 e2 e3)
  (if e1 (e2) (e3))) ; (e)

(define (factorial-okay x)
  (my-if-strange-but-works
   (= x 0)
   (lambda () 1)
   (lambda () (* x (factorial-okay (- x 1))))))

; The key pont
; evaluate an expression e to get a result: e
; a function that when called, evaluates e and returns result
; zero-argument function for "thunking": (lambda () e)
; evaluate e to some think and then call the thunk: (e)
; next: powerful idioms related to delaying evaluation and/or
; avoided repeated or unnecessary computations



