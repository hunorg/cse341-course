#lang racket

(provide (all-defined-out))

; unlike ML, Racket really has assignment statements
; but used only-when-really-appopiate!

; (set! x e) set-bang

; for the x in the current inverionment, subsequent lookups of x get the result
; of evaluating expression e
; any code using this x will be affected
; like x = e in Java, C, Python, etc.

; once you have side-effects, sequences are useful
; (begin e1 e2 ... en)

; example uses set! at top-level; mutation local variables is similar

(define b 3)
(define f (lambda (x) (* 1 (+ x b))))
(define c (+ b 4)) ; 7
(set! b 5)
(define z (f 4)) ; 9
(define w c) ; 7

; not much new here:
; environment for closure determined when function is defined,
; but body is evaluated when function is called
; once an expression produces a value, it is irrevelant how the value was produced

;mutating top-level definitions is particularly problematic
; what if any code could do set! on anything?
; how could we defend against this?

; a general principle: if something you need not to change might
; change, make a local copy of it - example:

; (define f
; (let ([b b])
; (lambda (x) (* 1 (+ x b)))))

; simple elegant languages design:
; primitives like + and * are just predefined variables bound to functions
; but maybe that means they are mutable
; example continued:
; (define f
; (let ([b b]
; [+ +]
; [* *]
; (lambda (x) (* 1 (+ x b))))))

; even that don't work if f uses other functions that use things
; that might get mutated - all functions would need to copy
; everyting mutable they used

; in racket, you do not have to program like this
; each file is a modile
; if a module does not use set! on a top-level variable, then
; Racket makes it constant and forbids set! outside the module
; primitives like +, *, and cons are in a module that does not
; mutate them

; Showed you this for the concept of copying to defend against mutation
; easier defense: Do not allow mutation
; mutable top-level bindings a highly dubious idea
