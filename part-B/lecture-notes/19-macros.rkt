#lang racket

(provide (all-defined-out))

; What is a macro

; a macro definition describes how to transform some new syntax
; into different syntax in the source language

; a macro is one way to implement syntactic sugar
; "replacte any syntax of the form e1 andalso e2 with
; if e1 then e2 else false"

; a macro system is a language (or part of a larger language) for
; defining macros

; Macro expansion is the process of rewriting the syntax for each macro use
; before a program is run (or even compiled)

; Using Racket Macros

; if you define a macro m in Racket, then m becomes a new special form:
; use (m ...) gets expanded according to definition

; example definitions (actual definitions in optional segment):
; expand (my-if e1 then e2 else e3) to (if e1 e2 e3)
; expand (comment-out e1 e2) to e2
; expand (my-delay e) to (mcons #f (lambda () e))

; a macro to replace an expression with another one
(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead) instead]))

; makes it so users don't write the thunk when using my-delay
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda () e))]))








