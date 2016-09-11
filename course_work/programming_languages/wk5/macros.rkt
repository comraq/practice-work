#lang racket

(provide (all-defined-out))

; A "macro definition" describes how to transform some new syntax into
; different syntax in the source language

; A macro is one way to implement syntactic sugar
; - "Replace any syntax of the form e1 andalse e2 with if e1 then e2 else
;    false"

; A "macro system" is a language (or part of a larger language) for defining
; macros

; "Macro expansion" is the process of rewriting the syntax for each macro
; use
; - macros are expanded before a program is ran (or even compiled)

; Macros often deserve a bad reputation due to overuse or misuse. If in
; doubt, perhaps resist defining a macro.

; Macro systems generally work at the level of tokens, not sequence of
; characters
;
; After expansion, the expanded macros are usually surrounded by
; parentheses.
;
; Macro variables that shadow other variables are scoped properly.

(define-syntax my-if
  (syntax-rules (then else)     ; defines 'then' and 'else' as keywords
    [(my-if e1 then e2 else e3) ; the 'pattern' to match
     (if e1 e2 e3)]))

; (my-if foo then bar else baz) -> (if foo bar baz)

(define-syntax comment-out
  (syntax-rules ()                ; no reserved keywords
    [(comment-out ignore instead) ; the 'pattern' to match
     instead]))

(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda() e))]))

; Both versions of 'dbl' are ok as functions
(define (dbl1 x) (+ x x))
(define (dbl2 x) (* 2 x))

; Macro versions of 'dbl' are different.
;
; ex: if the expression to be replaced contains side effects, the two macros
;     below when expanded leads to different results
;
;     - (dblm1 (begin (print "hi") 42))
;     - (dblm2 (begin (print "hi") 42))
(define-syntax dblm1 (syntax-rules() [(dblm1 x) (+ x x)]))
(define-syntax dblm2 (syntax-rules() [(dblm2 x) (* 2 x)]))

; Note that the expression 'x' is only evaluated once in this macro
(define-syntax dblm3
  (syntax-rules ()
    [(dbl x)
     (let ([y x]) (+ y y))]))

; Note that the expanded macro will evaluate 'e2' before 'e1', which may
; surprise the caller as the macro is used by passing in 'e1' before 'e2'.
; - This can be fixed by introducing local bindings
(define-syntax take
  (syntax-rules (from)
    [(take e1 from e2)
     (- e2 e1)]))

; "Hygienic" macros
;
; Expanded macros in Racket which contains local bindings do not shadow
; the bindings in the expanded scope (even if the names of the bindings are
; the same)
;
; To achieve the hygienic macro system:
; - secretly renames local variables in macros with fresh names
; - looks up variables used in macros where the macro is defined

; 'lo' and 'hi' are only evaluated once
; 'body' is evaluated everytime the loop body is entered
(define-syntax for
  (syntax-rules (to do)
    [(for lo to hi do body)
     (let ([l lo]
           [h hi])
       (letrec ([loop (lambda (it)
                        (if (> it h)
                            #t
                            (begin body (loop (+ it 1)))))])
         (loop l)))]))

; A macro with multiple 'pattern' use cases to match
(define-syntax let2
  (syntax-rules ()
    [(let2 () body)
     body]
    [(let2 (var val) body)
     (let ([var val]) body)]
    [(let2 (var1 val1 var2 val2) body)
     (let ([var1 val1])
       (let ([var2 val2])
         body))]))

; Recursive macros
; '...' means match 1 or more of the previous expression
(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body)
     body]
    [(my-let* ([var0 val0]
               [var-rest val-rest] ...)
              body)
     (let ([var0 val0])
       (my-let* ([var-rest val-rest] ...)
                body))]))

