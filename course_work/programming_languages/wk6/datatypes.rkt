; No need to have a 'sum' type when parameterizing over different types

#lang racket
(provide (all-defined-out))

; Should have default true clause for 'cond' for better style
(define (funny-sum xs)
  (cond [(null? xs)         0]
        [(number? (car xs)) (+ (car xs) (funny-sum (cdr xs)))]
        [(string? (car xs)) (+ (string-length (car xs))
                               (funny-sum (cdr xs)))]))

; same idea as the following sml:
; datatype exp = Const    of int
;              | Negate   of exp
;              | Add      of exp * exp
;              | Multiply of exp * exp
(define (Const    i    ) (list 'Const    i    ))
(define (Negate   e    ) (list 'Negate   e    ))
(define (Add      e1 e2) (list 'Add      e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))

; Note that ' defines a symbol, can be compared with 'eq?'

; Helper functions for testing the 'constructors'
(define (Const?    x) (eq? (car x) 'Const   ))
(define (Negate?   x) (eq? (car x) 'Negate  ))
(define (Add?      x) (eq? (car x) 'Add     ))
(define (Multiply? x) (eq? (car x) 'Multiply))

; Helper functions for extracting out elements from the 'exp' type
(define (Const-int   e) (car (cdr e)))        ; cadr
(define (Negate-e    e) (car (cdr e)))        ; cadr
(define (Add-e1      e) (car (cdr e)))        ; cadr
(define (Add-e2      e) (car (cdr (cdr e))))  ; caddr
(define (Multiply-e1 e) (car (cdr e)))        ; cadr
(define (Multiply-e2 e) (car (cdr (cdr e))))  ; caddr

(define (eval-exp e)
  (cond [(Const?    e) e]
        [(Negate?   e) (Const (- (Const-int (eval-exp (Negate-e e)))))]
        [(Add?      e) (let ([v1 (Const-int (eval-exp (Add-e1 e)))]
                             [v2 (Const-int (eval-exp (Add-e2 e)))])
                         (Const (+ v1 v2)))]
        [(Multiply? e) (let ([v1 (Const-int (eval-exp (Multiply-e1 e)))]
                             [v2 (Const-int (eval-exp (Multiply-e2 e)))])
                         (Const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))
