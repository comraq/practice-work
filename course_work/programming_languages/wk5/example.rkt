#lang racket

(provide (all-defined-out))

(define s "hello")

; Cond expressions, much like case expressions in other languages
;
; Note: The last branch of 'cond' should always be true (#t) to catch all
;       conditions
(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))

(define (sum4 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum4 (cdr xs)))]
        [(list? (car xs)) (+ (sum4 (car xs)) (sum4 (cdr xs)))]
        [#t (sum4 (cdr xs))]))

; Note: The first/test expression in 'if' or 'cond' can be any type of
;       value, not just booleans. However, only '#f' counts as false, all
;       other values are treated as true.
;
; Example:
;   >(if 34 14 15)
;   > 14
;
;   >(if null 14 15)
;   > 14
;
;   >(if #f 14 15)
;   > 15

(define (count-falses xs)
  (cond [(null? xs) 0]                       ; Empty list -> return 0
        [(car xs) (count-falses (cdr xs))]   ; Head of list not false, recurse
        [#t (+ 1 (count-falses (cdr xs)))])) ; Otherwise, +1 to result
