#lang racket

(provide (all-defined-out))

; Note: 'error' will stop execution of function when encountered
(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))]) ; bind 'tlans' to max of tail
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))

; Racket as the following 4 ways to define local variables/bindings:
; - let
;   - can bind any number of local variables
;   - the expressions within each let expression is evaluated "before" the
;     let expression
;
;     ie: (let ([x (+ x 3)]
;               [y (+ x 2)])
;           body...)
;         - both 'x' and 'y' are bound to the sum of 'x' that is defined
;           before the let expression with '3' and '2' respectively
;
;     ex: swapping the bindings of 'x' and 'y' within the body of a let
;         expression
;         (let ([x y] [y x]) body...)
;
; - let*
;   - same as 'let' except expressions to bind are evaluated in the
;     environment produced from previous bindings (ie: repeated bindings
;     shadow the bindings occured before)
;
; - letrec
;   - same as 'let*' except expressions to bind are evaluated in an
;     environment that includes "all" bindings (including both previous and
;     after)
;   - Note: Expressions to bind are still "evaluated in order". That is,
;     accessing uninitialized bindings would produce and error
;
;     ex: (letrec ([y (+ x 2)]
;                  [f (lambda (z) (+ z y w x))]
;                  [w (+ x y)])
;            (f -9))
;         - function 'f' will sum its argument 'z' with 'y', 'w' from the
;           let bindings, as well as 'x' which should be defined outside of
;           the 'letrec' expression
;   - this allows mutually recursive functions, since function bodies are
;     not evaluated until called, these function bodies defined in 'letrec'
;     are perfectly safe to refer to bindings that occur before or after in
;     the 'letrec' expression
;     ie: functions 'f' and 'g' mutually calls each other
;
;     ex: (define (silly-mod2 x)
;           (letrec
;             ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
;              [odd?  (lambda (x) (if (zero? x) #f (even? (- x 1))))])
;                (if (even? x) 0 1)))
;
; - define
;   - used as we have seen before, same semantics as 'letrec'
;
;     ex: (define (silly-mod2 x)
;           (define (even? x) (if (zero? x) #t (odd? (- x 1))))
;           (define (odd? x)  (if (zero? x) #f (even? (- x 1))))
;           (if (even? x) 0 1))

; Top-level (module) bindings are similar to 'letrec'
(define (f x) (+ x (* x b))) ; forward reference okay here
(define b 3)
(define c (+ b 4)) ; backward reference okay
;(define d (+ e 4)) ; not okay (get an error)
(define e 5)
;(define f 17) ; not okay: f already defined in this module
