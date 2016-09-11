#lang racket

(provide (all-defined-out))

(define b 3)
(define f (lambda (x) (* 1 (+ x b))))
(define c (+ b 4)) ; 7
(set! b 5)
(define z (f 4))   ; 9
(define w c)       ; 9

; To protect against unpredicatable future mutations, make a local copy of a
; captured variable as follows:
(define f-copied
  (let ([b b])
    (lambda (x) (* 1 (+ x b)))))

; Note: Racket's module system prevents mutation against bindings that are
;       defined outside of the current module.
;       ie: cannot mutate pre-defined functions such as '+', '*' and etc...

; A sequence of expressions can be performed via 'begin'
(define x (begin
            (+ 5 3)
            (* 3 8)
            (- 3 2)))

; x is defined to be 1, return values of expressions before the last in
; a 'begin' clause are discarded
