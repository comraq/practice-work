#lang racket

(provide (all-defined-out))

(define x 3)         ; val x = 3
(define y (+ x 2))   ; + is a function, calling it here

(define cube1
  (lambda (x)
    (* x (* x x))))

(define cube2
  (lambda (x)
    (* x x x)))      ; * is a function that can take multiple arguments

(define (cube3 x)    ; Syntactic sugar for lambda with 1 argument
  (* x x x))

(define (pow1 x y)   ; xo the yth power (y must be non-negative)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

; Currying example
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

(define three-to-the (pow2 3))

; Note, to called curried functions:
(define sixteen-with-pow1 (pow1 4 2))
(define sixteen-with-pow2 ((pow2 4) 2))
