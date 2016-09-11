#lang racket

(provide (all-defined-out))

; A stream is defined as an infinite sequence of values
; - use thunks to delay creating the sequence that's not currently needed
; - stream producer knows how to create any number of values
; - stream consumer decides how many values to ask for
;
; Examples:
; - user actions (mouse clicks and etc...)
; - unix pipes
; - output values from a sequential feedback circuit

; Let a stream be a thunk that when called, returns a pair of the following
; form:
; - (next-answer . next-thunk)
;
; Usage:
; - first:  (car (s))
; - second: (car ((cdr (s))))
; - third:  (car ((cdr ((cdr (s))))))

(define (number-until stream tester)
  (letrec ([f (lambda (stream acc)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      acc
                      (f (cdr pr) (+ acc 1)))))])
    (f stream 1)))

; 1 1 1 1 ...
(define ones (lambda () (cons 1 ones)))

; 1 2 3 4 ...
(define (f x) (cons x (lambda () (f (+ x 1)))))
(define nats (lambda () (f 1)))

(define nats-again
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; 1 2 4 8 16 ...
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define (stream-maker fn arg)
  (letrec ([f (lambda (x) (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))

(define p-of-two (stream-maker * 2))
(define nat-nums (stream-maker + 1))
