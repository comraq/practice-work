#lang racket

(provide (all-defined-out))

; Data constructed from 'cons' are immutable (unlike Scheme).
; Even using 'set!', lists constructed by 'cons' are still immutable.
(define x (cons 14 null))
(define y x)

(set! x (cons 42 null))
; x = (42)
; y = (14)

; Note: (set! (car x) "hi") -> error
; 'set-car!' exists in Scheme, but no in Racket.

; Mutable lists are constructed by 'mcons'. 'mcar' and 'mcdr' are used to
; access the first and second element of an 'mcons' cell.
(define mpr (mcons 1 (mcons #t "hi")))

(define one (mcar mpr))              ; 1
(define bool-true (mcar (mcdr mpr))) ; #t

; Note: (car mpr) -> error

; 'set-mcdr!' and 'set-mcar!' exists
(set-mcar! mpr 2) ; mpr = (mcons 2 (mcons #t "hi"))
(set-mcar! (mcdr mpr) 14) ; mpr = (mcons 2 (mcons 14 "hi"))

; Note: (length (mcons #t null)) -> error!

; 'mpair?' can be used to check for 'mcons' cells
