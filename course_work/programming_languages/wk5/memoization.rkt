#lang racket

(provide (all-defined-out))

; If a function has no side effects and does not read mutable memory, no
; point in computing it twice with the same arguments
; - can keep a cache of previous result
; - net gain if:
;   - maintaining cache is cheaper than recomputing
;   - cached results are reused
;
; "Memoization" is similar to promises, but if the function takes arguments,
; then there are multiple "previous results" (as the function can return
; different results depending on its input arguments)
;
; For recursive functions, "memoization" can lead to exponentially faster
; programs
; - This is related to the algorithmic technique of dynamic programming

(define (fibonacci1 x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci1 (- x 1))
         (fibonacci1 (- x 2)))))

(define (fibonacci2 x)
  (letrec ([f (lambda (acc1 acc2 y)
                (if (= y x)
                    (+ acc1 acc2)
                    (f acc2 (+ acc1 acc2) (+ y 1))))])

    (if (or (= x 1) (= x 2))
        1
        (f 1 1 3))))

; Note: 'assoc' returns the kv pair (a cons cell) of an 'association list',
;       false if value is not found as the 'car' of a pair
(define fibonacci3
  (letrec ([memo null] ; build an 'association list' of (arg . result)
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1) (= x 2))
                                         1
                                         (+ (f (- x 1))
                                            (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))
