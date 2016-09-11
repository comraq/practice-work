#lang racket

(provide (all-defined-out))

; Delayed Evaluation:
;
; In typical languages such as ML, Racket, Java, C:
; - Function arguments are "eager" (call-by-value), that is, the function
;   arguments are evaluated before entering the function
; - Conditional branches on the other hand are not "eager"
;   ex: division by 0 in a branch that is evaluated to false will not execute
;

; Example:
(define (my-if-bad x y z)
  (if x y z))

; A call to 'factorial-bad' will never terminate because both true and
; false branches are forced to evaluate when calling 'my-if-bad' function
(define (factorial-bad n)
  (my-if-bad (= n 0)
             1
             (* n (factorial-bad (- n 1)))))

; This works because false branches to 'if' is not evaluated
(define (factorial-normal x)
  (if (= x 0)
      1
      (* x (factorial-normal (- x 1)))))

; 'e2' and 'e3' are zero-argument functions (thunks)
(define (my-if-strange-but-works e1 e2 e3)
  (if e1 (e2) (e3)))

(define (factorial-thunk x)
  (my-if-strange-but-works (= x 0)
                           (lambda () 1)
                           (lambda () (* x (factorial-thunk (- x 1))))))

; Thunks can be used to delay evaluation

(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

; multiplies 'x' and the result of 'y-thunk', callying 'y-thunk' x times
; This is slow because 'y-thunk' is re-evaluated many times
(define (my-mult x y-thunk) ;; assumes x >= 0
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

; Delay and force

; '#f' indicates whether the thunk has been evaluated once
(define (my-delay th)
  (mcons #f th))

; '#t' indicates thunk already evaluated and stored in 'mcdr'
; '#f' indicates thunk not evaluated, thus:
; - set 'mcar' as '#t'
; - set 'mcdr' as result of thunk
; - return 'mcdr'
;
; Now, all subsequent calls to the mcons cell can avoid re-evaluating the
; thunk everytime because 'mcar' is already '#t', indicating that the result
; of the thunk is already stored in 'mcdr'
(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

; Lets rewrap 'my-mult' using 'my-delay' and 'my-force'
(define (my-mult-wrapped x y-thunk)
  (let ([my-promise (my-delay y-thunk)])
    (my-mult x (lambda () (my-force my-promise)))))
