#|
A "term" is either:

 - an "atom"
   ex: #t, #f, 34, "hi", null, 4.0, x and ...

 - a "special form"
   ex: define, lambda, if, macros and etc...

 - A "sequence" of terms in parens
   ex: (t1 t2 ... tn)
     - if 't1' is a special form, semantics of sequence is special
     - otherwise, the sequence becomes a function call

 - examples:
   (+ 3 (car xs))              ; a sequence with three terms
   (lambda (x) (if x "hi" #t)) ; a sequence with three terms
 |#

#|
Why parentheses?

By parenthesizing everything, converting the program text into a tree
representing the program (parsing) is trivial and unambiguous
- Atoms are leaves
- Sequences are nodes with elements as children
- No other rules (no complications such as "operator precedence")
- Indentation is now also simple
 |#

#|
Parentheses Matter.

- In most places, (e) means call 'e' with zero arguments
- ((e)) means call 'e' with zero arguments, and call that result with zero
  arguments
 |#

#lang racket

(provide (all-defined-out))

(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

; Incorrect as the constant '1' is treated as a procedure to call
(define (fact1 n) (if (= n 0) (1) (* n (fact1 (- n 1)))))

; This works as long as the argument is not 0
(define (fact1b n) (if (= n 0) (1) (* n (fact (- n 1)))))

; Syntax error:
; > if: bad syntax (has 5 parts after keyword)
;
; (define (fact2 n) (if = n 0 1 (* n (fact2 (- n 1)))))

; Syntax error:
; > define: bad syntax (multiple expressions after identifier
;
; (define fact3 (n) (if (= n 0) 1 (* n (fact3 (- n 1)))))

; The function 'fact4' is passed as an argument to the function '*'
(define (fact4 n) (if (= n 0) 1 (* n fact4 (- n 1))))

; Error! The extra parentheses around fact5 calls fact 5 with 0 arguments
(define (fact5 n) (if (= n 0) 1 (* n ((fact5) (- n 1)))))

; Error! Attempts to call 'n' as a function
(define (fact6 n) (if (= n 0) 1 (n * (fact6 (- n 1)))))

