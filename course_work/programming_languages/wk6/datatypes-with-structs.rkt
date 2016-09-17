#lang racket
(provide (all-defined-out))

; Example
(struct foo (bar baz quux) #:transparent)

; The above struct definition defines the following functions:
; - (foo e1 e2 e3)
;   - returns a "foo" with 'bar', 'baz' and 'quux' fields holding results of
;     evaluating 'e1', 'e2' and 'e3'
;
; - (foo? e)
;   - evaluates 'e' and returns '#t' if and only if the result of 'e' is
;     something that was constructed with the 'foo' function
;
; - (foo-bar e)
;   - evaluates 'e'. If the result was made with the 'foo' function, return
;     the contents of the 'bar' field, else an error
;
; - (foo-baz e)
;   - evaluates 'e'. If the result was made with the 'foo' function, return
;     the contents of the 'baz' field, else an error
;
; - (foo-quux e)
;   - evaluates 'e'. If the result was made with the 'foo' function, return
;     the contents of the 'quux' field, else an error

; The same example in 'datatypes.rkt' but now defined using 'struct'
(struct const    (int)   #:transparent)
(struct negate   (e)     #:transparent)
(struct add      (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

(define (eval-exp e)
  (cond [(const?    e) e]
        [(negate?   e) (const (- (const-int (eval-exp (negate-e e)))))]
        [(add?      e) (let ([v1 (const-int (eval-exp (add-e1 e)))]
                             [v2 (const-int (eval-exp (add-e2 e)))])
                         (const (+ v1 v2)))]
        [(multiply? e) (let ([v1 (const-int (eval-exp (multiply-e1 e)))]
                             [v2 (const-int (eval-exp (multiply-e2 e)))])
                         (const (* v1 v2)))]
        [#t            (error "eval-exp expected an exp")]))

(define a-test (eval-exp (multiply (negate (add (const 2) (const 2)))
                                   (const 7))))

; Note: '#:transparent' allows printing of structs to print the contents

; Note: '#:mutable' can also be used to provide mutation functions
;
; In the following example, 'set-card-suit!' and 'set-card-rank!'
; are also defined for the struct 'card'
(struct card (suit rank) #:transparent #:mutable)
