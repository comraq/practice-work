#lang racket
(provide (all-defined-out))

; A parser first parses the raw string for syntax errors then constructs an
; "Abstract Syntax Tree" (AST)

; Fundamentally, there are two approaches to implementing a programming
; language "B":
;
; - Write an "interpreter" for "B" in another language "A"
;   ie: take a program in "B" and produce an answer in 'B"
;
; - Write a "compiler" for "B" in another language "A" and compile/translate
;   it to a different language "C" (usually binary code)
;   - Note: The translation must preserve meaning (equivalence)
;
; Note: the language "A" in this case is usually called the "metalanguage"
;
; Note: Modern languages may make use of both an "interpreter" and a
;       "compiler"

; A language is defined by a set of rules for the different symantics and
; constructs present in the language. Whether it is implemented via an
; "interpreter", "compiler" or both is an implementation detail, not a part
; of the language definition.
;   - Thus technically, there is no such thing as a "compiled language" or
;     an "interpreted language"

; The "parsing" step of implementing programming language "B" can be skipped
; if "B" programmers directly write the "AST"s in language "A"
; ie: embed "B" programs as trees in "A"

; Workflow:
; 1) Define (abstract) syntax of "B" using constructs in "A"
; 2) Write "B" programs directly with the permitted syntax
; 3) Implement interpreter for "B" as a (recrusive) function
;
; Assumptions:
; - interpreter can assume input as a legal "AST" for "B"
;   - if not, can error/crash
; - interpreter cannot assume that recursive results are correct, but must
;   explicitly check that the values are correct
;   - if not, give an appropriate error message

(struct const    (int)   #:transparent)
(struct negate   (e)     #:transparent)
(struct add      (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

; Example for the above struct:
; - ok to assume that:
;   - 'const' contains a number
;   - 'negate' contains a legal "AST"
;   - both 'add' and 'multiply' contain 2 legal "AST"s
;   - otherwise, unhandled error/crash is tolerable

; Results of the Interpreter:
; - Interpreters should always return expressions that are values (an
;   expression that evaluates to itself)
;   - in the above example, values are only expressions of type 'const'

(struct bool         (b)        #:transparent)
(struct eq-num       (e1 e2)    #:transparent)
(struct if-then-else (e1 e2 e3) #:transparent)

; If "AST" is legal but evaluation ends up using the wrong kind of value
; (ie: "add a boolean"), then interpreter should detect this and give an
; useful/handled error message

; Should evaluate fine
(define test1 (multiply (negate (add (const 2) (const 2)))
                        (const 7)))

; Interpreter should report the bad value error
(define test2 (multiply (negate (add (const 2) (const 2)))
                        (if-then-else (bool #f)
                                      (const 7)
                                      (bool #t))))

; Interpreter can crash due to invalid "AST"
(define non-test (multiply (negate (add (const #t)
                                        (const 2)))
                           (const 7)))

(define (eval-exp-wrong e)
  (cond [(const? e) e]

        [(negate? e)
         (const (- (const-int (eval-exp-wrong (negate-e e)))))]

        [(add? e)
         (let ([i1 (const-int (eval-exp-wrong (add-e1 e)))]
               [i2 (const-int (eval-exp-wrong (add-e2 e)))])
           (const (+ i1 i2)))]

        [(multiply? e)
         (let ([i1 (const-int (eval-exp-wrong (multiply-e1 e)))]
               [i2 (const-int (eval-exp-wrong (multiply-e2 e)))])
           (const (* i1 i2)))]

        [(bool? e) e]

        ; evals both sub expressions as ints and equates them,
        ; creating a 'bool' expression
        [(eq-num? e)
         (let ([i1 (const-int (eval-exp-wrong (eq-num-e1 e)))]
               [i2 (const-int (eval-exp-wrong (eq-num-e2 e)))])
           (bool (= i1 i2)))]

        [(if-then-else? e)
         (if (bool-b (eval-exp-wrong (if-then-else-e1 e)))
             (eval-exp-wrong (if-then-else-e2 e))
             (eval-exp-wrong (if-then-else-e3 e)))]

        [#t (error "eval-exp expected and exp")]
        ))

(define (eval-exp e)
  (cond [(const? e) e]

        [(negate? e)
         (let ([v (eval-exp (negate-e e))])
           (if (const? v)
               (const (- (const-int v)))
               (error "negate applied to non-number")))]

        [(add? e)
         (let ([v1 (eval-exp (add-e1 e))]
               [v2 (eval-exp (add-e2 e))])
           (if (and (const? v1) (const? v2))
               (const (+ (const-int v1) (const-int v2)))
               (error "add applied to non-number")))]

        [(multiply? e)
         (let ([v1 (eval-exp (multiply-e1 e))]
               [v2 (eval-exp (multiply-e2 e))])
           (if (and (const? v1) (const? v2))
               (const (* (const-int v1) (const-int v2)))
               (error "multiply applied to non-number")))]

        [(bool? e) e]

        [(eq-num? e)
         (let ([v1 (eval-exp (eq-num-e1 e))]
               [v2 (eval-exp (eq-num-e2 e))])
           (if (and (const? v1) (const? v2))
               (bool (= (const-int v1) (const-int v2)))
               (error "eq-num applied to non-number")))]

        [(if-then-else? e)
         (let ([v-test (eval-exp (if-then-else-e1 e))])
           (if (bool? v-test)
               (if (bool-b v-test)
                   (eval-exp (if-then-else-e2 e))
                   (eval-exp (if-then-else-e3 e)))
               (error "if-then-else applied to non-boolean")))]

        [#t (error "eval-exp expected an exp")]
        ))
