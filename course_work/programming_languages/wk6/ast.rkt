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

; Variables:
;
; To implement variables, use a list of pairs (string, value) as the
; environment, where 'string' is the variable name and 'value' is the value
; stored in the variable.
;
; This environment should be passed around in the recursive 'eval-exp'
; function, to be able to lookup variable values when encountering
; expressions involving variables.

; Closures:
;
; To implement closures, create a struct with the function and the
; environment of the function. Thus later when invoked, the function stored
; in the closure can be invoked within the environment in the closure
; struct.
;
; Note:
;   - The closure and arguments to be passed to the closure should be
;     evaluted in the current environment
;   - The closure function body should be evaluated in the closure's
;     environment
;   - To allow function bodies to recursively call itself, must extend the
;     function's environment with the function's name mapping to the entire
;     closure struct
;
; Note:
;   - blindly storing all variables in the environment of a closure can be
;     expensive (space-wise), as there may be many bindings not referenced in
;     the function body of the closure but the closure environment prevents
;     garbage collection
;   - scan through all closure function bodies and only store the referenced
;     free variables of the function in its environment (this step can be
;     done before run time)
;
; Note:
;   - in a compiled language, since there is no interpreter to invoke a
;     closure function with a closure environment, the compiled/translated
;     code must "re-write" the function to take in an extra argument (an
;     argument that contains the look up information for the closure
;     environment's free variables)
;   - the extra "environment/free variables argument" must be passed to the
;     function, and the function must be re-written to lookup variable
;     bindings inside this argument
;     - every function must be re-written to take in this extra
;       "environment/free variables argument"
;     - every function call must be re-written to pass in this extra argument
;     - every function must be re-written to lookup bindings in this extra
;       argument

; Macros can just be racket functions as follows:
(define (andalso e1 e2)
  (if-then-else e1 e2 (bool #f)))

(define (double e)
  (multiply e (const 2)))

(define (list-product es)
  (if (null? es)
      (const 1)
      (multiply (car es) (list-product (cdr es)))))

(define test (andalso (eq-num (double (const 4))
                              (list-product (list (const 2)
                                                  (const 2)
                                                  (const 1)
                                                  (const 2))))
                      (bool #t)))

; Note that these "function macros" do not evaluate an expression, they are
; just syntax that construct/expand the actual expressions to be evaluated
; by 'eval-exp'.
