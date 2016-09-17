;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))
      ))

(define (mupllist->racketlist es)
  (if (aunit? es)
      null
      (cons (apair-e1 es) (mupllist->racketlist (apair-e2 es)))
      ))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here

        [(int? e) e]

        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env)
                   )
               (error "e1 or e2 of ifgreater is non-number"))
           )]

        [(mlet? e)
         (let* ([binding  (cons (mlet-var e) (eval-under-env (mlet-e e) env))]
                [temp-env (cons binding env)])
           (eval-under-env (mlet-body e) temp-env))]

        [(fun? e) (closure env e)]
        [(closure? e) e]

        [(call? e)
         (let ([closure (eval-under-env (call-funexp e) env)]
               [arg     (eval-under-env (call-actual e) env)])
           (if (closure? closure)
               (let* ([cl-fun      (closure-fun closure)]
                      [cl-fun-body (fun-body cl-fun)]
                      [cl-funarg   (cons (fun-formal cl-fun) arg)]
                      [cl-env      (cons cl-funarg
                                         (append (closure-env closure) env))]
                      [cl-funname  (fun-nameopt cl-fun)]
                      [final-env   (if cl-funname
                                       (cons (cons cl-funname
                                                   (eval-under-env cl-fun
                                                                   env))
                                             cl-env)
                                       cl-env)])
                 (eval-under-env cl-fun-body final-env))
               (error (format
                        "funexp of call is not evaluated to a closure: ~v"
                        closure))))]

        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]

        [(fst? e)
         (let ([apr (eval-under-env (fst-e e) env)])
           (if (apair? apr)
               (apair-e1 apr)
               (error (format "MUPL fst not applied to apair: ~v" apr)))
           )]

        [(snd? e)
         (let ([apr (eval-under-env (snd-e e) env)])
           (if (apair? apr)
               (apair-e2 apr)
               (error "MUPL snd not applied to apair"))
           )]

        [(isaunit? e)
         (let ([value (eval-under-env (isaunit-e e) env)])
           (if (aunit? value)
               (int 1)
               (int 0)))]

        [(aunit? e) e]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let* ([curr      (car lstlst)]
             [tail      (cdr lstlst)]
             [var-name  (car curr)]
             [var-value (cdr curr)])
        (mlet var-name var-value (mlet* tail e2)))))

(define (ifeq e1 e2 e3 e4)
  (let ([bindings (list (cons "_x" e1)
                        (cons "_y" e2))]
        [exp-body (ifgreater (var "_x") (var "_y")
                             e4
                             (ifgreater (var "_y") (var "_x")
                                        e4
                                        e3))])
    (mlet* bindings exp-body)
    ))

;; Problem 4

(define mupl-map
  (let* ([body (fun "mupl-map-body" "apairs"
                    (ifgreater (isaunit (var "apairs")) (int 0)
                               (aunit)
                               (apair (call (var "mupl-map-func")
                                            (fst (var "apairs")))
                                      (call (var "mupl-map-body")
                                            (snd (var "apairs"))))))]
         [func (fun #f "mupl-map-func" body)])
    func))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "intN"
             (call (var "map")
                   (fun #f "x" (add (var "intN") (var "x")))
                   ))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
