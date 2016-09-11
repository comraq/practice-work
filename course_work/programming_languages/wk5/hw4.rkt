
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative mumber")]
        [(empty? xs)   (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]
      ))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let* ([pr     (s)]
             [curr   (car pr)]
             [stream (cdr pr)])
        (cons curr (stream-for-n-steps stream (- n 1))))
    ))

(define funny-number-stream
  (letrec ([nats (lambda (n) (cons n (lambda() (nats (+ n 1)))))]
           [if-div-by-5-negate (lambda (x)
             (if (= (modulo x 5) 0)
                 (* -1 x)
                 x))]
           [funny-nats (lambda (stream)
                         (let* ([pr   (stream)]
                                [n    (if-div-by-5-negate (car pr))]
                                [ns   (cdr pr)])
                           (cons n (lambda() (funny-nats ns)))
                         ))])
  (lambda() (funny-nats (lambda() (nats 1))))))

(define dan-then-dog
  (let* ([dog "dog.jpg"]
         [dan "dan.jpg"]
         [alternate (lambda (pic) (if (eq? pic dog) dan dog))]
         [stream (lambda (pic)
                   (cons pic (lambda() (stream (alternate pic)))))]
         )
    (lambda() (stream dan))))

(define (stream-add-zero s)
  (let* ([pr     (s)]
         [curr   (car pr)]
         [stream (cdr pr)])
    (lambda() (cons (cons 0 curr) (lambda() (stream-add-zero stream)))))
  )

(define (cycle-lists xs ys)
  (letrec ([next (lambda (curr-list default-list)
                   (if (null? curr-list)
                       (cons (car default-list) (cdr default-list))
                       (cons (car curr-list) (cdr curr-list))))]
           [stream (lambda (curr-list default-list)
                     (let* ([pr   (next curr-list default-list)]
                            [head (car pr)]
                            [tail (cdr pr)])
                       (cons head (lambda() (stream tail default-list)))))]
           [xs-stream (lambda() (stream xs xs))]
           [ys-stream (lambda() (stream ys ys))]
           [zip-stream (lambda (s1 s2)
                         (let* ([pr1    (s1)]
                                [v1     (car pr1)]
                                [s1next (cdr pr1)]
                                [pr2    (s2)]
                                [v2     (car pr2)]
                                [s2next (cdr pr2)])
                           (cons (cons v1 v2)
                                 (lambda() (zip-stream s1next s2next)))))])
    (lambda() (zip-stream xs-stream ys-stream))
  ))

(define (vector-assoc v vec)
  (letrec ([car-equals (lambda (pr v) (equal? (car pr) v))]
           [check (lambda (pos vec)
                    (if (>= pos (vector-length vec))
                        #f
                        (let ([elem (vector-ref vec pos)])
                          (if (and (pair? elem) (car-equals elem v))
                            elem
                            (check (+ pos 1) vec)))
                          ))])
    (check 0 vec)
    ))

(define (cached-assoc xs n)
  (letrec ([cache-pos     0]
           [cache         (make-vector n #f)]
           [inc-cache-pos (lambda() (set! cache-pos (modulo (+ cache-pos 1) n)))]
           [lookup-cache  (lambda (v)
                            (let ([found (vector-assoc v cache)])
                              (if found
                                  found
                                  (let ([pr (assoc v xs)])
                                    (begin [save-to-cache pr]
                                           pr))
                                  )))]
           [save-to-cache (lambda (v)
                            (begin [vector-set! cache cache-pos v]
                                   [inc-cache-pos]))]
           )
    lookup-cache))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([target-num e1]
              [helper     (lambda()
                            (let ([curr-num   e2])
                              (if (< curr-num target-num)
                                (helper)
                                #t)))])
       (helper))]
    ))
