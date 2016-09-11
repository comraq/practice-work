#lang racket

(provide (all-defined-out))

; 'cons' actually makes a pair, not a list. (called "cons cell")
; In fact, a list is reallly just nested pairs that eventually end with 'null'.
;
; Hence, 'car' get the first element of a pair, 'cdr' gets the second.

(define pr (cons 1 (cons #t "hi"))) ; Note: no 'null' at the end of list
; pr = (1 #t . "hi")

(define lst (cons 1 (cons #t (cons "hi" null)))) ; Note: 'null' at the end
; lst = (1 #t "hi")

(define hi (cdr (cdr pr))) ; hi = "hi"
(define hi-again (car (cdr (cdr lst)))) ; hi-again = "hi"
; Note: (cdr (cdr lst)) = ("hi"), a list with single element

; (define (caddr x) (car (cdr (cdr x))))
(define hi-again-shorter (caddr lst))

(define no (list? pr))                           ; #f
(define yes (pair? pr))                          ; #t
(define of-course (and (list? lst) (pair? lst))) ; #t

; (list? pr)                   = #f
; (list? null)                 = #t
; (list? (cons null))          = #t

; (pair? pr)                   = #t
; (and (list? lst) (pair? pr)) = # t

; (length lst) = 3
; (length pr)  -> error

; Note: cons not terminated by 'null' are sometimes called improper lists,
;       while those terminated by 'null' are called proper lists

; Typical usage of lists vs pairs:
; - use proper lists for collections of unknown size
; - use pairs for those with known/fixed sizes (though structs may be better)
