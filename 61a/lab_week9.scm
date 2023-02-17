#lang sicp

;;1
;3.12
(define (append x y)
  (if (null? x)
      x
      (cons (car x) (append (cdr x) y))))

(define (find-last x)
  (if (null? (cdr x))
      x
      (find-last (cdr x))))

(define (append! x y)
  (set-cdr! (find-last x) y)
  x)

(define x '(a b))
(define y '(c d))
(define w (append x y))
(cdr x); (b)
(define z (append! x y))
(cdr x); (b c d)


;;2.
;Suppose that the following definitions have been provided.
(define _x (cons 1 3))
(define _y 2)
;A CS 61A student, intending to change the value of x to a pair with car equal to 1 and cdr equal to 2,
;types the expression (set! (cdr x) y) instead of (set-cdr! x y) and gets an error. Explain why.


;3a.
;Provide the arguments for the two set-cdr! operations in the blanks below to produce the indicated
;effect on list1 and list2. Do not create any new pairs; just rearrange the pointers to the existing ones.
;> (define list1 (list (list ’a) ’b))
;list1
;> (define list2 (list (list ’x) ’y))
;list2
;> (set-cdr! ________ ________ )
;okay
;> (set-cdr! ________ ________ )
;okay
;> list1
;((a x b) b)
;> list2
;((x b) y)

;3b.
;After filling in the blanks in the code above and producing the specified effect on list1 and list2,
;draw a box-and-pointer diagram that explains the effect of evaluating the expression
;(set-car! (cdr list1) (cadr list2))


;;4


;3.13


;3.14