#lang sicp

;;1
;3.12
(define (append x y)
  (if (null? x)
      x
      (cons (car x) (append (cdr x) y))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
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

;because (cdr x) returns a pair, and set! changes bindings, not pointers in compound data types

;3a.
;Provide the arguments for the two set-cdr! operations in the blanks below to produce the indicated
;effect on list1 and list2. Do not create any new pairs; just rearrange the pointers to the existing ones.
(define list1 (list (list 'a) 'b))
(define list2 (list (list 'x) 'y))
;> (set-cdr! ________ ________ )
(set-cdr! (car list2) (cdr list1))
;> (set-cdr! ________ ________ )
(set-cdr! (car list1) (car list2))
list1 ;((a x b) b)
list2 ;((x b) y)

;3b.
;After filling in the blanks in the code above and producing the specified effect on list1 and list2,
;draw a box-and-pointer diagram that explains the effect of evaluating the expression
(set-car! (cdr list1) (cadr list2))
list1 ;((a x y) y)

;;4
;3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define _z (make-cycle (list 'a 'b 'c)))

; (last-pair _z) causes infinte recursion. the cdr of the last pair of the input to make-cycle in the
; definition of _z points to the last pair to the input, making it a cyclical list.


;3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
; mystery will reduce in the input list to its first element, and return a reversed copy of the input.

(define v '(a b c d))
(define u (mystery v))
v
u


