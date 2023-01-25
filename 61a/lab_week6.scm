#lang simply-scheme

;2.62
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((= (car s1)(car s2))
         (cons (car s1) (union-set (cdr s1)(cdr s2))))
        ((< (car s1)(car s2))
         (cons (car s1) (union-set (cdr s1) s2)))
        ((> (car s1)(car s2))
         (cons (car s2) (union-set s1 (cdr s2))))))

(define l1 (list 1 4 8 9))
(define l2 (list 2 7 9 10))
(define l3 (list 5 8))

(union-set l1 l2)
(union-set l1 l3)
(union-set l2 l3)