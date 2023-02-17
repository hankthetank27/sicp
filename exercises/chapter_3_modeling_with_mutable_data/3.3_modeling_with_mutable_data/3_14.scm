#lang sicp


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
