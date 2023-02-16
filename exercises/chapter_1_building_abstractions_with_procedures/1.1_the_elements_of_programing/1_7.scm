#lang simply-scheme
;1.7

(define (average x y)
  (/ (+ x y) 2))

(define (in-range guess prev)
  (< (abs (- guess prev))
     (* guess 0.001)))  

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqr-rtr guess prev x)
  (if (in-range guess prev)
      guess
      (sqr-rtr (improve guess x) guess x)))

(define (sqrt x)
  (sqr-rtr 1.0 x x))