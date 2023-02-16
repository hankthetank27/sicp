#lang simply-scheme
;1.8
(define (sqr x)
  (* x x))

(define (improve-cube guess x)
  (/ (+ (/ x (sqr guess))
        (* 2 guess))
     3))

(define (cube-rtr guess prev x)
  (if (in-range guess prev)
      guess
      (cube-rtr (improve-cube guess x) guess x)))

(define (cbrt x)
  (cube-rtr 1.0 x x))
