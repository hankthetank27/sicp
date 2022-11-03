#lang simply-scheme

(define (abs x)
  (if (< x 0) (- x) x))

(define (sqr x)
  (* x x))

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

