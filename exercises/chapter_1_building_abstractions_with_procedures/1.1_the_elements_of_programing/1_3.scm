#lang simply-scheme
;1.3

(define (square x)
  (* x x))

(define (sumofsquares x y)
  (+ (square x) (square y)))

(define (sum2lrg x y z)
  (cond ((> x y)
         (if (> y z)
             (sumofsquares x y)
             (sumofsquares x z)))
        ((< x y)
         (if (> x z)
             (sumofsquares y x)
             (sumofsquares y z)))))