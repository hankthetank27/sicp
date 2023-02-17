#lang sicp

;; (a. opt 1.17

(define (double x) (+ x x))
(define (half x) (/ x 2))

(define (fast-mult x n)
  (cond ((= n 1) x)
        ((even? n) (double (fast-mult x (half n))))
        (else (+ x (fast-mult x (- n 1))))))
