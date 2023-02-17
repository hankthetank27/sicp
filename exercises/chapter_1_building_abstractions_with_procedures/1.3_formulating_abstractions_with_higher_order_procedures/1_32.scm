#lang sicp

;; (1.32 (a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(accumulate + 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
(accumulate * 1 (lambda (x) x) 1 (lambda (x) (+ 1 x)) 10)