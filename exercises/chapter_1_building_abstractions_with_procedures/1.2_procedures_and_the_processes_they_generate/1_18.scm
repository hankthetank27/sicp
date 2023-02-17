#lang sicp

;; (a opt 1.18

(define (fast-mult-iter x n)
  (define (iter x n prod)
    (cond ((= n 0) prod)
          ((even? n) (iter (double x)
                           (half n)
                           prod))
          (else (iter x
                      (- n 1)
                      (+ x prod)))))
  (iter x n 0))