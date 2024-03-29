#lang sicp

;; (a. 1.16

(define (square x) (* x x))

(define (fast-expt b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b)
                           (/ n 2)
                           a))
          (else (iter b
                      (- n 1)
                      (* a b)))))
  (iter b n 1))