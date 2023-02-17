#lang sicp

;; (1.43
(define (repeated fn n)
  (lambda (x)
    (define (repeat res times)
      (if (= times n)
          res
          (repeat (fn res) (+ times 1))))
    (repeat x 0)))


(define (repeated2 fn n)
  (lambda (x)
    (if (= n 0)
        x
        (fn ((repeated fn (- n 1)) x)))))
