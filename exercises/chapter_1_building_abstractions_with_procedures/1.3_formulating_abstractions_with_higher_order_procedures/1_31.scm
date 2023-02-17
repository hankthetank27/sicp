#lang sicp

;; (1.31 (a)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial x)
  (define (id x) x)
  (define (inc x) (+ x 1))
  (product id 1 inc x))

(define (pi-prod term)
  (* 4 (product (lambda (n)
                  (if (even? n)
                      (/ (+ n 2.0) (+ n 1.0))
                      (/ (+ n 1.0) (+ n 2.0))))
                  1
                  (lambda (n)
                    (+ 1 n))
                  term)))
