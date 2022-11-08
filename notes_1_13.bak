#lang simply-scheme


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x)
  (+ 1 x))

(define (square x) (* x x))

(define (id x) x)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (fn x)
  ((lambda (a b c d)
     (+ x a b c d))
   2
   2
   2
   2))

(define (fn2 x)
  (let ((a 2)
        (b 2)
        (c 2)
        (d 2))
    (+ x a b c d)))

(define (fn3 x)
  (+ (let ((x 10)
           (y 12)
       (+ x 5 y))
     x))