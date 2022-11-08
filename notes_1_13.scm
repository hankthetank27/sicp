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
  (+ (let ((x 1)
           (y (+ x 2)))
       (+ x y))
     x))

(define (f g)
  (g 2))

(define (fixed-point f first-guess)
  
  (define (in-range? x y)
    (< (abs (- x y)) 0.00001))
  
  (define (try guess)
    (let ((next (f guess)))
      (if (in-range? guess next)
          next
          (try next))))
  
  (try first-guess))

;; at 1.3.4 in text----