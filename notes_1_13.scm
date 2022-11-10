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

(define (average x y)
  (/ (+ x y) 2.0))

(define (fixed-point f first-guess)
  
  (define (in-range? x y)
    (< (abs (- x y)) 0.00001))
  
  (define (try guess)
    (let ((next (f guess)))
      (if (in-range? guess next)
          next
          (try next))))
  
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
                 1.0))

;; at 1.3.4 (newtowns method) in text----
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx))(g x))
       dx)))

(define (newtons-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))


(define (sqrt2 x)
  (newtons-method (lambda(y) (- (* y y) x))
                  1.0))






    