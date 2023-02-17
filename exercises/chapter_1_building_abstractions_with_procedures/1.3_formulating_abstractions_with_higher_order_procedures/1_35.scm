#lang sicp


;; (1.35
(define (fixed-point f first-guess)
  
  (define (in-range? x y)
    (< (abs (- x y)) 0.00001))
  
  (define (try guess)
    (let ((next (f guess)))
      (if (in-range? guess next)
          next
          (try next))))
  
  (try first-guess))

(fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0) 


;; (b. 1.35

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough?
                            improve)(improve guess)))))

(define (fixed-point f first-guess)
  (define (in-range? guess)
    (< (abs (- guess (f guess))) 0.00001))
  ((iterative-improve in-range? f) first-guess))

(fixed-point (lambda (x)(+ 1.0 (/ 1.0 x))) 1.0)