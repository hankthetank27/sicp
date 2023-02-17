#lang sicp


;; (1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough?
                            improve)(improve guess)))))


(define (fixed-point2 f first-guess)
  (define (in-range? guess)
    (< (abs (- guess (f guess))) 0.00001))
  ((iterative-improve in-range? f) first-guess))
