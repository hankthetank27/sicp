#lang sicp

;; (1.33
(define (filter-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filter-accumulate filter combiner null-value term a next b))))

(define (prime? x)
  (define (check a b)
    (cond ((= a b) #t) 
          ((= (remainder x a) 0) #f)
          (else (check (+ 1 a) b))))
  (if (< 1 x)
      (check 2 x)
      #f))

(define (sum-non-prime-squares a b)
  (filter-accumulate
   prime?
   +
   0
   (lambda (x)
     (* x x))
   a
   (lambda (x)
     (+ 1 x))
   b))