#lang simply-scheme

;; 1

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

;; (a. opt 1.17

(define (double x) (+ x x))
(define (half x) (/ x 2))

(define (fast-mult x n)
  (cond ((= n 1) x)
        ((even? n) (double (fast-mult x (half n))))
        (else (+ x (fast-mult x (- n 1))))))

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

;; (c. 1.37

(define (cont-frac n d k)
  (if (= k 0)
      (/ (n k) (d k))
      (/ (n k)
         (+ (d k) (cont-frac n d (- k 1))))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

(define (cont-frac-iter n d k)
  (define (iter term res)
    (if (= term 0)
        res
        (iter (- term 1)
              (/ (n term)
                 (+ (d term) res)))))
  (iter k 1))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                100)

;; (d. 1.38

(define (e-euler k)
  (+ 2.0 (cont-frac (lambda (x) 1)
                     (lambda (x)
                       (if (= (remainder x 3) 2)
                           (/ (+ x 1) 1.5)
                           1))
                       k)))

;; .2

(define (next-perf x)
  (define (sum-of-factors x i sum)
    (if (= i x)
        sum
        (if ( = (remainder x i) 0)
            (sum-of-factors x (+ i 1) (+ sum i))
            (sum-of-factors x (+ i 1) sum))))
  (if (= (sum-of-factors x 1 0) x)
      x
      (next-perf (+ 1 x))))




  