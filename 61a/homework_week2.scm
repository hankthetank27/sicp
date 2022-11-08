#lang simply-scheme


;; .1
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

;; (1.32 (a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(accumulate + 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
(accumulate * 1 (lambda (x) x) 1 (lambda (x) (+ 1 x)) 10)

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

;; (1.40
