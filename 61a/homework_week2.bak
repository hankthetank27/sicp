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

;; (1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;; (1.41
(define (double f)
  (lambda (x)
    (f (f x))))

(((double (double double)) (lambda (x) (+ x 1))) 5) ;; -> 21

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

;; 2.
(define (every fn s)
  (if (empty? s)
      '()
      (se (fn (first s))
          (every fn (bf s)))))

;; 3.
(every (lambda (letter) (word letter letter)) 'purple)

(define (keep pred? s)
  (every (lambda (x)
           (if (pred? x) x '()))
         s))

;; 4(EC).
((lambda (fn n) (fn fn n))
 (lambda (fn n)
   (if (= 0 n)
       1
       (* n (fn fn (- n 1)))))
 5)

 ;;or.. 
(((lambda (fn)
    (lambda (n)
      (fn fn n)))
  (lambda (fun x)
    (if (= x 0)
	 1
	 (* x (fun fun (- x 1))))))
 5)

  
