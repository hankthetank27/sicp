#lang sicp

(define size 2)
(* size 2)

(define (square x)
  (* x x))

(define (sumofsquares x y)
  (+ (square x) (square y)))

(define (f a)
  (sumofsquares (+ a 1)(* a 2)))


(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(abs -10)
(abs 0)
(abs 5)


(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))
(abs2 -10)
(abs2 0)
(abs2 5)


(define (abs3 x)
  (if (< x 0)
      (- x)
      x))
(abs3 -10)
(abs3 0)
(abs3 5)


(define (>= x y)
  (and (> x y)(= x y)))


(/ (+ 5
      4
      (- 2(- 3(+ 6(/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

(define (sum2lrg x y z)
  (cond ((> x y)
         (if (> y z)
             (sumofsquares x y)
             (sumofsquares x z)))
        ((< x y)
         (if (> x z)
             (sumofsquares y x)
             (sumofsquares y z)))))

(sum2lrg 3 2 1)
(sum2lrg 1 2 3)
(sum2lrg 2 1 3)
(sum2lrg 3 1 2)


