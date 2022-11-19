#lang simply-scheme

(define (factorial x)
  (fact-iter 1 1 x))

(define (fact-iter product count max)
  (if (= count max)
      product
      (fact-iter (* product count)
                 (+ 1 count)
                 max)))

;;fib iter
(define (fib n)
  ((lambda (fn a b count)
     (fn fn a b count))
   (lambda (fn a b count)
     (if (= count 0)
         b
         (fn fn (+ a b) a (- count 1))))
   1 1 n))

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))