#lang simply-scheme

(define (factorial x)
  (fact-iter 1 1 x))

(define (fact-iter product count max)
  (if (= count max)
      product
      (fact-iter (* product count)
                 (+ 1 count)
                 max)))

