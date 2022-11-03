#lang simply-scheme
;; homework week 1

;; .2
(define (sqr x)
  (* x x))

(define (squares sen)
  (if (empty? sen)
      '()
      (se (sqr (first sen))
          (squares (bf sen)))))

;; .3

