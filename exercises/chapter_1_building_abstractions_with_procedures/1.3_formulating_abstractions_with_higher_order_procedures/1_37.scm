#lang sicp

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