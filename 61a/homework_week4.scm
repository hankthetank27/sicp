#lang simply-scheme

;;1

;2.7
(define (make-interval lower upper)
  (cons lower upper))
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

;2.8
(define (subtract-interval x y)
  (make-interval (- (lower-bound x)(upper-bound y))
                 (- (upper-bound x)(lower-bound y))))

;2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (define (span-zero interval)
    (and (<= (lower-bound interval) 0)
         (>= (upper-bound interval) 0)))
  (if (span-zero y) (error "interval cannot span zero")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(div-interval (make-interval 1 2)
              (make-interval 4 5))
(div-interval (make-interval 1 2)
              (make-interval -4 5))