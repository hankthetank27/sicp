#lang simply-scheme

;;.1
(define x (cons 4 5))
(car x)
(cdr x)
(define y (cons 'hello 'goodbye))
(define z (cons x y))
(car (cdr z))
(cdr (cdr z))

;;2.
(cdr (car z)) ;5
(car (cons 8 3)) ;8
(car z) ;(4 . 5)

;;3.
(define (make-rational num den)
  (cons num den))
(define (numerator rat)
  (car rat))
(define (denominator rat)
  (cdr rat))
(define (*rat a b)
  (make-rational (* (numerator a) (numerator b))
                 (* (denominator a) (denominator b))))
(define (print-rat rat)
  (word (numerator rat) '/ (denominator rat)))

;;4.
(print-rat (make-rational 2 3))
(print-rat (*rat (make-rational 2 3) (make-rational 1 4)))

;;5.
(define (+rat a b)
  (make-rational (+ (numerator a) (numerator b))
                 (+ (denominator a) (denominator b))))

;;6.
;2.2
(define (make-segment-test start end)
  (lambda (x)
    (cond ((= x 0) start)
          ((= x 1) end))))
(define (start-seg-test x)
  (x 0))
(define (end-seg-test x)
  (x 1))

(define (make-seg start end)
  (cons start end))
(define (start-seg x)
  (car x))
(define (end-seg x)
  (cdr x))

(define (make-point x y)
  (cons x y))
(define (x-point x)
  (car x))
(define (y-point x)
  (cdr x))

(define (midpoint-seg line)
  (define (average x y)
    (/ (+ x y) 2))
  (make-point (average (x-point (start-seg line))
                       (y-point (start-seg line)))
              (average (x-point (end-seg line))
                       (y-point (end-seg line)))))
;2.3

