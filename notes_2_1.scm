#lang simply-scheme


(define x (cons 1 2))
(display x)
(newline)
(car x)
(cdr x)

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (demon x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (demon x))
  (newline))