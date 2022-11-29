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

(define (make-list n)
  (if (= 0 n)
      '()
      (cons n (make-list (- n 1)))))

(define y (make-list 10))

(define (length list)
  (if (null? list)
      0
      (+ 1 (length (cdr list)))))

(define (rev-list list)
  (if (null? list)
      list
      (append (rev-list (cdr list)) (cdr list))))

(rev-list y)