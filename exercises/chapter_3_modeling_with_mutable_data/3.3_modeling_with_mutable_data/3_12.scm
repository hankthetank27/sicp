#lang sicp

;3.12
(define (append x y)
  (if (null? x)
      x
      (cons (car x) (append (cdr x) y))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x '(a b))
(define y '(c d))
(define w (append x y))
(cdr x); (b)
(define z (append! x y))
(cdr x); (b c d)
