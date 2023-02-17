#lang sicp

;3.13
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define _z (make-cycle '(a b c)))

; (last-pair _z) causes infinte recursion. the cdr of the last pair of the input to make-cycle in the
; definition of _z points to the last pair to the input, making it a cyclical list.

