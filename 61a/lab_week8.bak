#lang simply-scheme

(define (make-account bal)
  (define (withdraw amount)
    (if (> amount bal)
        "inc funds"
        (begin (set! bal (- bal amount))
               bal)))
  (define (deposit amount)
    (set! bal (+ bal amount))
     bal)
  (define (dispatch method amount)
    (cond ((eq? method 'withdraw)
           (withdraw amount))
          ((eq? method 'deposit)
           (deposit amount))
          (else "unknown method")))
  dispatch)