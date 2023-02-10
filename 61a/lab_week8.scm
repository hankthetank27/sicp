#lang simply-scheme

(define (make-account bal)
  (define (withdraw amount)
    (set! bal (- bal amount))
    bal)
  (define (deposit amount)
    (set! bal (+ bal amount))
    bal)
  (define (dispatch method)
    (cond
      ((eq? method 'withdraw) withdraw)
      ((eq? method 'deposit) deposit)))
  dispatch)

;1/ 2/ 3.
(define (make-account2 init-amount)
  (let ((bal init-amount)
        (transactions '()))
    (define (withdraw amount)
      (set! transactions
            (cons (list 'withdraw amount)
                  transactions))
      (set! bal (- bal amount))
      bal)
    (define (deposit amount)
      (set! transactions
            (cons (list 'deposit amount)
                  transactions))
      (set! bal (+ bal amount))
      bal)
    (define (dispatch method)
      (cond
        ((eq? method 'transactions) transactions) 
        ((eq? method 'balance) bal)
        ((eq? method 'init-balance) init-amount)
        ((eq? method 'withdraw) withdraw)
        ((eq? method 'deposit) deposit)))
    dispatch))

(define b1 (make-account2 100))
((b1 'withdraw) 20)
((b1 'withdraw) 10)
((b1 'deposit) 200)
(b1 'balance)
(b1 'init-balance)
(b1 'transactions)


;4.
(define (plus1 var)
  (set! var (+ var 1))
  var)
(plus1 5)

;; using substitution model...

; ((lambda (var) (set! var (+ var 1)) var) 5)
; ((set! 5 (+ 5 1) 5)
; ((set! 5 (6)) 5)
; ((6) 5)
; 5

;;scheme uses env model and looks up the value of var in the env before returing

(define (make-adder n)
  (lambda (x) (+ x n)))
(make-adder 3)

((make-adder 3) 5) ;8

(define (f x) (make-adder 3))

(f 5) ;proc

(define g (make-adder 3))

(g 5) ;8

(define (make-funny-adder n)
  (lambda (x)
    (if (equal? x 'new)
        (set! n (+ n 1))
        (+ x n))))
(define h (make-funny-adder 3))
(define j (make-funny-adder 7))
(h 5) ;8

(h 5) ;8
(h 'new) 
(h 5) ;9

(j 5) ;12

(let ((a 3))
  (+ 5 a)) ;8

(let ((a 3))
  (lambda (x) (+ x a)))
;proc
((let ((a 3))
   (lambda (x) (+ x a)))
 5) ;8

((lambda (x)
   (let ((a 3))
     (+ x a)))
 5) ;8

'k
(define k
  (let ((a 3))
    (lambda (x) (+ x a))))
(k 5) ;8

'm
(define m
  (lambda (x)
    (let ((a 3))
      (+ x a))))
(m 5) ;8

(define  p
  (let ((a 3))
    (lambda (x)
      (if (equal? x 'new)
          (set! a (+ a 1))
          (+ x a)))))
'p
(p 5); 8
(p 5); 8
(p 'new)
(p 5) ;9

(define r
  (lambda (x)
    (let ((a 3))
      (if (equal? x 'new)
          (set! a (+ a 1))
          (+ x a)))))
'r
(r 5) ;8
(r 5) ;8
(r 'new)
(r 5) ;8, binding of var 'a' happens inside of the context of lambda

's
(define s
  (let ((a 3))
    (lambda (msg)
      (cond ((equal? msg 'new)
             (lambda ()
               (set! a (+ a 1))))
            ((equal? msg 'add)
             (lambda (x) (+ x a)))
            (else (error "huh?"))))))

(s 'add) ;proc
;(s 'add 5) ;error, args do not match call
((s 'add) 5) ;8
(s 'new) ;proc
((s 'add) 5) ;8
((s 'new)) ;a = a + 1 (no val printed)
((s 'add) 5) ; 9

'apply
(define (ask obj msg . args)
  (apply (obj msg) args))

(ask s 'add 5) ; 9
(ask s 'new) ; a = a + 1 (no val printed)
(ask s 'add 5) ; 10
(define x 5)

;the following....
(let ((x 10)
      (f (lambda (y) (+ x y))))
  (f 7))

; is not....
((lambda (x)
   ((lambda (f)
     (f 7))
   (lambda (y) (+ x y))))
 10)

;it is....
((lambda (x f)
   (f 7))
 10
 (lambda (y) (+ x y)))

;x is scoped to the enclosing enviroment (global in this case) because...
;x is defined in the same context as f
  
 