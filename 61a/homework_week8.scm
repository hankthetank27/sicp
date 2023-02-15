#lang simply-scheme


;3.1
(define (make-accumulator init-num)
  (lambda (x)
    (set! init-num (+ x init-num))
    init-num))

;(define A (make-accumulator 5))
;(define B (make-accumulator 6))
;(A 10)
;(A 10)
;(B 10)
;(B 10)


;3.2
(define (make-monitored f)
  (let ((counter 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) counter)
            ((eq? x 'reset-counter) (set! counter 0))
            (else (begin (set! counter (+ counter 1))
                         (f x)))))))

;(define s (make-monitored (lambda (x) (* x x))))
;(s 2)
;(s 3)
;(s 'how-many-calls?)
;(s 'reset-counter)
;(s 'how-many-calls?)
;(s 2)
;(s 'how-many-calls?)         

;3.3
#;(define (make-account bal master-pw)
  (define (withdraw amount)
    (set! bal (- bal amount))
    bal)
  (define (deposit amount)
    (set! bal (+ bal amount))
    bal)
  (define (dispatch pw method)
    (cond
      ((not (eq? pw master-pw))
       (lambda _ "incorrect password"))
      ((eq? method 'withdraw) withdraw)
      ((eq? method 'deposit) deposit)))
  dispatch)

;3.4

(define (make-account bal master-pw)
  (define bad-pw-counter
    (make-accumulator 0))
  (define (handle-bad-pw _)
    (let ((count (bad-pw-counter 1)))  
      (if (>= count 7)
          (call-the-cops)
          "incorrect password")))
  (define (call-the-cops)
    "the cops have been called on you mf")
  (define (withdraw amount)
    (set! bal (- bal amount))
    bal)
  (define (deposit amount)
    (set! bal (+ bal amount))
    bal)
  (define (dispatch pw method)
    (cond ((not (eq? pw master-pw)) handle-bad-pw)
          ((eq? method 'bad-pw) handle-bad-pw)
          ((eq? method 'withdraw) withdraw)
          ((eq? method 'deposit) deposit)))
  dispatch)
    


(define a (make-account 100 'asdf))
;((a 'qwer 'withdraw) 12)
;((a 'qwer 'withdraw) 12)
;((a 'qwer 'withdraw) 12)
;((a 'qwer 'withdraw) 12)
;((a 'qwer 'withdraw) 12)
;((a 'qwer 'withdraw) 12)
;((a 'qwer 'withdraw) 12)
;((a 'qwer 'withdraw) 12)

;3.6
(define random-init (random 2))
(define (rand-update x)
  (+ x (random 2)))

(define rand
  (let ((x random-init))
    (define (dispatch method)
      (cond ((eq? method 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? method 'reset)
             (lambda (new)
               (set! x new)))))
    dispatch))

;(rand 'generate)
;(rand 'generate)
;(rand 'generate)
;(rand 'generate)
;(rand 'generate)
;((rand 'reset) 100)
;(rand 'generate)
;(rand 'generate)
;(rand 'generate)
;(rand 'generate)
;(rand 'generate)


;3.7

(define (make-joint account old-pw new-pw)
    (if (number? ((account old-pw 'withdraw) 0))
        (lambda (input-pw method)
          (cond ((eq? input-pw new-pw)
                 (account old-pw method))
                (else
                 (account old-pw 'bad-pw))))
        (error "Parent account password is incorrect")))

(define b (make-joint a 'asdf 'qwer))
(define c (make-joint b 'qwer 'zxcv))
((b 'qwer 'withdraw) 10)
((c 'zxcv 'withdraw) 10)


;3.8

(define f
  (let ((seen-zero #f))
    (lambda (x)
      (cond (seen-zero 0)
            ((= x 0)
             (set! seen-zero #t) x)
            (else x)))))
  


;3.10
;-- drawn in notebook....
; W1 -> [proc-body -> (if (>= ....)), proc-env -> [E2 - balance: 100] -> [E1 - inital-amount: 100] -> Global]


;3.11
; - the local state for 'acc' is kept in the frame/envrioment created when 'make-account' is called.
; - when 'make-account' is called for a second time, a new enviroment is made, with the procedure containted in...
;   'acc2' pointing to it, thereby creating new local variables for that instance.
; - shared enviroment between 'acc' and 'acc2' are primitive procedures, or anything else in the global env.