#lang sicp

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (range a b)
  (if (> a b)
    '()
    (cons a (range (+ 1 a) b))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;1
    ;(delay (+ 1 27)) = (lambda () (+ 1 27))
    ;(force (delay (+ 1 27))) = 28

;2
    ; This produces an error because evaluating (stream-cdr (cons-stream 1 '(2 3))
    ; forces the promise of the inital cons-stream to resolve, returning a list
    ; '(2 3). Since this is then being passed to stream-cdr, stream-cdr tries to force
    ; the value 3, which is not a function and therefore cannot be called as such.

;3 
    ; (delay (enumerate-interval 1 3)) returns a function that when invoked,
    ; returns a list of integers 1-3.
    ;
    ; (stream-enumerate-interval 1 3) returns a stream, or a pair with 1 as its 
    ; car, and as its cdr a function which calls (stream-enumerate-interval (+ low 1) high),
    ; in this intance pointing to an enviroment where low = 1 and high = 3.

;4
(define (num-seq n)
  (let ((next-num (if (= 0 (modulo n 2))
                    (/ n 2)
                    (+ 1 (* 3 n)))))
    (cons-stream n (num-seq next-num))))

(define (seq-len seq)
  (if (= (stream-car seq) 1)
    1
    (+ 1 (seq-len (stream-cdr seq)))))


