#lang sicp

(define x (list (list 'a 'b) 'c 'd))
(define y (list 'e 'f))

x
y
(set-car! x y)
x