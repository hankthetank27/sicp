#lang sicp

;CS 61A Week 12 Lab

;Reading:
;Abelson & Sussman, 4.1.1–6
;MapReduce paper in course reader.
;(metacircular evaluator: ~cs61a/lib/mceval.scm

;------ lecture interpreter example -------

(define (scheme)
  (display ">")
  (print (eval-x (read) the-global-evniroment))
  (scheme))

(define (eval-x exp env)
  (cond ((self-evaluating? exp) exp)
        ((symbol? exp) (lookup-in-env exp env))
        ((special-form? exp) (do-special-form exp env))
        (else (apply-x (eval-x (car exp) env)
                       (map (lambda (x) (eval-x x env))
                            (cdr exp))))))

(define (apply-x proc args)
  (if (primitive? proc)
    (do-magic proc args)
    (eval-x (body proc)
            (extend-environment (formals proc)
                                args
                                (proc-env proc)))))


;1. List all the procedures in the metacircular evaluator that call eval.

;2. List all the procedures in the metacircular evaluator that call apply.

;3. Explain why make-procedure does not call eval.

;5. In this lab exercise you will become familiar with the Logo programming language, for which you’ll be
;writing an interpreter in project 4.
;(ref PDF)

;4.1
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
    '()
    (let ((first-exp (eval (first-operand exps) env)))
      (let ((rest-exp (list-of-values-lr (rest-operands exps) env)))
        (cons first-exp rest-exp)))))

(define (list-of-values-rl exps env)
  (if (no-operands? exps)
    '()
    (let ((rest-exp (list-of-values-rl (rest-operands exps) env)))
      (let ((first-exp (eval (first-operand exps) env)))
        (cons first-exp rest-exp)))))

;4.2 
    ;a.
    ; since the applcation of procedures is based on the predicate that expression
    ; is a pair, based on the assumption that all other matches have been exausted,
    ; any assignment will be treated as an application. IE. (define x 3) will 
    ; try to be evlauated as (apply 'define (map (lambda (x) (eval x env)) '('x 3)))

    ;b.
    ; we can create a type tag for functions as a list starting with 'call 
    ;EG.
    (define (applcation-with-call? exp) (tagged-list? exp 'call))
    (define (operator-with-call exp) (cadr exp))
    (define (operands-with-call exp) (cddr exp))

;4.3

;4.4

;4.5

;4.6

;4.7*

;4.10*

;4.11*

;4.13

;4.14

;4.15


;2*. Modify the metacircular evaluator to allow type-checking of arguments to procedures.
;Here is how the feature should work. When a new procedure is defined, a formal parameter
;can be either a symbol as usual or else a list of two elements. In this case, the second
;element is a symbol, the name of the formal parameter. The first element is an expression
;whose value is a predicate function that the argument must satisfy. That function should
;return #t if the argument is valid. For example, here is a procedure foo that has type-
;checked parameters num and list:
;> (define (foo (integer? num) ((lambda (x) (not (null? x))) list))
;(list-ref list num))
;FOO
;> (foo 3 ’(a b c d e))
;D
;> (foo 3.5 ’(a b c d e))
;Error: wrong argument type -- 3.5
;> (foo 2 ’())
;Error: wrong argument type -- ()
;In this example we define a procedure foo with two formal parameters, named num and
;list. When foo is invoked, the evaluator will check to see that the first actual argument
;is an integer and that the second actual argument is not empty. The expression whose
;value is the desired predicate function should be evaluated with respect to foo’s defining
;environment. (Hint: Think about extend-environment.)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;Extra for experts:
;Abelson & Sussman, exercises 4.16 through 4.21
