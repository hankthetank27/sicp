#lang sicp

;CS 61A Week 12 Lab

;Reading:
;Abelson & Sussman, 4.1.1–6
;MapReduce paper in course reader.
;(metacircular evaluator: ~cs61a/lib/mceval.scm

;------ lecture interpreter example -------

;(define (scheme)
;  (display ">")
;  (print (eval-x (read) the-global-evniroment))
;  (scheme))
;
;(define (eval-x exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((symbol? exp) (lookup-in-env exp env))
;        ((special-form? exp) (do-special-form exp env))
;        (else (apply-x (eval-x (car exp) env)
;                       (map (lambda (x) (eval-x x env))
;                            (cdr exp))))))
;
;(define (apply-x proc args)
;  (if (primitive? proc)
;    (do-magic proc args)
;    (eval-x (body proc)
;            (extend-environment (formals proc)
;                                args
;                                (proc-env proc)))))
;

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
   (define (eval-dd exp env)
     (cond ((self-evaluating? exp) exp)
           ((variable? exp) (lookup-variable-value exp env))
           ((get 'op (operator exp)) ((get 'op (operator exp)) exp env))
           ((applcation? exp)
            (apply (eval-dd (operator exp) env)
                   (map (lambda (x) (eval-dd x env))
                        (operands exp))))
           (else (error "Unknown expression" exp))))

;4.4
    (define (and? exp) (tagged-list? exp 'and))
    (define (and-clauses exp) (cdr exp))
    (define (eval-and exp env)
      (and-seq (and-clauses exp) env))
    (define (and-seq clauses env)
      (cond ((null? clauses) 'true)
            ((true? (eval (car clauses) env))
             (and-seq (cdr clauses) env))
            (else 'false)))

    (define (or? exp) (tagged-list? exp 'or))
    (define (or-clauses exp) (cdr exp))
    (define (eval-or exp env)
      (or-seq (or-clauses exp) env))
    (define (or-seq clauses env)
      (cond ((null? clauses) 'false)
            ((true? (eval (car clauses) env)) 'true)
            (else (or-seq (cdr clauses) env))))

    ; derived expressions
    (define (eval-and-derived exp env)
      (eval (and->if exp) env))
    (define (and->if exp)
      (expand-and (and-clauses exp)))
    (define (expand-and clauses)
      (if (null? clauses)
        'true
        (make-if (car clauses)
                 (expland-and (cdr clauses))
                 'false)))

    (define (eval-or-derived exp env)
      (eval (or->if exp) env))
    (define (or->if exp)
      (expand-or (or-clauses exp)))
    (define (expand-or clauses)
      (if (null? clauses)
        'false
        (make-if (car clauses)
                 'true
                 (expand-or (cdr clauses)))))
    
;4.5
    
    (define (cond? exp) (tagged-list? exp 'cond))
    (define (cond-clauses exp) (cdr exp))
    (define (cond-else-clause? clause)
      (eq? (cond-predicate clause) 'else))
    (define (cond-predicate clause) (car clause))
    (define (cond-actions clause) (cdr clase))
    (define (cond->if exp)
      (expand-clauses (cond-clauses exp)))

    (define (handle-true clause)
      (let ((actions (cond-actions clause))
            (predicate (cond-predicate clause)))
        (if (eq? '=> (car actions))
          (list (cdr actions) predicate)
          (sequence->exp actions))))

    (define (expand-clauses clauses)
      (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
            (if (null? rest)
              (handle-true first)
              (error "ELSE clause isn't last" clauses))
            (make-if (cond-predicate first)
                     (handle-true first)
                     (expand-clauses rest))))))

;4.6
    ;--for eval
    (define (let? exp)
      (tagged-list? (car exp) 'let))
    (define (eval-let exp env)
      (eval (let->combination exp) env))
    ;--
        
    (define (let-body statements)
      (caddr statements))
    (define (let-vars exp)
      (map car (cadr exp)))
    (define (let-bindings exp)
      (map cdr (cadr exp)))
    (define (let->combination exp)
      (cons (make-lambda (let-vars exp) (let-body exp)) 
            (let-bindings exp)))

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

;; currying example

(((lambda (x)
   (lambda (y)
     (+ x y)))
  3)
 4)

;x = 3

((lambda (y)
  (+ 3 y))
  4)

7
