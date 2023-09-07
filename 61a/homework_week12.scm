#lang sicp

;cs 61a week 12 lab

;reading:
;abelson & sussman, 4.1.1–6
;mapreduce paper in course reader.
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

;1. list all the procedures in the metacircular evaluator that call eval.

;2. list all the procedures in the metacircular evaluator that call apply.

;3. explain why make-procedure does not call eval.

;5. in this lab exercise you will become familiar with the logo programming language, for which you’ll be
;writing an interpreter in project 4.
;(ref pdf)


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
    ; any assignment will be treated as an application. ie. (define x 3) will 
    ; try to be evlauated as (apply 'define (map (lambda (x) (eval x env)) '('x 3)))

    ;b.
    ; we can create a type tag for functions as a list starting with 'call 
    ;eg.
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
           (else (error "unknown expression" exp))))

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
              (error "else clause isn't last" clauses))
            (make-if (cond-predicate first)
                     (handle-true first)
                     (expand-clauses rest))))))

;4.6
    (define (make-lambda parameters body)
      (cons 'lambda (cons parameters body)))

    ;--for eval
    (define (let? exp)
      (tagged-list? exp 'let))
    (define (eval-let exp env)
      (eval (let->combination exp) env))
    ;--
        
    (define (let-body exp)
      (caddr exp))
    (define (let-vars exp)
      (map car (cadr exp)))
    (define (let-bindings exp)
      (map cadr (cadr exp)))
    (define (let->combination exp)
      (cons (make-lambda (let-vars exp) (let-body exp)) 
            (let-bindings exp)))

;4.7*
    ;;~~currying example~~
    (((lambda (x)
       (lambda (y)
         (+ x y)))
      3)
     4) ;x = 3
    ((lambda (y)
      (+ 3 y))
      4) ;return 7
    ;;~~~~~~~~~~~~~~~~~~

    (define let-star
      (let* ((x 3)
             (y (+ x 2))
             (z (+ x y 5)))
        (* x z)))

    (define let-star-nested 
      (let ((x 3))
        (let ((y (+ x 2)))
          (let ((z (+ x y 5)))
            (* x z)))))

    (define let-star-curried
      ((lambda (x)
         ((lambda (y)
            ((lambda (z)
               (* x z))
             (+ x y 5)))
          (+ x 2)))
       3))

    (define (let*->nested-lets exp)
      (define (make-let-nest statements)
        (if (null? statements)
          (let-body exp)
          (list 'let 
                (list (car statements))
                (make-nest (cdr statements)))))
      (make-let-nest (cadr exp)))

;4.11*
    ;;~~~~~ unchanged defs ~~~~~
    (define (enclosing-environment env) (cdr env))
    (define the-empty-environment '())
    (define (first-frame env) (car env))
    (define (extend-environment vars vals base-env)
      (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
          (error "too many arguments supplied" vars vals)
          (error "too few arguments supplied" vars vals))))
    ;;~~~~~~~~~~~~~~~~~~~~~~~~~~

    (define (make-frame variables values)
      (cons 'frame (map cons variables values)))

    (define (add-binding-to-frame! var val frame)
       (set-cdr! frame 
                 (cons (cons var val) (frame-pairs frame))))

    (define (frame-pairs frame)
      (cdr frame))
    
    (define (curr-frame-pair pairs)
      (car pairs))

    (define (rest-frame-pairs pairs)
      (cdr pairs))
    
    (define (scan-pairs var pairs)
      (cond ((null? pairs) #f)
            ((eq? var (car (curr-frame-pair pairs)))
             (curr-frame-pair pairs))
            (else (scan-pairs var (rest-frame-pairs pairs)))))

    (define (lookup-variable-value var env)
      (define (env-loop env)
        (if (eq? the-empty-environment env)
          (error "unbound variable" var)
          (let ((res (scan-pairs var (frame-pairs (first-frame env)))))
            (if res 
              (cdr res)
              (env-loop (enclosing-environment env))))))
      (env-loop env))

    (define (set-variable-value! var val env)
      (define (env-loop env)
        (if (eq? the-empty-environment env)
          (error "unbound variable" var)
          (let ((res (scan-pairs var (frame-pairs (first-frame env)))))
            (if res 
              (set-cdr! res val)
              (env-loop (enclosing-environment env))))))
      (env-loop env))

    (define (define-variable! var val env)
      (let* ((frame (first-frame env))
             (res (scan-pairs var (frame-pairs frame))))
        (if res 
          (set-cdr! res val)
          (add-binding-to-frame! var val frame))))

;4.13
    ;eval cond
    (define (unbind? exp)
      (tagged-list? exp 'unbind))

    ;example list ('unbind 'varname env)
    (define (make-unbound! exp env)
      (unbine-variable! (cadr exp) env)
      'ok)

    ;we will unbind the variable in the current evnrioment if it exists.
    (define (unbine-variable! var env)
      (let* ((frame (first-frame env))
             (vars (frame-variables frame))
             (vals (frame-values frame)))
        (define (scan curr-vars curr-vals prev-vars prev-vals)
          (cond ((null? curr-vars) '())
                ((eq? var (car curr-vars))
                 (begin (set-cdr! prev-vars (cdr curr-vars))
                        (set-cdr! prev-vals (cdr curr-vals))))
                (else 
                  (scan (cdr curr-vars) (cdr curr-vals) curr-vars curr-vals))))
        (if (eq? var (car vars))
          (begin
            (set-car! frame (cdr vars))
            (set-cdr! frame (cdr vals)))
          (scan (cdr vars) (cdr vals) vars vals))))

;4.14
    ;the underlying scheme map does not have any awareness of our scheme implementation.
    ;when we pass the evuated procedure as argument in the form of a list 
    ;('procedure params body env), map errors. It's probably best to avoid using
    ;compound procedures as primitives in our interpreter.


;2*. modify the metacircular evaluator to allow type-checking of arguments to procedures.
;here is how the feature should work. when a new procedure is defined, a formal parameter
;can be either a symbol as usual or else a list of two elements. in this case, the second
;element is a symbol, the name of the formal parameter. the first element is an expression
;whose value is a predicate function that the argument must satisfy. that function should
;return #t if the argument is valid. for example, here is a procedure foo that has type-
;checked parameters num and list:
;> (define (foo (integer? num) ((lambda (x) (not (null? x))) list))
;(list-ref list num))
;foo
;> (foo 3 ’(a b c d e))
;d
;> (foo 3.5 ’(a b c d e))
;error: wrong argument type -- 3.5
;> (foo 2 ’())
;error: wrong argument type -- ()
;in this example we define a procedure foo with two formal parameters, named num and
;list. when foo is invoked, the evaluator will check to see that the first actual argument
;is an integer and that the second actual argument is not empty. the expression whose
;value is the desired predicate function should be evaluated with respect to foo’s defining
;environment. (hint: think about extend-environment.)

    ;calling apply directly
    (define (validate-type-app var val env)
      (if (symbol? var)
        var
        (let ((type-valid? (eval (car var) env))
              (typed-var (cadr var)))
          (if (apply type-valid? (list val))
            typed-var
            (error "Argument type invalid --" typed-var)))))

    ;using cons cell to implicly call apply
    (define (validate-type var val env)
      (if (symbol? var)
        var
        (let ((type-valid? (eval (cons (car var) (list val)) env))
              (typed-var (cadr var)))
          (if type-valid?
            typed-var
            (error "argument type invalid --" typed-var)))))

    (define (extend-environment vars vals base-env)
      ;we will check each variable for valid value types given the user supplied
      ;type checking procedure, in the context of the procedures base environment.
      (let ((vars (map (lambda (var val) 
                         (validate-type var val base-env)) 
                       vars 
                       vals)))
        (if (= (length vars) (length vals))
          (cons (make-frame vars vals) base-env)
          (if (< (length vars) (length vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals)))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;extra for experts:
;abelson & sussman, exercises 4.16 through 4.21



