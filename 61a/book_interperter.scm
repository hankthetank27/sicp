#lang sicp

(define true #t)
(define false #f)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval-let exp env))
        ((let*? exp) (eval-let* exp env))
        ((application? exp)
         (apply-x (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression 
                 type: EVAL" exp))))

(define (apply-x procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        ;we will check validity of argument types in extend-environment
                        (extend-environment (procedure-parameters procedure)
                                            arguments
                                            (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

(define (make-procedure params body env)
  (list 'procedure params (scan-out-defines body) env))

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-body proc)
  (caddr proc))

(define (procedure-environment proc)
  (cadddr proc))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values 
            (rest-operands exps) 
            env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) 
                         env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)   ; formal parameters
                 (cddr exp)))) ; body

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (let? exp)
  (tagged-list? exp 'let))
(define (eval-let exp env)
  (eval (let->combination exp) env))
(define (let*? exp)
  (tagged-list? exp 'let*))
(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

(define (let-body exp)
  (cddr exp))
(define (let-vars exp)
  (map car (cadr exp)))
(define (let-bindings exp)
  (map cadr (cadr exp)))
(define (let*-body exp)
  (caddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp)) 
        (let-bindings exp)))

(define (let*->nested-lets exp)
  (define (make-let-nest statements)
    (if (null? statements)
      (let*-body exp)
      (list 'let 
            (list (car statements))
            (make-let-nest (cdr statements)))))
  (make-let-nest (cadr exp)))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false     ; no else clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp 
            (cond-actions first))
          (error "ELSE clause isn't 
                 last: COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (sequence->exp 
                   (cond-actions first))
                 (expand-clauses 
                   rest))))))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

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

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
               (if (eq? (car vals) '*unassigned*)
                 (error "Variable value is undefined --" var)
                 (car vals)))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))


(define (make-let vars exps)
  (cons 'let (cons vars exps)))

(define (scan-out-defines body)
  (define (collect seqs defs exps)
    (cond ((null? seqs) (cons defs exps))
          ((definition? (car seqs))
           (collect (cdr seqs) 
                    (cons (car seqs) defs) 
                    exps))
          (else 
            (collect (cdr seqs) 
                     defs 
                     (cons (car seqs) exps)))))
  (let ((defs-exps (collect body '() '())))
    (if (null? (car defs-exps))
      body
      (list (make-let (map (lambda (def)
                             (list (definition-variable def) ''*unassigned*))
                           (car defs-exps))
                      (append (map (lambda (def)
                                     (list 'set! 
                                           (definition-variable def) 
                                           (definition-value def)))
                                   (car defs-exps))
                              (cdr defs-exps)))))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) 
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'eq? eq?)
        (list 'symbol? symbol?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
    (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment 
  (setup-environment))

(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output 
            (eval input 
                  the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display 
      (list 'compound-procedure
            (procedure-parameters object)
            (procedure-body object)
            '<procedure-env>))
    (display object)))

(driver-loop)
