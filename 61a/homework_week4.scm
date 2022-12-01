#lang simply-scheme

;;1

;2.7
(define (make-interval lower upper)
  (cons lower upper))
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

;2.8
(define (subtract-interval x y)
  (make-interval (- (lower-bound x)(upper-bound y))
                 (- (upper-bound x)(lower-bound y))))

;2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (define (span-zero interval)
    (and (<= (lower-bound interval) 0)
         (>= (upper-bound interval) 0)))
  (if (span-zero y) (error "interval cannot span zero")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(div-interval (make-interval 1 2)
              (make-interval 4 5))


;2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* c p 0.01))
                 (+ c (* c p 0.01))))
(define (percent i)
  (* 100 (/ (- (upper-bound i)(lower-bound i))
            (+ (lower-bound i)(upper-bound i)))))

(define xyz (make-center-percent 3.5 4.0))
(lower-bound xyz)
(upper-bound xyz)
(center xyz)
(width xyz)
(percent xyz)

;2.17
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

(last-pair (list 23 72 149 34))

;2.19
(define (first-denomination coins)
  (car coins))
(define (except-first-denomination coins)
  (cdr coins))
(define (no-more? coins)
  (null? coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc 100 us-coins)


;2.20
(define (same-parity first . nums)
  
  (define (has-parity? first compare)
    (let ((first-even (= 0 (remainder first 2)))
          (compare-even (= 0 (remainder compare 2))))
      (equal? first-even compare-even)))
  
  (define (helper first nums)
    (cond ((null? nums) nums)
          ((has-parity? first (car nums))
           (cons (car nums) (helper first (cdr nums))))
          (else
           (helper first (cdr nums)))))
  
  (cons first (helper first nums)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
        

;2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items)(car items))
            (square-list (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

;2.23
(define (for-each proc items)
  (cond ((null? items))
        (else (proc (car items))
              (for-each proc (cdr items)))))


(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))


;;2.
(define (substitute list old new)
  (cond ((null? list) '())
        ((equal? old (car list))
         (cons new (substitute (cdr list) old new)))
        ((pair? (car list))
         (cons (substitute (car list) old new)
               (substitute (cdr list) old new)))
        (else
         (cons (car list)(substitute (cdr list) old new)))))

(substitute '((lead guitar) (bass guitar) (rhythm guitar) drums)
'guitar 'axe)

;;3.
(define (substitute2 list old new)
  (define (match-word word old new)
    (cond ((null? old) word)
          ((equal? word (car old))
           (car new))
          (else
           (match-word word (cdr old) (cdr new)))))
    
  (cond ((null? list) '())
        ((pair? (car list))
         (cons (substitute2 (car list) old new)
               (substitute2 (cdr list) old new)))
        (else (cons (match-word (car list) old new)
                    (substitute2 (cdr list) old new)))))

(substitute2 '((4 calling birds) (3 french hens) (2 turtle doves))
'(1 2 3 4) '(one two three four))

;;.extra
;2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n) 
   (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f)
   (lambda (x)
     ((a f) ((b f) x)))))

(define (mult a b)
  (lambda (f)
    (lambda (x)
      ((a (b f)) x))))

(define (exp a b)
  (lambda (f)
    (lambda (x)
      (((a b) f) x))))

(define (convert-num cn)
  ((cn (lambda (x) (+ x 1))) 0))
       




   